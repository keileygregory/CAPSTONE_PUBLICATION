library(lubridate)
library(tidyverse)

# Load master CSV
eal_master <- read_csv("~/CAPSTONE_PUBLICATION/data/raw_data/water_quality_raw.csv")

################################################################################

# OPTIONAL: View all unique values in the 'MonitoringLocationName' column
# unique(eal_master$MonitoringLocationName)

# Create a new data frame that filters for only desired monitoring locations
eal_relevantlocations <- eal_master %>%
  filter(MonitoringLocationIdentifier %in% c("USVIST_WQX-STT-49",
                                             "EPAR2_USVIBAWQMP_WQX-STT-7A",
                                             "EPAR2_USVIBAWQMP_WQX-STT-32B",
                                             "EPAR2_USVIBAWQMP_WQX-STT-33A",
                                             "EPAR2_USVIBAWQMP_WQX-STT-33B",
                                             "EPAR2_USVIBAWQMP_WQX-STT-4"))
 
# Group by fields that define a unique monitoring event (date, time, location, and measured characteristic) so we can handle potential duplicates within each group
eal_grouped <- eal_relevantlocations %>%
  group_by(
    ActivityStartDate,
    `ActivityStartTime/Time`,
    `ActivityStartTime/TimeZoneCode`,
    MonitoringLocationIdentifier,
    MonitoringLocationName,
    `ActivityLocation/LatitudeMeasure`,
    `ActivityLocation/LongitudeMeasure`,
    CharacteristicName
  ) %>%
  mutate(row_id = row_number()) %>%  # *adds 1, 2, 3... to each duplicate group (1st dup row_id = 1, 2nd dup row_id = 2, etc.)
  ungroup()

# Pivot wider to create one row per sampling event and create separate columns for each measured characteristic (columns in <id_cols()> code line remain as column names; unique values in 'CharacteristicName' column become new column names; values in 'ResultMeasureValue' column fill values in new columns for each measured characteristic)
eal_wide <- eal_grouped %>%
  pivot_wider(
    id_cols = c(ActivityStartDate,
                `ActivityStartTime/Time`,
                `ActivityStartTime/TimeZoneCode`,
                MonitoringLocationIdentifier,
                MonitoringLocationName,
                `ActivityLocation/LatitudeMeasure`,
                `ActivityLocation/LongitudeMeasure`,
                row_id
                ),
    names_from = CharacteristicName,
    values_from = ResultMeasureValue
    )
  
# Format date and time as single column in POSIXct format (AST time zone)
eal_datetime <- eal_wide %>%
  mutate(
    ActivityStartDate = as.Date(ActivityStartDate),
    `ActivityStartTime/Time` = hms::as_hms(`ActivityStartTime/Time`),
    DateTime_AST = as.POSIXct(ActivityStartDate) + as.numeric(`ActivityStartTime/Time`) # combine date/time into POSIXct format (return NA if either absent)
  ) %>%
  select(-ActivityStartDate) %>%
  select(-`ActivityStartTime/Time`) %>%
  select(-`ActivityStartTime/TimeZoneCode`)

# Sort row order by descending values of location, then date-time, then duplicate index. Sort column order so that location-related columns are first, followed by the 'DateTime_AST' column, then the 'row_id' column, and then all measured water quality characteristic columns
eal_sorted <- eal_datetime %>%
  arrange(
    MonitoringLocationName,
    MonitoringLocationIdentifier,
    `ActivityLocation/LatitudeMeasure`,
    `ActivityLocation/LongitudeMeasure`,
    DateTime_AST,
    row_id
    ) %>%
  relocate(
    MonitoringLocationName, 
    MonitoringLocationIdentifier,
    `ActivityLocation/LatitudeMeasure`, 
    `ActivityLocation/LongitudeMeasure`,
    .before = 1
    ) %>%
  relocate(
    DateTime_AST,
    .after = `ActivityLocation/LongitudeMeasure`
    )

# Drop unnecessary columns, fix MonitoringLocationName column string values, and rename certain columns (dropped 'Depth' and 'Depth, Secchi disk depth' columns do to irrelevance in this study; dropped 'Salinity' and Temperature, water' columns due to the availability of more relevant and time-appropriate data for both of these characteristics which were directly measured as a part of this study)
eal_refined <- eal_sorted %>%
  select(-`Depth`, -`Depth, Secchi disk depth`, -`Salinity`, -`Temperature, water`) %>%
  mutate(
    MonitoringLocationName = case_when(
      MonitoringLocationName == "BREWER'S BAY" ~ "Brewers Bay",
      MonitoringLocationName == "Long Bay, Northeast Corner" ~ "Yacht Haven Grand",
      MonitoringLocationName == "Long Bay, Off Outfall" ~ "Yacht Haven Grand",
      TRUE ~ MonitoringLocationName
    )
  ) %>%
  rename(
    Latitude = `ActivityLocation/LatitudeMeasure`,
    Longitude = `ActivityLocation/LongitudeMeasure`,
    TSS = `Total suspended solids`,
    DO = `Dissolved oxygen (DO)`
  )

################################################################################

# Save cleaned dataframe as a CSV in project directory
write_csv(eal_refined, "~/CAPSTONE_PUBLICATION/data/tidy_data/water_quality_tidy.csv")

################################################################################