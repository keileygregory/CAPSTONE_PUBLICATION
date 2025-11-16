################################################################################
# This script cleans the raw water temperature data collected hourly by submerged temp loggers, including clipping the data by date-time to filter for only times when the temp logger was properly submerged (clip to 1 day after deployment date (01-22) and 1 day before last sampling date (04-22) = range 01-23 00:00:00 to 04-21 23:59:59 local time).
# This is script number 1 (first script) for the water temperature data (1. tidy_temp.R).

# Created by Keiley Gregory on September 16, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(lubridate)

# Load raw temp logger data
temp <- read_csv("~/CAPSTONE_PUBLICATION/data/raw_data/temp_raw.csv")

# Remove rows with NA in Temp_C
temp_tidy <- temp %>%
  filter(!is.na(Temp_C))

# Ensure 'Datetime_UTC' column is proper POSIXct format (!! do NOT include 'Datetime_local' column in this or it will incorrectlychange all values to match 'Datetime_UTC' col)
temp_date <- temp_tidy %>%
  mutate(
    Datetime_UTC   = force_tz(Datetime_UTC, tzone = "UTC")
  )

# ** Clip data by date to filter for only times when temp logger was submerged (clip to 1 day after deployment date (01-22) and 1 day before last sampling date (04-22) = range 01-23 00:00:00 to 04-21 23:59:59 local time)
temp_clipped <- temp_date %>%
  filter(
    Datetime_UTC >= ymd_hms("2025-01-23 04:00:00", tz = "UTC"), # equals 2025-01-23 00:00:00 AST (local time)
    Datetime_UTC <= ymd_hms("2025-04-22 03:59:59", tz = "UTC")  # equals 2025-04-21 23:59:59 AST (local time)
  )

################################################################################

# Save cleaned dataframe as a CSV in project directory
write_csv(temp_clipped, "~/CAPSTONE_PUBLICATION/data/tidy_data/temp_tidy.csv")

################################################################################