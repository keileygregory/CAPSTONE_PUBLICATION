################################################################################
# This script cleans and refines raw biofouling taxon ID data collected through manual analysis of 100 points randomly placed on each photo taken of each tile during each sampling date.
# This is script number 1 (first script) for the biofouling taxon ID data (1. tidy_taxa.R).

# Created by Abigail Lewine on October 28, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(janitor)
library(lubridate)

# Read & clean helper
read_and_clean <- function(file) {
  read_csv(
    file, 
    show_col_types = TRUE,
    col_types = cols(Notes = col_character())   # set 'Notes' column data type to character
   ) %>%
    janitor::clean_names() # make column names consistent (snake_case, no spaces)
}

# Read raw CSVs for each location
brb <- read_and_clean("~/CAPSTONE_PUBLICATION/data/raw_data/taxa_raw/BRB_taxa_raw.csv")
krm <- read_and_clean("~/CAPSTONE_PUBLICATION/data/raw_data/taxa_raw/KRM_taxa_raw.csv")
yhg <- read_and_clean("~/CAPSTONE_PUBLICATION/data/raw_data/taxa_raw/YHG_taxa_raw.csv")

################################################################################

# Combine data for all study sites, fix typo in 'tile_p_hoto' column name, and change 'id' column name to 'taxa_id'
taxa_raw <- bind_rows(brb, krm, yhg) %>%
  rename(
    tile_photo = tile_p_hoto,
    taxa_id = id
    )

# Separate 'location_day' column into a location column and a day column (increases simplicity when handling the time component)
taxa_locationday <- taxa_raw %>%
  separate(
    col = location_day,      
    into = c("location", "day"), 
    sep = "_"               
  )

# Add column with the actual date (yyyy-mm-dd) of each sampling event (instead of only having sampling day intervals (15, 30, etc.) in the 'day' column). Reorder columns by relevance, and reorder rows by sampling date then location.
taxa_date <- taxa_locationday %>%
  mutate(
    date = case_when(
      day == 15 ~ as.Date("2025-02-06"),
      day == 30 ~ as.Date("2025-02-21"),
      day == 45 ~ as.Date("2025-03-08"),
      day == 60 ~ as.Date("2025-03-23"),
      day == 75 ~ as.Date("2025-04-06"),
      day == 90 ~ as.Date("2025-04-22"),
      TRUE ~ NA_Date_
    )
  ) %>%
  select(location, date, day, tile_photo, point, taxa_id, observer, notes) %>%
  arrange(location, day)

# Manually fix mistyped values in 'taxa_id' column
taxa_fix <- taxa_date %>%
  mutate(taxa_id = str_trim(taxa_id)) %>%
  mutate(taxa_id = case_when(
    taxa_id == "CB"                  ~ "Cb",
    taxa_id == "SED"                 ~ "Sed",
    taxa_id == "Browm"               ~ "Brown",
    taxa_id == "O_S"                 ~ "OS",
    taxa_id == "IS"                  ~ "OS",
    taxa_id == "Sd"                  ~ "Sed",
    taxa_id == "AC"                  ~ "A_C",
    taxa_id == "V"                   ~ NA_character_,
    taxa_id == "Couldn't find point" ~ NA_character_,
    taxa_id == "N/A"                 ~ NA_character_,
    TRUE ~ taxa_id
  ))

################################################################################

# Total ALL points per location per date (including NA taxa)
total_all_pts <- taxa_fix %>%
  group_by(location, date, day) %>%
  summarise(n_total_pts = n(), .groups = "drop")

# Total VALID points per location per date (considers how many points are NA for taxa_id)
total_valid_pts <- taxa_fix %>%
  filter(!is.na(taxa_id)) %>%
  group_by(location, date, day) %>%
  summarise(n_valid_pts = n(), .groups = "drop")

# Merge into one dataframe
total_pts <- left_join(total_all_pts, total_valid_pts, by = c("location", "date", "day"))

################################################################################

# Count of taxa occurrences (excluding NA rows)
taxa_counts <- taxa_fix %>%
  filter(!is.na(taxa_id)) %>%
  group_by(location, date, day, taxa_id) %>%
  summarise(n = n(), .groups = "drop")

# Join with total point counts and calculate proportions
taxa_props <- taxa_counts %>%
  left_join(total_pts, by = c("location", "date", "day")) %>%
  mutate(taxa_proportion = n / n_valid_pts)

# Pivot wider so each taxa becomes its own column (<values_fill = 0> tells R that if a particular taxon did not occur at a location/day, it should become 0 proportion instead of NA)
taxa_wide <- taxa_props %>%
  select(location, date, day, taxa_id, taxa_proportion) %>%
  pivot_wider(names_from = taxa_id, values_from = taxa_proportion, values_fill = 0)

# Current taxon ID columns in taxa_wide dataframe (22 total): "A_C", "A_S", "Amp", "B_A", "B_E", "Biv", "Bn", "Brown", "C_H", "Cb", "CCA", "F", "Green", "Hyd", "Micro", "OS", "P_H", "P_S", "Red", "Sed", "Spg", "Turf"
################################################################################

# Create a vector of all expected taxon ID column names (even if absent in raw data) (26 total)
expected_taxa <- c("A_C", "A_S", "Amp", "An", "B_A", "B_E", "Biv", "Bn", "Brown", "C_H", "Cb", "CCA", "Ech", "F", "Green", "Hyd", "Micro", "OS", "Oy", "P_H", "P_S", "Red", "S_G", "Sed", "Spg", "Turf")

# Function that loops through each expected taxon ID column name, checks if the column already exists in the taxa_wide dataframe, and creates a new column for each missing taxon ID and fills the new column with 0 values (since these taxa were absent in raw data, it means that the proportion of points identified as these taxa were 0 for all locations during all sampling events)
for (taxon in expected_taxa) {
  if (!(taxon %in% colnames(taxa_wide))) {
    taxa_wide[[taxon]] <- 0
  }
}

# Arrange columns as location, date, day, taxa proportion columns ordered alphabetically
taxa_ordered <- taxa_wide %>%
  relocate(location, date, day, .before = everything()) %>%
  select(location, date, day, sort(colnames(.)[!(colnames(.) %in% c("location", "date", "day"))]))

# Add total point counts back in
taxa_complete <- left_join(taxa_ordered, total_pts, by = c("location", "date", "day"))

################################################################################

# Create a vector (taxa_name_map) of full taxa names
taxa_name_map <- c(
  location = "Location",
  date = "Sampling_date",
  day = "Sampling_interval_day",
  n_total_pts = "Count_total_points",
  n_valid_pts = "Count_valid_points",
  A_C = "Colonial_ascidians",
  A_S = "Solitary_ascidians",
  Amp = "Tube-building_amphipods",
  An  = "Anemones",
  B_A = "Arborescent_bryozoans",
  B_E = "Encrusting_bryozoans",
  Biv = "Bivalves",
  Bn  = "Barnacles",
  Brown = "Brown_algae",
  C_H = "Coral",
  Cb  = "Cyanobacteria",
  CCA = "Crustose_coralline_algae",
  Ech = "Echinodermata",
  F   = "Foraminifera",
  Green = "Green_algae",
  Hyd = "Hydroids",
  Micro = "Microalgae",
  OS  = "Open_space",
  Oy  = "Oysters",
  P_H = "Hard_tube-building_polychaetes",
  P_S = "Soft_tube-building_polychaetes",
  Red = "Red_algae",
  S_G = "Sessile_gastropods",
  Sed = "Sediment",
  Spg = "Sponge",
  Turf = "Turf"
)

# Make sure columns to be renamed are present and rename (names = NEW, values = OLD)
cols_present <- intersect(names(taxa_complete), names(taxa_name_map))

taxa_renamed <- taxa_complete %>%
  dplyr::rename(!!!setNames(cols_present, taxa_name_map[cols_present]))

# Now the columns are renamed to full names: "Location", "Sampling_date", "Count_total_points", "Count_valid_points", etc.

# Reorder columns using NEW column names
taxa_cols <- setdiff(
  names(taxa_renamed),
  c("Location", "Sampling_date", "Sampling_interval_day", "Count_total_points", "Count_valid_points")
)

taxa_renamedreordered <- taxa_renamed %>%
  dplyr::relocate(Location, Sampling_date, Sampling_interval_day, .before = dplyr::everything()) %>%
  dplyr::select(Location, Sampling_date, Sampling_interval_day, sort(taxa_cols),
                Count_total_points, Count_valid_points)

################################################################################

# Export created dataframe as CSV to tidy data folder
write_csv(taxa_renamedreordered, "~/CAPSTONE_PUBLICATION/data/tidy_data/taxa_tidy.csv")

################################################################################