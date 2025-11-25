library(tidyverse)
library(vegan)
library(knitr)
library(dplyr)
library(ggrepel)
library(ggplot2)

clean_data <- read_csv("data/tidy_data/taxa_tidy.csv")

# Select only species composition data (excluding metadata). *Explicitly call dplyr::select() because sometimes select() conflicts with functions from other packages (e.g., MASS::select()) so force the correct function by using dplyr::select():
species_data <- clean_data %>%
  dplyr::select(-Location, -Sampling_date, -Sampling_interval_day, -Open_space, -Sediment, -Count_total_points, -Count_valid_points) %>%
  dplyr::select(where(is.numeric)) %>%  # keep only numeric values
  dplyr::select(where(~ any(. != 0))) %>%  # remove columns that are all zero (no variation = no use for NMDS)
  mutate(across(everything(), as.numeric))  # ensure everything is numeric (including ints)

# OPTIONAL: Print list of all col names to make sure they match in next line of code
clean_data %>% names()

# Select only location/sampling date data (*fun fact: the select() function interprets hyphens as a subtraction operator unless they're in quotations)
site_data <- clean_data %>%
  dplyr::select(
    -Count_total_points,
    -Count_valid_points,
    -Colonial_ascidians,
    -Solitary_ascidians,
    -"Tube-building_amphipods",
    -Anemones,
    -Arborescent_bryozoans,
    -Encrusting_bryozoans,
    -Bivalves,
    -Barnacles,
    -Brown_algae,
    -Coral,
    -Cyanobacteria,
    -Crustose_coralline_algae,
    -Echinodermata,
    -Foraminifera,
    -Green_algae,
    -Hydroids,
    -Microalgae,
    -Open_space,
    -Oysters,
    -"Hard_tube-building_polychaetes",
    -"Soft_tube-building_polychaetes",
    -Red_algae,
    -Sessile_gastropods,
    -Sediment,
    -Sponge,
    -Turf
  )

###############################################################################
# Calculate nMDS and save result, stress value, and ordination scores
###############################################################################

# Perform non-metric multidimensional scaling (nMDS) 
nmds_result <- metaMDS(species_data, distance = "euclidean", trymax = 100)

# Check stress value (should be < 0.2 for a good fit)
print(nmds_result$stress) # [1] 0.06984391

# Save stress value (determines if the nMDS solution is reliable)
writeLines(as.character(nmds_result$stress), "data/analyzed_data/taxa_analyzed/NMDSstressvalue.txt")

# Extract NMDS coordinates (ordination scores aka transformed site positions in NMDS space)
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$Site <- clean_data$Location
site_scores$Day <- clean_data$Sampling_interval_day

species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$Species <- rownames(species_scores)
species_scores$distance <- sqrt(species_scores$NMDS1^2 + species_scores$NMDS2^2)
species_subset <- subset(species_scores, distance > quantile(distance, 0.25))  # top 75%

# Save ordination scores
write.csv(site_scores, "data/analyzed_data/taxa_analyzed/NMDSsitescores.csv", row.names = TRUE)
write.csv(species_scores, "data/analyzed_data/taxa_analyzed/NMDSspeciesscores.csv", row.names = TRUE)
write.csv(species_subset, "data/analyzed_data/taxa_analyzed/NMDSspeciessubsetscores.csv", row.names = TRUE)

###############################################################################
# SIMPER Post Hoc Analysis
###############################################################################

# Perform SIMPER analysis- explains which species are actually driving the variation
taxon_simper <- simper(species_data, group = clean_data$Location)
taxon_simper_summary <-summary(taxon_simper)

# Create a list to store cleaned-up data frames for each pairwise contrast
simper_list <- list()

# Loop through each contrast and convert to data frame
for (contrast in names(taxon_simper_summary)) {
  simper_df <- as.data.frame(taxon_simper_summary[[contrast]])
  simper_df$Contrast <- contrast  # Add a column to indicate which comparison this is
  simper_df$Species <- rownames(simper_df)  # Keep species names
  simper_list[[contrast]] <- simper_df
}

# Combine all contrasts into a single data frame
final_simper_output <- do.call(rbind, simper_list)
