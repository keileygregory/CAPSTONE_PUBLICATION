#nMDS and SIMPER

library(tidyverse)
library(vegan)
library(knitr)
library(dplyr)
library(ggrepel)
library(ggplot2)

clean_data <- read_csv("data/tidy_data/taxa_tidy.csv")

# Select only species composition data (excluding metadata) *Explicitly Call dplyr::select() because sometimes select() conflicts with functions from other packages (e.g., MASS::select()) so force the correct function by using dplyr::select():
species_data <- clean_data %>%
  dplyr::select(-Location,-day,-"Couldn't find point",-"N/A", -Open_space, -Sediment, -Count_total_points, -Count_valid_points)



species_data <- species_data %>%
  # keep only numeric values
  dplyr::select(where(is.numeric)) %>%
  # remove columns that are all zero (no variation = no use for NMDS)
  dplyr::select(where(~ any(. != 0))) %>%
  # ensure everything is numeric (including ints)
  mutate(across(everything(), as.numeric))

site_data <- clean_data %>%
  dplyr::select(-Count_total_points,-Count_valid_points,-Colonial_ascidians,-Solitary_ascidians,-Tube_building_amphipods,-Anemones,-Arborescent_bryzoans,-Encrusting_bryzoans,-Bivalves,-Barnacles,-Brown_algae,-Coral,-Cyanobacteria,-Crustose_coralline_algae,
-Echinodermata,-Foraminifera,-Green_algae,-Hydroids,-Microalgae,-Open_space,-Oysters,-Hard_tube_building_polychaetes,-Soft_tube_building_polychaetes,-Red_algae,-Sessile_gastropods,-Sediment,-Sponge,-Turf,-"Couldn't find point",-"N/A")







###############################################################################
# Calculate nMDS and save result, stress value, and ordination scores
###############################################################################

# Perform non-metric multidimensional scaling (nMDS) 
nmds_result <- metaMDS(species_data, distance = "euclidean", trymax = 100)

# Check stress value (should be < 0.2 for a good fit)
print(nmds_result$stress) # [1] 0.06711126

# Save stress value (determines if the nMDS solution is reliable)
writeLines(as.character(nmds_result$stress), "analyses_output/NMDSstressvalue.txt")

# Extract NMDS coordinates (ordination scores aka transformed site positions in NMDS space)
site_scores <- as.data.frame(scores(nmds_result, display = "sites"))
site_scores$Site <- clean_data$Location
site_scores$Day <- clean_data$day

species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$Species <- rownames(species_scores)
species_scores$distance <- sqrt(species_scores$NMDS1^2 + species_scores$NMDS2^2)
species_subset <- subset(species_scores, distance > quantile(distance, 0.25))  # top 75%

# Save ordination scores
#write.csv(site_scores, "analyses_output/NMDSsitescores.csv", row.names = TRUE)
#write.csv(species_scores, "analyses_output/NMDSspeciesscores.csv", row.names = TRUE)
#write.csv(species_subset, "analyses_output/NMDSspeciessubsetscores.csv", row.names = TRUE)

###############################################################################
# Visualize nMDS reuslts and *save figure*
###############################################################################

# Color-code by site name and shape-code by habitat type
site_colors <- c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue")

# Create abbreviated species names (first letter of genus + full species name)
species_subset$Species <- c(
  "Sponges",
  "Soft Tube Polychaetes",
  "Bivalves",
  "Arborescent Bryozoans",
  "Encrusting Bryozoans",
  "Tube-building Amphipods",
  "Solitary Ascidians",
  "Colonial Ascidians",
  "Turf",
  "Red algae",
  "Brown algae",
  "Cyanobacteria"
)

# Set exact x and y limits with room for labels and ellipses
x_min <- -1
x_max <- 1.1
y_min <- -0.6
y_max <- 0.6

# CREATE FINAL NMDS IMPROVED PLOT
nmds_plot <- ggplot() +
  # site points colored by site and shaped by habitat type
  geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, color = Site, shape = factor(Day)), position = position_jitter(width = 0.05, height = 0.05),
             size = 3, alpha = 0.9, stroke = 1) +
  # 95% confidence ellipses by habitat
  stat_ellipse(data = site_scores, aes(x = NMDS1, y = NMDS2, color = Site),
               type = "t", level = 0.95, size = 0.8, show.legend = FALSE) +
  # species labels
  geom_label_repel(
    data = species_subset,
    aes(x = NMDS1, y = NMDS2, label = Species),
    color = "deeppink4", fontface = "italic", size = 3.5,
    box.padding = 0.5, 
    segment.size = 0.3, force = 15, alpha = 0.8,
    min.segment.length = 0.1, max.overlaps = 20
  ) +
  # axis limits and labels
  xlim(x_min, x_max) +
  ylim(y_min, y_max) +
  labs(x = "NMDS1", y = "NMDS2") +
  # custom styling
  scale_color_manual(values = site_colors, name = "Site") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95"),
    panel.border = element_rect(size = 0.8),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = c(0.93, 0.3),
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.5),
    plot.title = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 30) # adjust as needed
  )

plot (nmds_plot)

# Save the plot
ggsave("figures/taxa_figures/NMDSplot.png", 
       plot = nmds_plot, width = 8, height = 6, dpi = 300)

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


