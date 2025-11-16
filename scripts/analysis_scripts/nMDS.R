#nMDS and SIMPER

library(tidyverse)
library(vegan)
library(knitr)
library(dplyr)
library(ggrepel)
library(ggplot2)

clean_data <- read_csv("data/tidy_data/taxa_tidy.csv")

# Select only species composition data (excluding metadata). *Explicitly call dplyr::select() because sometimes select() conflicts with functions from other packages (e.g., MASS::select()) so force the correct function by using dplyr::select():
species_data <- clean_data %>%
  dplyr::select(-Location, -Sampling_date, -Sampling_interval_day, -Open_space, -Sediment, -Count_total_points, -Count_valid_points)

species_data <- species_data %>%
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
print(nmds_result$stress) # [1] 0.06984391 ⚠️ stress value changed from what you had ([1] 0.06711126)???

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
# Visualize nMDS reuslts and *save figure*
###############################################################################

# Create abbreviated species names (first letter of genus + full species name)
pretty_names <- c(
  Arborescent_bryozoans           = "Arborescent bryozoans",
  Barnacles                       = "Barnacles",
  Bivalves                        = "Bivalves",
  Brown_algae                     = "Brown algae",
  Colonial_ascidians              = "Colonial ascidians",
  Crustose_coralline_algae        = "Crustose coralline algae",
  Cyanobacteria                   = "Cyanobacteria",
  Encrusting_bryozoans            = "Encrusting bryozoans",
  Hydroids                        = "Hydroids",
  Red_algae                       = "Red algae",
  `Soft_tube-building_polychaetes` = "Soft tube-building polychaetes",
  Solitary_ascidians              = "Solitary ascidians",
  Sponge                          = "Sponges",
  Turf                            = "Turf"
)

species_subset <- species_subset %>%
  mutate(Species = pretty_names[Species])

# Set exact x and y limits with room for labels and ellipses
x_min <- -1
x_max <- 1.1
y_min <- -0.6
y_max <- 0.6

# Shapes for each sampling interval (Day)
shape_values <- c(
  "15" = 16,  # filled circle
  "30" = 17,  # filled triangle
  "45" = 15,  # filled square
  "60" = 3,   # plus
  "75" = 7,   # square with X
  "90" = 8    # star
)

# Labels that should appear in the legend for each Day
date_labels <- c(
  "15" = "2025-02-06",
  "30" = "2025-02-21",
  "45" = "2025-03-08",
  "60" = "2025-03-23",
  "75" = "2025-04-06",
  "90" = "2025-04-22"
)

# Recode site labels and colors for the legend
site_scores$Site <- recode(
  site_scores$Site,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grand"
)

site_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grand" = "blue"
)

# Shapes and labels (as you already defined)
shape_values <- c(
  "15" = 16,
  "30" = 17,
  "45" = 15,
  "60" = 3,
  "75" = 7,
  "90" = 8
)

date_labels <- c(
  "15" = "2025-02-06",
  "30" = "2025-02-21",
  "45" = "2025-03-08",
  "60" = "2025-03-23",
  "75" = "2025-04-06",
  "90" = "2025-04-22"
)

# Padding for ellipses + labels
x_pad <- 0.3   # bigger padding than before
y_pad <- 0.3

# MAKE NMDS PLOT
nmds_plot <- ggplot() +
  # site points
  geom_point(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, color = Site, shape = factor(Day)),
    position = position_jitter(width = 0.05, height = 0.05),
    size = 3, alpha = 0.9, stroke = 1
  ) +
  # 95% ellipses
  stat_ellipse(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, color = Site),
    type = "t", level = 0.95, linewidth = 0.8, show.legend = TRUE
  ) +
  # species labels
  geom_label_repel(
    data = species_subset,
    aes(x = NMDS1, y = NMDS2, label = Species),
    color = "black", fontface = "italic", size = 3.5,
    box.padding = 0.5,
    segment.size = 0.3, force = 15, alpha = 0.8,
    min.segment.length = 0.1, max.overlaps = 20
  ) +
  # give extra room but don’t drop anything
  coord_cartesian(
    xlim = c(min(site_scores$NMDS1) - x_pad, max(site_scores$NMDS1) + x_pad),
    ylim = c(min(site_scores$NMDS2) - y_pad, max(site_scores$NMDS2) + y_pad),
    clip = "off"
  ) +
  labs(x = "NMDS1", y = "NMDS2") +
  # shapes = sampling dates
  scale_shape_manual(
    name   = "Sampling Date",
    values = c(
      "15" = 16,
      "30" = 17,
      "45" = 15,
      "60" = 3,
      "75" = 7,
      "90" = 8
    ),
    breaks = c("15","30","45","60","75","90"),
    labels = c(
      "15" = "2025-02-06",
      "30" = "2025-02-21",
      "45" = "2025-03-08",
      "60" = "2025-03-23",
      "75" = "2025-04-06",
      "90" = "2025-04-22"
    )
  ) +
  # colors = locations
  scale_color_manual(
    values = site_colors,
    name   = "Location"
  ) +
  # put both guides into one legend box
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2, override.aes = list(shape = 16, size = 4))
  ) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey95"),
    panel.border       = element_rect(linewidth = 0.8),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.text          = element_text(size = 12),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 9),   # slightly smaller
    legend.position    = c(0.93, 0.3),
    # single outer box around BOTH sections
    legend.box         = "vertical",
    legend.box.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.5),
    # no separate box around each key
    legend.background  = element_blank(),
    plot.margin        = margin(t = 10, r = 30, b = 10, l = 30)
  )

nmds_plot

# Save the plot
ggsave("figures/taxa_figures/NMDSplot.png", 
       plot = nmds_plot, width = 10, height = 6.67, dpi = 800)
# change color to deeppink4 for pink taxa labels

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


