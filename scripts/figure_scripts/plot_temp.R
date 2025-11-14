################################################################################
# This script plots water temperature data collected hourly by temp loggers. It is the third script for the water temperature data (1. tidy_temp.R, 2. analyze_temp.R, 3. plot_temp.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on September 23, 2025.
################################################################################

library(tidyverse) # includes ggplot2

# Load tidy temp logger data
temp_sigletters <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed_data/significance_letters/temp_sigletters.csv")
spec(temp_sigletters)

################################################################################

# SET CUSTOM LABELS AND VARIABLES FOR PLOTTING

################################################################################

# Set custom site labels
custom_names <- c(
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grand")

# Apply recoding for site names
temp_plotting <- temp_sigletters %>%
  mutate(Location = recode(Location, !!!custom_names))

# Set custom colors by site
custom_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grand" = "blue"
)

# Get summary positions for letters (*the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions <- temp_plotting %>%
  group_by(Location) %>%
  summarise(Temp_C = max(Temp_C, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

################################################################################

# TEMP BOX PLOT 
# NOTE: Cannot do point shape by date because hourly data collection. Temp logger measures in 5°C increments, so did not include individual jittered points because looks weird

################################################################################

# Visualize temp by site
boxplot <- ggplot(temp_plotting, aes(x = Location, y = Temp_C, fill = Location)) +
  geom_boxplot(
    width = 0.6, alpha = 0.70,
    outlier.size = 1.2, outlier.stroke = 0.2, outlier.shape = 21, outlier.color = "grey30", outlier.fill = "black") +
  geom_text(data = letter_positions,
            aes(x = Location, y = Temp_C + 0.5, label = sig_letter),
            inherit.aes = FALSE, size = 3, fontface = "bold") +  
  scale_fill_manual(values = custom_colors, guide = "none") +
  labs(
    title = "Distribution of Water Temperature (°C) Across Sampling Locations From January 23, 2025 Through \nApril 21, 2025",
    x = "Sampling Location",
    y = "Water Temperature (°C)",
    caption = "Colored boxes represent water temperature (°C) distributions at each sampling location. Black points mark outliers. Letters (a, b, c) indicate statistically \nsignificant differences in water temperature between sampling locations.") +
  theme_minimal(base_size = 10) +  
  theme(
    axis.text.x   = element_text(vjust = 1.2, color = "black", face = "bold"),
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.caption  = element_text(size = 7,   hjust = 0, color = "grey30"),
    axis.title.y  = element_text(size = 10,  color = "black"),
    legend.position = "none")

print(boxplot)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 300)

################################################################################

# TEMP OVER TIME PLOT SCATTERPLOT WITH TRENDLINES, COLOR = LOCATION
# NOTE: Temp logger measures in 5°C increments, so plot will display as such

################################################################################

# Visualize temp over time by site
scatterplot <- ggplot(temp_plotting, aes(x = Datetime_local, y = Temp_C)) +
  geom_point(aes(fill = Location), shape = 21, color = "black", size = 2, stroke = 0.2, alpha = 0.5) +  # semi-opaque points with outline
  geom_smooth(aes(color = Location), method = "loess", se = FALSE, linewidth = 0.5) +  # colored trend lines
  labs(
    title = "Water Temperature (°C) Over Time Across Sampling Locations From January 23, 2025 \nThrough April 21, 2025",
    x = "Date",
    y = "Water Temperature (°C)",
    fill = "Location",
    color = "Location",
    caption = "Points represent water temperature (°C) values colored by sampling location. Lines represent trendlines colored by sampling location.") +
  theme_minimal(base_size = 10) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  theme(
    axis.text.x   = element_text(vjust = 1.2, color = "black"),
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.caption  = element_text(size = 7,   hjust = 0, color = "grey30"),
    axis.title.y  = element_text(size = 10,  color = "black"),
    legend.text   = element_text(face = "bold"))  

print(scatterplot)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_over_time_scatterplot.png", plot = scatterplot, width = 8, height = 6, dpi = 300)
