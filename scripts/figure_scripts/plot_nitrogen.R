################################################################################
# This script plots nitrogen concentration data collected by the EAL. It is the third script for the EAL nitrogen concentration data (1. tidy_eal_waterquality.R., 2. analyze_eal_waterquality.R, 3. plot_eal_nitrogen.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on September 23, 2025.
################################################################################

library(tidyverse) # includes ggplot2

# Load tidy temp logger data
nitrogen_sigletters <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed_data/significance_letters/eal_nitrogen_sigletters.csv")
spec(nitrogen_sigletters)

################################################################################

# SET CUSTOM LABELS AND VARIABLES FOR PLOTTING

################################################################################

# Set custom colors by site
custom_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grand" = "blue"
)

# Get summary positions for letters (*the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions <- nitrogen_sigletters %>%
  group_by(MonitoringLocationName) %>%
  summarise(Nitrogen = max(Nitrogen, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

################################################################################

# NITROGEN BOX PLOT

################################################################################

# Visualize nitrogen concentration by site
boxplot <- ggplot(nitrogen_sigletters, aes(x = MonitoringLocationName, y = Nitrogen, fill = MonitoringLocationName)) +
  geom_boxplot(
    width = 0.6, alpha = 0.70,
    outlier.size = 1.2, outlier.stroke = 0.2, outlier.shape = 21, outlier.color = "grey30", outlier.fill = "black") +
  geom_text(data = letter_positions,
            aes(x = MonitoringLocationName, y = Nitrogen + 0.05, label = sig_letter),
            inherit.aes = FALSE, size = 3, fontface = "bold") +  
  scale_fill_manual(values = custom_colors, guide = "none") +
  labs(
    title = "Distribution of Nitrogen Concentration (mg/L) Across Sampling Locations From August 2022 \nThrough June 2025",
    x = "Sampling Location",
    y = "Nitrogen Concentration (mg/L)",
    caption = "Colored boxes represent nitrogen concentration (mg/L) distributions at each sampling location. Black points mark outliers. Letters (a, b) indicate statistically significant \ndifferences in nitrogen concentration between sampling locations; letter combinations (ab) inidcate intermediate groups that are not significantly different from groups \nwith either letter.") +
  theme_minimal(base_size = 10) +  
  theme(
    axis.text.x   = element_text(vjust = 1.2, color = "black", face = "bold"),
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.caption  = element_text(size = 7,   hjust = 0, color = "grey30"),
    axis.title.y  = element_text(size = 10,  color = "black"),
    legend.position = "none")

print(boxplot)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/eal_nitrogen_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 300)
