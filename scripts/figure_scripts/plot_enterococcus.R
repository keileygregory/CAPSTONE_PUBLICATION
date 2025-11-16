################################################################################
# This script plots Enterococcus data collected by the EAL. 
# This is script number 3 for the EAL Enterococcus data (1. tidy_water_quality.R., 2. analyze_water_quality.R, 3. plot_enterococcus.R).

# Created by Keiley Gregory on November 14, 2025.
# Last edited by Keiley Gregory on November 14, 2025.
################################################################################

library(tidyverse) # includes ggplot2

# Load tidy temp logger data
enterococcus <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/insignificant_vars_boxplot_data/enterococcus_data.csv")

################################################################################
# SET CUSTOM COLORS
################################################################################

# Set custom colors by site
custom_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grand" = "blue"
)

################################################################################
# BOX PLOT
################################################################################

# Visualize enterococcus by site
boxplot <- ggplot(enterococcus, aes(x = MonitoringLocationName, y = Enterococcus, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymin.., ymax = ..ymax..), width = 0, linewidth = 0.8, color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +
  scale_fill_manual(values = custom_colors) +  # use pre-defined custom colors palette
  scale_color_manual(values = custom_colors) +
  labs(
    x = "Sampling Location",
    y = "Enterococcus (MPN/100mL)"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.caption  = element_text(size = 7,   hjust = 0, color = "grey30"),
    axis.title.x  = element_text(size = 11,  color = "black"),
    axis.title.y  = element_text(size = 11,  color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    legend.position = "none"
  )
print(boxplot)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/enterococcus_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 600)
