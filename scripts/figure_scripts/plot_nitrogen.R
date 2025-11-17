################################################################################
# This script plots nitrogen concentration data collected by the EAL. It is the third script for the EAL nitrogen concentration data (1. tidy_eal_waterquality.R., 2. analyze_eal_waterquality.R, 3. plot_eal_nitrogen.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on September 23, 2025.
################################################################################

library(tidyverse) # includes ggplot2

# Load tidy temp logger data
nitrogen_sigletters <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/nitrogen_sigletters.csv")

#YHG name
nitrogen_sigletters$MonitoringLocationName <-
  recode(nitrogen_sigletters$MonitoringLocationName,
         "Yacht Haven Grand" = "Yacht Haven Grande")


################################################################################
# SET CUSTOM LABELS AND VARIABLES FOR PLOTTING
################################################################################

# Set custom colors by site
custom_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grande" = "blue"
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
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions, aes(x = MonitoringLocationName, y = Nitrogen + 0.03, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
  scale_fill_manual(values = custom_colors, guide = "none") +  # use pre-defined custom colors palette
  scale_color_manual(values = custom_colors, guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grande")) +
  labs(
    title = "Distribution of Nitrogen Concentration (mg/L) Across Sampling Locations From August 2022 \nThrough June 2025",
    x = "Sampling Location",
    y = "Nitrogen Concentration (mg/L)",
    caption = "Colored boxes represent nitrogen concentration (mg/L) distributions at each sampling location. Letters (a, b) indicate statistically significant \ndifferences in nitrogen concentration between sampling locations; letter combinations (ab) inidcate intermediate groups that are not significantly different from groups \nwith either letter."
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
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/nitrogen_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 600)


