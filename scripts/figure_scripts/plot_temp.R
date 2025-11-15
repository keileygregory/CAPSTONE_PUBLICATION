################################################################################
# This script plots water temperature data collected hourly by temp loggers. It is the third script for the water temperature data (1. tidy_temp.R, 2. analyze_temp.R, 3. plot_temp.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on November 14, 2025.
################################################################################

library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)  # for creating multipanel plots

# Load tidy temp logger data
temp_sigletters <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/temp_sigletters.csv")

################################################################################
# Visualize water temperature data
################################################################################
# NOTE: Cannot do point shape by date because hourly data collection. Temp logger measures in 5°C increments, so did not include individual jittered points because looks weird

# Get summary positions for letters (*the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions <- temp_sigletters %>%
  group_by(Location) %>%
  summarise(Temp_C = max(Temp_C, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

# Visualize water temp between locations
boxplot <- ggplot(temp_sigletters, aes(x = Location, y = Temp_C, fill = Location)) + 
  geom_boxplot(aes(fill = Location), alpha = 0.35, outlier.shape = NA, color = "black") + # add black box outline; alpha value here controls transparency of box fill colors
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.6,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = Location), alpha = 0.25, size = 2.5) +  # align points vertically
  geom_text(data = letter_positions, aes(x = Location, y = Temp_C + 0.3, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position sig letters on plot
  scale_fill_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") +
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grand")) +
  labs(
    title = "Distribution of Water Temperature (°C) Across Sampling Locations From January 23, 2025 Through \nApril 21, 2025",
    y = "Water Temperature (°C)",
    x = "Sampling Location",
    caption = "Colored boxes represent water temperature (°C) distributions at each sampling location. Letters (a, b, c) indicate statistically \nsignificant differences in water temperature between sampling locations."
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
boxplot

# Export diversity boxplot
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_boxplot.png", boxplot, width = 8, height = 6, dpi = 600)

# Narrow format
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_boxplot_thin.png", boxplot, width = 8, height = 8, dpi = 600)

################################################################################
# Visualize GAM results for water temperature
################################################################################

# Import saved GAM model for SDI
GAM <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/GAM_results/GAM_model_temp.rds")

# Simple gratia diagnostic plot (faceted by smooth)
draw(GAM, scales = "free")

# Get smooth estimates for custom ggplot
smooths <- smooth_estimates(GAM)

# FIX TIME LABELS FOR PLOT---------------------------------------------
# Recreate temp_gam (same calculation as in analyze_temp.R)
temp_gam <- temp_sigletters %>%
  mutate(
  time_hours = as.numeric(Datetime_UTC - min(Datetime_UTC, na.rm = TRUE)) / 3600,
  Location = as.factor(Location)
 )

# Make a new dataframe with all combinations of datetime + location
newdat <- temp_gam %>%
  dplyr::select(Datetime_UTC, time_hours, Location) %>%
  arrange(Datetime_UTC)

# Add fitted GAM values *and* SE for confidence bands
pred <- predict(
  GAM,
  newdata = newdat,
  type = "response",
  se.fit = TRUE
)

newdat <- newdat %>%
  mutate(
    fitted = pred$fit,
    se     = pred$se.fit,
    ymin   = fitted - 2 * se,   # approx 95% CI
    ymax   = fitted + 2 * se
  )
#-----------------------------------------------------------------------

# Rename site abbreviations to full names in Location col in smooths dataframe for plotting
newdat$Location <- recode(
  newdat$Location,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grand"
)

# Detailed smooth plot
GAM_smoothsplot <- ggplot(newdat, aes(
  x = Datetime_UTC, 
  y = fitted, 
  color = Location
)) +
  geom_ribbon(
    aes(ymin = ymin, ymax = ymax, fill = Location),
    alpha = 0.25,
    color = NA,
    linewidth = 0
  ) +
  geom_line(
    aes(color = Location),
    size = 0.6,
    lineend = "round"
  ) +
  facet_wrap(~ Location, ncol = 3, scales = "free_y") +
  labs(
    x = "Date",
    y = "Water Temperature (°C)"
  ) +
  scale_color_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grand" = "blue" )) +
  scale_fill_manual(values = c( "Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grand" = "blue" )) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "4 weeks") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )
GAM_smoothsplot

# Export detailed GAM smooths plot
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_GAMplot.png", GAM_smoothsplot, width = 8, height = 5, dpi = 500)

# Export wider version
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/temp_GAMplot.png", GAM_smoothsplot_wide, width = 10, height = 5, dpi = 500)