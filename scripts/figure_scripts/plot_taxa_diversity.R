################################################################################
# This script...
# This is script number 3A for the biofouling taxon ID data (1. tidy_taxa.R, 2A. analyze_taxa_diversity.R, 3A. plot_taxa_diversity.R).

# Created by Abigail Lewine on October 28, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)  # for creating multipanel plots

################################################################################
# Visualize Shannon Diversity Index calculations
################################################################################

# Import data with SDI calculations
shannon_diversity <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/taxa_diversity.csv")

# Visualize Shannon Diversity Index between locations
boxplot <- ggplot(shannon_diversity, aes(x = Location, y = Shannon_Diversity_Index)) + 
  geom_boxplot(aes(fill = Location), alpha = 0.35, outlier.shape = NA, color = "black") + # add black box outline; alpha value here controls transparency of box fill colors
  geom_point(aes(color = Location, shape = factor(Sampling_date)),  # add unique point shapes by sampling date
             alpha = 1, size = 2.5) +  # align points vertically
  scale_fill_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "cornflowerblue"), guide = "none") + # <guide = "none"> removes Location legend
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "cornflowerblue"), guide = "none") + # <guide = "none"> removes Location legend
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grand")) +
  labs(
    y = "Shannon Diversity Index", 
    x = "Sampling Location",
    shape = "Sampling Date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",              # keep legend for shapes
    legend.text = element_text(size = 8),   # smaller legend text
    legend.title = element_text(size = 10), # slightly smaller title
    legend.key.size = unit(0.95, "lines"),  # smaller legend key boxes
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
boxplot

# Export diversity boxplot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/diversity_boxplot.png", boxplot, width = 8, height = 6, dpi = 800)

# Visualize Shannon Diversity Index between locations over time (THIS IS WHAT SHOULD BE IN THE PAPER!)
lineplot <- ggplot(shannon_diversity, aes(x = Sampling_date, y = Shannon_Diversity_Index, color = Location, shape = factor(Sampling_date), group = Location)
) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3, alpha = 1) +
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "cornflowerblue"),
                     labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay", "YHG" = "Yacht Haven Grand")) +
  theme_minimal() +
  labs(
    y = "Shannon Diversity Index",
    x = "Date",
    color = "Location",
    shape = "Sampling Date"
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),   # smaller legend text
    legend.title = element_text(size = 10), # slightly smaller title
    legend.key.size = unit(0.95, "lines"),  # smaller legend key boxes
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
lineplot

# Export diversity line plot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/diversity_lineplot.png", lineplot, width = 8, height = 6, dpi = 800)

################################################################################
# Visualize GAM results for Shannon Diversity Index
################################################################################

# Import saved GAM model for SDI
GAM <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_model_diversity.rds")

# OPTIONAL: Import saved GAM data for SDI
# gamdf <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_gamdf_diversity.rds")

# Simple gratia diagnostic plot (faceted by smooth)
draw(GAM, scales = "free")

# Get smooth estimates for custom ggplot
smooths <- smooth_estimates(GAM)

# Rename site abbreviations to full names in Location col in smooths dataframe for plotting
smooths$Location <- recode(
  smooths$Location,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grand"
)

# Detailed smooth plot
GAM_smoothsplot <- ggplot(smooths, aes(
  x = Sampling_interval_day, 
  y = .estimate, 
  ymin = .estimate - .se, 
  ymax = .estimate + .se, 
  color = Location,  # set line custom colors by site
  fill = Location    # set ribbon custom colors by site
)
) +
  geom_ribbon(alpha = 0.15) +   # transparent ribbons
  geom_line(size = 1.5, lineend = "round") +  # solid colored lines
  facet_wrap(~ Location, ncol = 3) + 
  labs(
    x = "Sampling Interval Day", 
    y = "Shannon Diversity Index"
  ) + 
  scale_x_continuous(breaks = c(15, 30, 45, 60, 75, 90)) +
  scale_color_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grand" = "cornflowerblue")) + # define line colors
  scale_fill_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grand" = "cornflowerblue")) +  # define ribbon colors (match line colors)
  theme_bw() + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"  # remove legend
  )
GAM_smoothsplot

# Export detailed GAM smooths plot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/diversity_GAMplot.png", GAM_smoothsplot, width = 8, height = 5, dpi = 500)
