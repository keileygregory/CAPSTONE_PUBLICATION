library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)  # for creating multipanel plots

################################################################################
# Visualize Taxon Richness calculations
################################################################################

# Import data with richness calculations
richness <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/taxa_richness.csv")

# Visualize Taxon Richness between locations
boxplot <- ggplot(richness, aes(x = Location, y = Taxon_Richness)) + 
  geom_boxplot(aes(fill = Location), alpha = 0.35, outlier.shape = NA, color = "black") + # add black box outline; alpha value here controls transparency of box fill colors
  geom_point(aes(color = Location, shape = factor(Sampling_date)),  # add unique point shapes by sampling date
             alpha = 1, size = 2.5) +  # align points vertically
  scale_fill_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") + # remove Location legend
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") + # remove Location legend
  scale_x_discrete(labels = c(
    "BRB" = "Brewers Bay",
    "KRM" = "Krum Bay",
    "YHG" = "Yacht Haven Grande"
  )) +
  labs(
    y = "Taxon Richness", 
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

# Export richness boxplot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/richness_boxplot.png", boxplot, width = 8, height = 6, dpi = 800)
# Export narrow version for multipanel
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/richness_boxplot_thin.png", boxplot, width = 6, height = 8, dpi = 800)

# Visualize Taxon Richness between locations over time (THIS IS WHAT SHOULD BE IN THE PAPER!)
lineplot <- ggplot(richness, aes(x = Sampling_date, y = Taxon_Richness, color = Location, shape = factor(Sampling_date), group = Location)
) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3, alpha = 1) +
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"),
                     labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay", "YHG" = "Yacht Haven Grande")) +
  theme_minimal() +
  labs(
    y = "Taxon Richness",
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

# Export richness line plot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/richness_lineplot.png", lineplot, width = 8, height = 6, dpi = 800)
# Export narrow version for multipanel
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/richness_lineplot_thin.png", lineplot, width = 7, height = 8, dpi = 800)

################################################################################
# Visualize GAM results for Taxon Richness
################################################################################

# Import saved GAM model for richness
GAM <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_model_richness.rds")

# OPTIONAL: Import saved GAM data for SDI
# gamdf <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_gamdf_richness.rds")

# Simple gratia diagnostic plot (faceted by smooth)
draw(GAM, scales = "free")

# Get smooth estimates for custom ggplot
smooths <- smooth_estimates(GAM)

# Rename site abbreviations to full names in Location col in smooths dataframe for plotting
smooths$Location <- recode(
  smooths$Location,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grande"
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
    y = "Taxon Richness"
  ) + 
  scale_x_continuous(breaks = c(15, 30, 45, 60, 75, 90)) +
  scale_color_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grande" = "blue")) + # define line colors
  scale_fill_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grande" = "blue")) +  # define ribbon colors (match line colors)
  theme_bw() + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"  # remove legend
  )
GAM_smoothsplot

# Export detailed GAM smooths plot
ggsave("~/CAPSTONE_PUBLICATION/figures/taxa_figures/richness_GAMplot.png", GAM_smoothsplot, width = 8, height = 5, dpi = 500)


#-----------------------------------

# NOTE!: If one of the location names was too long to fix into the text box on the plot, you can use \n to set custom line breaks. See example below:

# library(ggtext)  # for wrapped YHG title in GAM smooths plot

# Rename site abbreviations to full names in Location col in smooths dataframe for plotting 
# **Include \n in each site name so that text wraps in plot and YHG full name is not cut off from overflowing text box

#smooths$Location <- recode(
#  smooths$Location,
#  "BRB" = "Brewers\nBay",
#  "KRM" = "Krum\nBay",
#  "YHG" = "Yacht Haven\nGrand"
#)

# In GAM plot code, add \n in everywhere that the location names are written out (to ensure they match the exact string values in the smooths df 'Location' column that were manually set in the code above^)

#GAM_smoothsplot <- ggplot(...
#...
#  scale_color_manual(values = c("Brewers\nBay" = "lightskyblue", "Krum\nBay" = "lightseagreen", "Yacht Haven\nGrand" = "blue")) + # define line colors
#  scale_fill_manual(values = c("Brewers\nBay" = "lightskyblue", "Krum\nBay" = "lightseagreen", "Yacht Haven\nGrand" = "blue")) +  # define ribbon colors (match line colors)
#...



