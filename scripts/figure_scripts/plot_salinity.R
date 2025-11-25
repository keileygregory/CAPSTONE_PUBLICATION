library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)  # for creating multipanel plots

# Load salinity data (did not require any tidying)
salinity <- read_csv("~/CAPSTONE_PUBLICATION/data/raw_data/salinity_raw.csv")

# Since we are comparing water quality across sites, we need to make location a factor
salinity <- salinity %>%
  mutate(Location = as.factor(Location))

################################################################################
# Visualize salinity data
################################################################################

# Visualize salinity between locations
boxplot <- ggplot(salinity, aes(x = Location, y = Salinity_PPT)) + 
  geom_boxplot(aes(fill = Location), alpha = 0.2, outlier.shape = NA, color = "black") +
  geom_point(
    aes(color = Location, shape = factor(Date)),
    alpha = 0.75, size = 2.5,
    position = position_jitter(width = 0.4, height = 0)  # **jitter pts bc 6 replicates per each location-date group
  ) +
  scale_fill_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") +
  scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"), guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay", "YHG" = "Yacht Haven Grande")) +
  labs(
    y = "Salinity (PPT)", 
    x = "Sampling Location",
    shape = "Sampling Date"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.95, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )
boxplot

# Export boxplot
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_boxplot.png", boxplot, width = 10, height = 6, dpi = 800)
# Wider
# ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_boxplot_wide.png", boxplot, width = 10, height = 5, dpi = 800)

# ------------------------------

# Visualize salinity between locations over time
# lineplot <- ggplot(salinity, aes(x = Date, y = Salinity_PPT, color = Location, shape = factor(Date), group = Location)
# ) +
#   geom_line(linewidth = 0.5) +
#   geom_point(size = 3, alpha = 1) +
#   scale_color_manual(values = c("BRB" = "lightskyblue", "KRM" = "lightseagreen", "YHG" = "blue"),
#                      labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay", "YHG" = "Yacht Haven Grande")) +
#   theme_minimal() +
#   labs(
#     y = "Salinity (PPT)",
#     x = "Date",
#     color = "Location",
#     shape = "Sampling Date"
#   ) +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 8),   # smaller legend text
#     legend.title = element_text(size = 10), # slightly smaller title
#     legend.key.size = unit(0.95, "lines"),  # smaller legend key boxes
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(color = "gray90")
#   )
# lineplot
# 
# # Export line plot
# ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_lineplot.png", lineplot, width = 8, height = 6, dpi = 800)
# # Export narrow version
# ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_lineplot_thin.png", lineplot, width = 6.25, height = 7.5, dpi = 800)

################################################################################
# Visualize GAM results for Shannon Diversity Index
################################################################################

# Import saved GAM model for SDI
GAMM <- readRDS("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/GAM_results/GAMM_model_salinity.rds")

# Simple gratia diagnostic plot (faceted by smooth)
draw(GAMM, scales = "free")

# Get smooth estimates for custom ggplot
smooths <- smooth_estimates(GAMM)

smooths$Location <- smooths$Location |> as.character()

# Rename site abbreviations to full names in Location col in smooths dataframe for plotting
smooths$Location <- dplyr::recode(
  smooths$Location,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grande"
)

# Detailed smooth plot
GAMM_smoothsplot <- ggplot(smooths, aes(
  x = day, 
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
    y = "Salinity (PPT)"
  ) + 
  scale_x_continuous(breaks = c(15, 30, 45, 60, 75, 90)) +
  scale_color_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grande" = "blue")) + # define line colors
  scale_fill_manual(values = c("Brewers Bay" = "lightskyblue", "Krum Bay" = "lightseagreen", "Yacht Haven Grande" = "blue")) +  # define ribbon colors (match line colors)
  theme_bw() + 
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"  # remove legend
  )
GAMM_smoothsplot

# Export detailed GAMM smooths plot
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_GAMMplot.png", GAMM_smoothsplot, width = 10, height = 6, dpi = 800)
# Narrow
# ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_GAMMplot_thin.png", GAMM_smoothsplot, width = 5.25, height = 7.5, dpi = 800)
# Wide
# ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/salinity_GAMMplot_wide.png", GAMM_smoothsplot, width = 10, height = 5, dpi = 800)
