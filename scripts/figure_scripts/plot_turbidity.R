library(tidyverse) # includes ggplot2

# Set custom colors by site
custom_colors <- c(
  "Brewers Bay"       = "lightskyblue",
  "Krum Bay"          = "lightseagreen",
  "Yacht Haven Grande" = "blue"
)


################################################################################
# PLOT WITH OUTLIER
################################################################################

# Load data WITH outlier
turbidity_sigletters_outlier <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters_plot_data/turbidity_sigletters_outlier.csv")

# Get summary positions for letters (*the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions_outlier <- turbidity_sigletters_outlier %>%
  group_by(MonitoringLocationName) %>%
  summarise(Turbidity = max(Turbidity, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

# Box plot WITH outlier (turbidity concentration by site)
boxplot_outlier <- ggplot(turbidity_sigletters_outlier, aes(x = MonitoringLocationName, y = Turbidity, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions_outlier, aes(x = MonitoringLocationName, y = Turbidity + 1, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
  scale_fill_manual(values = custom_colors, guide = "none") +  # use pre-defined custom colors palette
  scale_color_manual(values = custom_colors, guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grande")) +
  labs(
    title = "Distribution of Turbidity (NTU) Across Sampling Locations From August 2022 Through June 2025",
    x = "Sampling Location",
    y = "Turbidity (NTU)",
    caption = "Colored boxes represent turbidity (NTU) distributions at each sampling location. Letters (a, b) indicate statistically significant differences \nin turbidity between sampling locations."
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
print(boxplot_outlier)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/turbidity_boxplot_outlier.png", plot = boxplot_outlier, width = 8, height = 6, dpi = 800)


################################################################################
# PLOT WITH*OUT* OUTLIER
################################################################################

# Load data WITH outlier
turbidity_sigletters_clean <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters_plot_data/turbidity_sigletters_clean.csv")

# Get summary positions for letters (*the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions_clean <- turbidity_sigletters_clean %>%
  group_by(MonitoringLocationName) %>%
  summarise(Turbidity = max(Turbidity, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

# Box plot WITHOUT outlier (turbidity concentration by site)
boxplot_clean <- ggplot(turbidity_sigletters_clean, aes(x = MonitoringLocationName, y = Turbidity, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions_clean, aes(x = MonitoringLocationName, y = Turbidity + 0.3, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
  scale_fill_manual(values = custom_colors, guide = "none") +  # use pre-defined custom colors palette
  scale_color_manual(values = custom_colors, guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grande")) +
  labs(
    title = "Distribution of Turbidity (NTU) Across Sampling Locations From August 2022 Through June 2025",
    x = "Sampling Location",
    y = "Turbidity (NTU)",
    caption = "OUTLIER REMOVED. Colored boxes represent turbidity (NTU) distributions at each sampling location. Letters (a, b) indicate statistically significant differences \nin turbidity between sampling locations."
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
print(boxplot_clean)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/turbidity_boxplot_clean.png", plot = boxplot_clean, width = 8, height = 6, dpi = 800)
