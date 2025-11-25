library(tidyverse) # includes ggplot2

# Load tidy temp logger data
pH_sigletters <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/pH_sigletters.csv")

# Correct YHG name spelling (using case-when)
pH_sigletters <- pH_sigletters %>%
  mutate(
    MonitoringLocationName = case_when(
      MonitoringLocationName == "Yacht Haven Grand" ~ "Yacht Haven Grande",
      TRUE ~ MonitoringLocationName
    )
  )

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
letter_positions <- pH_sigletters %>%
  group_by(MonitoringLocationName) %>%
  summarise(pH = max(pH, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

################################################################################
# pH BOX PLOT
################################################################################

# Visualize pH concentration by site
boxplot <- ggplot(pH_sigletters, aes(x = MonitoringLocationName, y = pH, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions, aes(x = MonitoringLocationName, y = pH + 0.04, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
  scale_fill_manual(values = custom_colors, guide = "none") +  # use pre-defined custom colors palette
  scale_color_manual(values = custom_colors, guide = "none") +
  scale_x_discrete(labels = c("BRB" = "Brewers Bay", "KRM" = "Krum Bay","YHG" = "Yacht Haven Grande")) +
  labs(
    title = "Distribution of pH Across Sampling Locations From August 2022 Through June 2025",
    x = "Sampling Location",
    y = "pH",
    caption = "Colored boxes represent pH distributions at each sampling location. Letters (a, b) indicate statistically significant differences in pH between sampling locations; \nletter combinations (ab) inidcate intermediate groups that are not significantly different from groups with either letter."
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
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/pH_boxplot.png", plot = boxplot, width = 8, height = 6, dpi = 800)
