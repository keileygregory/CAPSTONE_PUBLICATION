library(tidyverse)
library(ggrepel)
library(ggplot2)

# Load saved ordination output files
site_scores <- read_csv("data/analyzed_data/taxa_analyzed/NMDSsitescores.csv")
species_subset <- read_csv("data/analyzed_data/taxa_analyzed/NMDSspeciessubsetscores.csv")

# Update factor levels for site names
site_scores$Site <- recode(
  site_scores$Site,
  "BRB" = "Brewers Bay",
  "KRM" = "Krum Bay",
  "YHG" = "Yacht Haven Grande"
)

# Assign site colors
site_colors <- c(
  "Brewers Bay"        = "lightskyblue",
  "Krum Bay"           = "lightseagreen",
  "Yacht Haven Grande" = "blue"
)

# Assign shapes for sampling days
shape_values <- c(
  "15" = 16,  "30" = 17,  "45" = 15,
  "60" = 3,   "75" = 7,   "90" = 8
)

# Rename species nicely for labeling
pretty_names <- c(
  Arborescent_bryozoans            = "Arborescent bryozoans",
  Barnacles                        = "Barnacles",
  Bivalves                         = "Bivalves",
  Brown_algae                      = "Brown algae",
  Colonial_ascidians               = "Colonial ascidians",
  Crustose_coralline_algae         = "Crustose coralline algae",
  Cyanobacteria                    = "Cyanobacteria",
  Encrusting_bryozoans             = "Encrusting bryozoans",
  Hydroids                         = "Hydroids",
  Red_algae                        = "Red algae",
  `Soft_tube-building_polychaetes` = "Soft tube-building polychaetes",
  Solitary_ascidians               = "Solitary ascidians",
  Sponge                           = "Sponges",
  Turf                             = "Turf"
)

species_subset <- species_subset %>%
  mutate(Species = pretty_names[Species])

# Set padding around plot
x_pad <- 0.3
y_pad <- 0.3

# MAKE NMDS PLOT (taxa names in black)
nmds_plot <- ggplot() +
  # site points
  geom_point(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, color = Site, shape = factor(Day)),
    position = position_jitter(width = 0.05, height = 0.05),
    size = 3, alpha = 0.9, stroke = 1
  ) +
  # 95% ellipses
  stat_ellipse(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, color = Site),
    type = "t", level = 0.95, linewidth = 0.8, show.legend = TRUE
  ) +
  # species labels
  geom_label_repel(
    data = species_subset,
    aes(x = NMDS1, y = NMDS2, label = Species),
    color = "black", fontface = "italic", size = 3.5, # change color to deeppink4 for pink taxa labels
    box.padding = 0.5,
    segment.size = 0.3, force = 15, alpha = 0.75, # alpha controls text box transparency
    min.segment.length = 0.1, max.overlaps = 20
  ) +
  # give extra room but don’t drop anything
  coord_cartesian(
    xlim = c(min(site_scores$NMDS1) - x_pad, max(site_scores$NMDS1) + x_pad),
    ylim = c(min(site_scores$NMDS2) - y_pad, max(site_scores$NMDS2) + y_pad),
    clip = "off"
  ) +
  labs(x = "NMDS1", y = "NMDS2") +
  # shapes = sampling dates
  scale_shape_manual(
    name   = "Sampling Day",
    values = c(
      "15" = 16,
      "30" = 17,
      "45" = 15,
      "60" = 3,
      "75" = 7,
      "90" = 8
    ),
  )+
  # colors = locations
  scale_color_manual(
    values = site_colors,
    name   = "Location"
  ) +
  # put both guides into one legend box
  guides(
    shape = guide_legend(order = 1),
    color = guide_legend(order = 2, override.aes = list(shape = 16, size = 4))
  ) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey95"),
    panel.border       = element_rect(linewidth = 0.8),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.text          = element_text(size = 12),
    legend.title       = element_text(size = 10, face = "bold"),
    legend.text        = element_text(size = 9),   # slightly smaller
    legend.position    = c(0.91, 0.32),
    # single outer box around BOTH sections
    legend.box         = "vertical",
    legend.box.background = element_rect(fill = "white", colour = "grey80", linewidth = 0.5),
    # no separate box around each key
    legend.background  = element_blank(),
    plot.margin        = margin(t = 10, r = 10, b = 10, l = 10)
  )
nmds_plot

# Save plot (black)
ggsave("figures/taxa_figures/NMDSplot.png", plot = nmds_plot, width = 11, height = 6.67, dpi = 800)
