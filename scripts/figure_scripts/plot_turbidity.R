################################################################################
# This script plots turbidity data collected by the EAL. It is the third script for the EAL turbidity data (1. tidy_eal_waterquality.R., 2. analyze_eal_waterquality.R, 3. plot_eal_turbidity.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on September 23, 2025.
################################################################################

library(tidyverse) # includes ggplot2

# Load tidy temp logger data
turbidity_sigletters_outlier <- read_csv("~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/turbidity_sigletters_outlier.csv")

#YHG name
turbidity_sigletters_outlier$MonitoringLocationName <-
  recode(turbidity_sigletters_outlier$MonitoringLocationName,
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
letter_positions_outlier <- turbidity_sigletters_outlier %>%
  group_by(MonitoringLocationName) %>%
  summarise(Turbidity = max(Turbidity, na.rm = TRUE),
            sig_letter = first(sig_letter)) %>%
  ungroup()

################################################################################
# TURBIDITY BOX PLOT ** with ** OUTLIER
################################################################################

# Visualize turbidity concentration by site
boxplot_outlier <- ggplot(turbidity_sigletters_outlier, aes(x = MonitoringLocationName, y = Turbidity, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions_outlier, aes(x = MonitoringLocationName, y = Turbidity + 0.07, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
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
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/turbidity_boxplot_outlier.png", plot = boxplot_outlier, width = 8, height = 6, dpi = 600)

################################################################################
# ⚠️❗️⚠️❗️⚠️❗️⚠️❗️️ REMOVE OBVIOUS OUTLIER IN BREWERS BAY ⚠️❗️⚠️❗️⚠️❗️⚠️❗️
################################################################################

# row MonitoringLocationName  MonitoringLocationIdentifier  Latitude  Longitude   DateTime_AST          Turbidity
# 7   Brewers Bay             USVIST_WQX-STT-49             18.34509  -64.97880   2022-09-06 14:12:00   15.30

# Remove only the one row with clear outlier (turbidty value = 15.30)
turbidity_sigletters_CLEAN <- turbidity_sigletters_outlier %>%
  filter(!(MonitoringLocationName == "Brewers Bay" & Turbidity == 15.30))

# -------------------------------------------------------------------------------
# RE-CALCULATE MEAN AND SEM VALUES FOR BRB --------------------------------------
SEM_function <- function(x, na.rm = TRUE) {
  n <- sum(!is.na(x))
  if (n == 0) {return(NA_real_)}
  sd(x, na.rm = na.rm) / sqrt(n)}

mean_function <- function(x) {
  if (all(is.na(x))) {NA_real_} 
  else {mean(x, na.rm = TRUE)}}

numeric_cols <- c("Turbidity")

# Group by location and compute mean and SEM of all variables for each location across all years
mean_CLEAN <- turbidity_sigletters_CLEAN %>%
  group_by(MonitoringLocationName) %>%
  summarise(
    across(all_of(numeric_cols), mean_function, .names = "mean_{.col}"),
    across(all_of(numeric_cols), SEM_function, .names = "SEM_{.col}"),
    .groups = "drop"
  )

# Save summary table with CLEAN mean and SEM for BRB (other sites unchanged)
write_csv(mean_CLEAN, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/turbidity_summarytable_CLEAN.csv")

# -------------------------------------------------------------------------------

# *RECALCULATE summary positions for letters now that outlier has been removed (the numbers in this df are the MAX values for plotting display purposes, NOT the mean)
letter_positions_CLEAN <- turbidity_sigletters_CLEAN %>%
  group_by(MonitoringLocationName) %>%
  summarise(
    Turbidity = max(Turbidity, na.rm = TRUE),
    sig_letter = first(sig_letter)
  ) %>%
  ungroup()

# Export full turbidity dataframe EXCLUDING OUTLIER with sig. letters as CSV for plotting
write_csv(turbidity_sigletters_CLEAN, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/turbidity_sigletters_CLEAN.csv")

# -------------------------------------------------------------------------------
# RE-RUN STAT TESTS (MAKE SURE SIG DIFFERENCES SAME) ----------------------------
library(car)           # for Levene's Test
library(rstatix)       # for various stat tests
library(multcompView)  # for significance letters

turbidity_shapiro_CLEAN <- turbidity_sigletters_CLEAN %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(Turbidity)
# all p-values < 0.05 = none of the locations have normally distributed data = ANOVA assumption of normality is violated

turbidity_kruskal_CLEAN <- kruskal_test(Turbidity ~ MonitoringLocationName, data = turbidity_sigletters_CLEAN)
# p = 1.54e-13 (was 7.84e-13) = still significant

# n     statistic   df  p
# 145   59.0051     2   1.54e-13

turbidity_dunn_CLEAN <- turbidity_sigletters_CLEAN %>%
  dunn_test(Turbidity ~ MonitoringLocationName, p.adjust.method = "bonferroni")
# BRB x YHG p.adj = 3.45e-12 (was 2.08e-11); KRM x YHG p.adj = 8.21e-07 (was 9.90e-07)

# Export Dunn's test results
write_csv(turbidity_dunn_CLEAN, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/turbidity_dunnstest_CLEAN.csv")

turbidity_pvals_CLEAN <- turbidity_dunn_CLEAN %>%
  mutate(comparison = paste(group1, group2, sep = "-")) %>%
  select(comparison, p.adj) %>%
  deframe()

turbidity_letters_CLEAN <- multcompLetters(turbidity_pvals_CLEAN)$Letters
print(turbidity_letters_CLEAN)
# *Sig letters remain exact same as before outlier removal (BRB and KRM "a", YHG "b" = indicates that BRB and KRM are statistically similar to each other, but both are significantly different from YHG)
# -------------------------------------------------------------------------------

################################################################################
# RE-PLOT TURBIDITY BOX PLOT *** EXCLUDING *** OUTLIER
################################################################################

# Visualize turbidity concentration by site
boxplot_CLEAN <- ggplot(turbidity_sigletters_CLEAN, aes(x = MonitoringLocationName, y = Turbidity, fill = MonitoringLocationName)) + 
  geom_boxplot(aes(fill = MonitoringLocationName), alpha = 0.35, outlier.shape = NA, color = "black") +  # add black box outline; alpha value here controls transparency of box fill color
  stat_boxplot(geom = "errorbar",  # **add extra layer that has ONLY whisker lines (so can make them ticker without effecting entire box border)
               aes(ymin = ..ymin.., ymax = ..ymax..),   # use whisker endpoints
               width = 0,   # no horizontal caps, just vertical line
               linewidth = 0.8,  # thickness of whisker lines
               color = "black") +
  geom_point(aes(color = MonitoringLocationName), alpha = 0.5, size = 2.75) +  # align points vertically
  geom_text(data = letter_positions_CLEAN, aes(x = MonitoringLocationName, y = Turbidity + 0.3, label = sig_letter), inherit.aes = FALSE, size = 4, fontface = "bold") +  # position significance letters on plot
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
print(boxplot_CLEAN)

# Export plot as PNG
ggsave("~/CAPSTONE_PUBLICATION/figures/driver_figures/turbidity_boxplot_CLEAN.png", plot = boxplot_CLEAN, width = 8, height = 6, dpi = 600)

