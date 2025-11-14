################################################################################
# This script analyzes water temperature data collected hourly by temp loggers. A summary table is created containing the calculated mean and standard error of the mean (SEM) of water temperature for each location across all the entire study period. The presence of statistically significant differences in the measured water temperature values between locations (across all sampling events) is determined using a Kruskal-Wallis test and a Dunn's post hoc test with Bonferroni correction.
# This is script number 2 for the water temperature data (1. tidy_temp.R, 2. analyze_temp.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(rstatix)       # for stat tests
library(multcompView)  # for sig. letters

# Load tidy temp logger data
temp_tidy <- read_csv("~/CAPSTONE_PUBLICATION/data/tidy_data/temp_tidy.csv")

# Since we are comparing water quality across sites, we need to make location a factor
temp_tidy <- temp_tidy %>%
  mutate(Location = as.factor(Location))

################################################################################
# SUMMARY TABLE (MEAN & SEM) BY LOCATION
################################################################################

# Calculate mean and SEM for each location across all dates
summary_location <- temp_tidy %>%
  group_by(Location) %>%
  summarise(
    mean_temp = mean(Temp_C, na.rm = TRUE),
    sem_temp = sd(Temp_C, na.rm = TRUE) / sqrt(n())
  )

# Export summary table as CSV
write_csv(summary_location, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/temp_summarytable_location.csv")

################################################################################
# WATER TEMPERATURE STATISTICS
################################################################################

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
temp_shapiro <- temp_tidy %>%
  group_by(Location) %>%
  shapiro_test(Temp_C)
# all p-values < 0.05 = none of the locations have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST #####

# Kruskal-Wallis test for differences between locations
temp_kruskal <- kruskal_test(Temp_C ~ Location, data = temp_tidy)
# p = 6.69e-183

##### DUNN'S POST HOC TEST #####

# Dunn's post hoc test with Bonferroni correction
temp_dunn <- temp_tidy %>%
  dunn_test(Temp_C ~ Location, p.adjust.method = "bonferroni")
print(temp_dunn)
# BRB x KRM p.adj = 2.63e-177; BRB x YHG p.adj = 3.99e-80; KRM x YHG p.adj = 1.37e-20

# Export Dunn's test results
write_csv(temp_dunn, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/temp_dunnstest.csv")

##### DUNN'S POST HOC SIGNIFICANCE LETTERS #####

# Convert Dunn’s results to a named vector of p-values
temp_pvals <- temp_dunn %>%
  mutate(comparison = paste(group1, group2, sep = "-")) %>%
  select(comparison, p.adj) %>%
  deframe()

# Generate compact letters (BRB "a", KRM "b", YHG "c" = indicates that all sites are significantly different from each other)
temp_letters <- multcompLetters(temp_pvals)$Letters

# Convert to dataframe for joining
temp_dfletters <- tibble(
  Location = names(temp_letters),
  sig_letter = temp_letters
)

# Join significance letters back to the full temp dataframe
temp_sigletters <- temp_tidy %>%
  left_join(temp_dfletters, by = "Location")

# Export full temp dataframe WITH sig. letters as CSV for plotting
write_csv(temp_sigletters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/temp_sigletters.csv")
