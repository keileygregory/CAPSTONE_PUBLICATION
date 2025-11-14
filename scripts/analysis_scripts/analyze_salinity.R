################################################################################
# This script analyzes salinity data collected during each sampling event. Summary tables are created containing the calculated mean and standard error of the mean (SEM) of salinity for each location-sampling event group and for each location across all sampling events. The absence of statistically significant differences in the measured salinity values between locations (across all sampling events) is determined using a Kruskal-Wallis test.
# This is script number 1 (first script) for the salinity data (1. analyze_salinity.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(rstatix)   # for stat tests

# Load salinity data (did not require any tidying)
salinity <- read_csv("~/CAPSTONE_PUBLICATION/data/raw_data/salinity_raw.csv")

# Since we are comparing water quality across sites, we need to make location a factor
salinity <- salinity %>%
  mutate(Location = as.factor(Location))


################################################################################
################################################################################
# SUMMARY TABLE (MEAN & SEM) BY LOCATION-SAMPLING EVENT DATE AND LOCATION ACROSS ALL DATES
################################################################################
################################################################################

# SUMMARY TABLE BY EACH LOCATION-SAMPLING EVENT GROUP

# Calculate mean and SEM for each location during each sampling event date
summary_datelocation <- salinity %>%
  group_by(Location, Date) %>%
  summarise(
    mean_salinity = mean(Salinity_PPT, na.rm = TRUE),
    SEM_salinity = sd(Salinity_PPT, na.rm = TRUE) / sqrt(n())
  )

# Export summary table as CSV
write_csv(summary_datelocation, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/salinity_summarytable_datelocation.csv")

# SUMMARY TABLE (MEAN & SEM) BY LOCATION

# Calculate mean and SEM for each location across all dates
summary_location <- salinity %>%
  group_by(Location) %>%
  summarise(
    mean_salinity = mean(Salinity_PPT, na.rm = TRUE),
    SEM_salinity = sd(Salinity_PPT, na.rm = TRUE) / sqrt(n())
  )

# Export summary table as CSV
write_csv(summary_location, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/salinity_summarytable_location.csv")


################################################################################
################################################################################
# SALINITY STATISTICS
################################################################################
################################################################################

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
salinity_shapiro <- salinity %>%
  group_by(Location) %>%
  shapiro_test(Salinity_PPT)
# all p-values < 0.05 = none of the locations have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST #####

# Kruskal-Wallis test for differences between locations
salinity_kruskal <- kruskal_test(Salinity_PPT ~ Location, data = salinity)
# p = 0.0692 = NO sig. difference between sites