################################################################################
# This script analyzes water quality data collected by the UVI EAL from 2022-2025. The chosen markers of water quality from the EAL-collected data are turbidity, pH, total suspended solids (TSS), dissolved oxygen (DO) concentration, Enterococcus (bacteria) concentration, phosphorus concentration, and nitrogen concentration. Summary tables are created containing the calculated mean and standard error of the mean (SEM) for each water quality variable in each location-year group and in each location across all years. Statistically significant differences in the measured values of each water quality variable between locations (all years combined) are determined using various statistical tests depending on the specific characteristics of each variable's data (normality of residuals, homogeneity of variances, etc.)
# This is script number 2 for the EAL water quality data (1. tidy_water_quality.R., 2. analyze_water_quality.R).

# *NOTE! There was no data collected from 2025-02-04 to 2025-06-18, which is when the capstone sampling period occurred. Therefore, we average all data for entire time frame of EAL data collection spanning 2022-2025.

# Created by Keiley Gregory on September 16, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(lubridate)
library(janitor)
library(car)           # for Levene's Test
library(rstatix)       # for various stat tests
library(multcompView)  # for significance letters

# Load tidy CSV
eal_tidy <- read_csv("~/CAPSTONE_PUBLICATION/data/tidy_data/water_quality_tidy.csv")

# Examine imported dataframe
print(colnames(eal_tidy))
print(unique(eal_tidy$MonitoringLocationName))

################################################################################
# Since we are comparing water quality across sites, we need to make location a factor
eal_tidy <- eal_tidy %>%
  mutate(MonitoringLocationName = as.factor(MonitoringLocationName))

# Create vector of variables that you want to calculate the average for (check these match actual column names using output printed in console from <print(colnames(eal_tidy))> code)
numeric_cols <- c(
  "Turbidity",
  "pH",
  "Enterococcus",
  "Phosphorus",
  "TSS",
  "DO",
  "Nitrogen"
)

# OPTIONAL: See which values in each column will be turned into NAs when the data type for columns in 'numeric_cols' vector is forced to numeric
eal_tidy %>% filter(!str_detect(Turbidity, "^[0-9.]+$") & !is.na(Turbidity)) %>% tabyl(Turbidity)
eal_tidy %>% filter(!str_detect(pH, "^[0-9.]+$") & !is.na(pH)) %>% tabyl(pH)
eal_tidy %>% filter(!str_detect(Enterococcus, "^[0-9.]+$") & !is.na(Enterococcus)) %>% tabyl(Enterococcus) # <1 (n=1), <1.0 (n=1), <10 (n=35)
eal_tidy %>% filter(!str_detect(Phosphorus, "^[0-9.]+$") & !is.na(Phosphorus)) %>% tabyl(Phosphorus) # <0.0014 (n=1), <0.0023 (n=1)
eal_tidy %>% filter(!str_detect(TSS, "^[0-9.]+$") & !is.na(TSS)) %>% tabyl(TSS) # <0.45 (n=2)
eal_tidy %>% filter(!str_detect(DO, "^[0-9.]+$") & !is.na(DO)) %>% tabyl(DO)
eal_tidy %>% filter(!str_detect(Nitrogen, "^[0-9.]+$") & !is.na(Nitrogen)) %>% tabyl(Nitrogen) # <0.012 (n=1), <0.02 (n=1)

# Force columns in 'numeric_cols' vector to numeric data type (any text values, such as values with "<", will become NAs)
eal_numeric <- eal_tidy %>%
  mutate(across(all_of(numeric_cols), ~as.numeric(.)))

# Define SEM function (return NA if water quality variable has no measured values or only 1 measured value for each individual location-year group)
SEM_function <- function(x, na.rm = TRUE) {
  n <- sum(!is.na(x))
  if (n == 0) {
    return(NA_real_)
  }
  sd(x, na.rm = na.rm) / sqrt(n)
}

# Define mean function (return NA if water quality variable has no measured values for each individual location-year group - !! this function is necessary instead of just using R's mean calculation to ensure that output dataframe has correct NA value for the mean any variables that had no measurements in a single location-year group, such as mean_Enterococcus in KRM 2025)
mean_function <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    mean(x, na.rm = TRUE)
  }
}
################################################################################


################################################################################
# FIRST GOING TO AVERAGE ALL DATA IN EAL_MASTER CSV BY LOCATION AND YEAR.
################################################################################

# Group by location and year and compute mean and SEM of all variables for each location during each year
eal_mean_yr_location <- eal_numeric %>%
  mutate(Year = lubridate::year(DateTime_AST)) %>%  # add year column for grouping
  group_by(MonitoringLocationName, Year) %>%
  summarise(
    across(all_of(numeric_cols), mean_function, .names = "mean_{.col}"),
    across(all_of(numeric_cols), SEM_function, .names = "SEM_{.col}"),
    .groups = "drop"
  )

# Note: Output dataframe (eal_mean_yr_location) has NAs in the following cells:
#### 'mean_Enterococcus' and 'SEM_Enterococcus' cols for KRM 2025 because this location-year group has no measurements recorded for Enterococcus
#### 'SEM_Enterococcus' col for BRB 2024, BRB 2025, and KRM 2022 because each of these location-year groups only had 1 measurement recorded for Enterococcus
#### 'SEM_Phosphorus' col for KRM 2022 because this location-year group has only 1 measurement recorded for Phosphorus
#### 'SEM_TSS' col for KRM 2022 because this location-year group has only 1 measurement recorded for Total Suspended Solids (TSS)
#### 'SEM_Nitrogen' col for KRM 2022 because this location-year group has only 1 measurement recorded for Nitrogen

# Interleave (mean_X, SEM_X, mean_Y, SEM_Y, etc.) and reorder columns
ordered_cols_yr <- c("MonitoringLocationName", "Year") %>%
  append(unlist(map(numeric_cols, ~c(paste0("mean_", .x), paste0("SEM_", .x)))))
eal_mean_yr_location_ordered <- eal_mean_yr_location[, ordered_cols_yr]


################################################################################

# Save summary table as a CSV in project directory
write_csv(eal_mean_yr_location_ordered, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/water_quality_summarytable_yearlocation.csv")

################################################################################


################################################################################
# THERE WAS NO DATA COLLECTED FROM 2025-02-04 TO 2025-06-18 (AKA DURING THE CAPSTONE STUDY PERIOD). THEREFORE, NOW GOING TO AVERAGE ALL DATA IN EAL_MASTER CSV BY LOCATION ACROSS ALL YEARS.
################################################################################

# Group by location and compute mean and SEM of all variables for each location across all years
eal_mean_location <- eal_numeric %>%
  group_by(MonitoringLocationName) %>%
  summarise(
    across(all_of(numeric_cols), mean_function, .names = "mean_{.col}"),
    across(all_of(numeric_cols), SEM_function, .names = "SEM_{.col}"),
    .groups = "drop"
  )

# Interleave (mean_X, SEM_X, mean_Y, SEM_Y, etc.) and reorder columns
ordered_cols <- c("MonitoringLocationName") %>%
  append(unlist(map(numeric_cols, ~c(paste0("mean_", .x), paste0("SEM_", .x)))))
eal_mean_location_ordered <- eal_mean_location[, ordered_cols]

################################################################################

# Save summary table as a CSV in project directory
write_csv(eal_mean_location_ordered, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/water_quality_summarytable_location.csv")

################################################################################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


################################################################################
# DISSOLVED OXYGEN (DO) STATISTICS
################################################################################

# Filter for selected columns and only rows with valid DO values
DO <- eal_numeric %>%
  filter(!is.na(DO)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         DO)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
DO_shapiro <- DO %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(DO)
# KRM and YHG p-value < 0.05 = KRM and YHG do not have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST + DUNN'S POST HOC #####

# Kruskal-Wallis test for differences between locations
DO_kruskal <- kruskal_test(DO ~ MonitoringLocationName, data = DO)
summary(DO_kruskal)
# n   statistic   df  p    
# 122 40.94827    2   1.28e-09

# Dunn's post hoc test with Bonferroni correction
DO_dunn <- DO %>%
  dunn_test(DO ~ MonitoringLocationName, p.adjust.method = "bonferroni")
# BRB x KRM p.adj = 3.64e-7; BRB x YHG p.adj = 9.04e-9

# Export Dunn's test results
write_csv(DO_dunn, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/DO_dunnstest.csv")

##### DUNN'S POST HOC SIGNIFICANCE LETTERS #####

# Convert Dunn’s results to a named vector of p-values
DO_pvals <- DO_dunn %>%
  mutate(comparison = paste(group1, group2, sep = "-")) %>%
  select(comparison, p.adj) %>%
  deframe()

# Generate vector of compact letters (BRB "a", KRM and YHG "b" = indicates that BRB is significantly different from both KRM and YHG, while KRM and YHG are not significantly different from each other)
DO_letters <- multcompLetters(DO_pvals)$Letters

# Convert sig. letters to dataframe for joining to main DO dataframe
DO_dfletters <- tibble(
  MonitoringLocationName = names(DO_letters),
  sig_letter = DO_letters
)

# Join significance letters back to DO dataframe (*for future plotting)
DO_sigletters <- DO %>%
  left_join(DO_dfletters, by = "MonitoringLocationName")

# Export full DO dataframe WITH sig. letters as CSV for plotting
write_csv(DO_sigletters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/DO_sigletters.csv")

################################################################################
# PH STATISTICS
################################################################################

# Filter for selected columns and only rows with valid pH values
pH <- eal_numeric %>%
  filter(!is.na(pH)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         pH)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
pH_shapiro <- pH %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(pH)
# all p-values > 0.05 = all normally distributed data = ANOVA assumption of normality is met

# Homogeneity of variances
pH_levene <- leveneTest(pH ~ MonitoringLocationName, data = pH)
pH_bartlett <- bartlett.test(pH ~ MonitoringLocationName, data = pH)
# both p-values > 0.05 = all homoscedastic data = ANOVA assumption of homoscedasticity is met

##### ONE-WAY ANOVA TEST + TUKEY'S HSD POST HOC #####

# One-way ANOVA test for differences between locations
pH_anova <- aov(pH ~ MonitoringLocationName, data = pH)
summary(pH_anova)
#                         Df  Sum Sq  Mean Sq   F value   Pr(>F)   
# MonitoringLocationName  2   0.1205  0.06024   6.568     0.00197 **
# Residuals               119 1.0915  0.00917                   

# Tukey’s HSD post hoc test
pH_tukey <- TukeyHSD(pH_anova)
print(pH_tukey)
# BRB x YHG p.adj = 0.0013473

# Convert Tukey HSD results to a data frame
pH_tukey_df <- as.data.frame(pH_tukey$MonitoringLocationName)

# Add comparison labels (rownames become a column)
pH_tukey_df <- pH_tukey_df %>%
  rownames_to_column("comparison")

# Export Tukey's HSD test results
write_csv(pH_tukey_df, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/pH_tukeyshsdtest.csv")

##### TUKEY'S HSD POST HOC SIGNIFICANCE LETTERS #####

# Generate compact letters (BRB "a", KRM "ab", YHG "b" = indicates that YHG and BRB are significantly different from each other, but KRM is not significantly different from either of them aka KRM is an intermediate group)
pH_letters <- multcompLetters4(pH_anova, pH_tukey)

# Extract only the "Letters" element (a named character vector)
pHletters_vec <- pH_letters$MonitoringLocationName$Letters

# Convert to tibble
pH_dfletters <- tibble(
  MonitoringLocationName = names(pHletters_vec),
  sig_letter = pHletters_vec
)

# Debug check
print(pH_dfletters)

# Join sig letters back to pH dataframe
pH_sigletters <- pH %>%
  left_join(pH_dfletters, by = "MonitoringLocationName")

# Export full pH dataframe WITH sig. letters as CSV for plotting
write_csv(pH_sigletters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/pH_sigletters.csv")

################################################################################
# TURBIDITY STATISTICS
################################################################################

# Filter for selected columns and only rows with valid turbidity values
turbidity <- eal_numeric %>%
  filter(!is.na(Turbidity)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         Turbidity)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
turbidity_shapiro <- turbidity %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(Turbidity)
# all p-values < 0.05 = none of the locations have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST + DUNN'S POST HOC #####

# Kruskal-Wallis test for differences between locations
turbidity_kruskal <- kruskal_test(Turbidity ~ MonitoringLocationName, data = turbidity)
# n   statistic   df  p 
# 146 55.74798    2   7.84e-13

# Dunn's post hoc test with Bonferroni correction
turbidity_dunn <- turbidity %>%
  dunn_test(Turbidity ~ MonitoringLocationName, p.adjust.method = "bonferroni")
# BRB x YHG p.adj = 2.08e-11; KRM x YHG p.adj = 9.90e-07

# Export Dunn's test results
write_csv(turbidity_dunn, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/turbidity_dunnstest_outlier.csv")

##### DUNN'S POST HOC SIGNIFICANCE LETTERS #####

# Convert Dunn’s results to a named vector of p-values
turbidity_pvals <- turbidity_dunn %>%
  mutate(comparison = paste(group1, group2, sep = "-")) %>%
  select(comparison, p.adj) %>%
  deframe()

# Generate compact letters (BRB and KRM "a", YHG "b" = indicates that BRB and KRM are statistically similar to each other, but both are significantly different from YHG)
turbidity_letters <- multcompLetters(turbidity_pvals)$Letters

# Convert to dataframe for joining
turbidity_dfletters <- tibble(
  MonitoringLocationName = names(turbidity_letters),
  sig_letter = turbidity_letters
)

# Join significance letters back to the turbidity dataframe
turbidity_sigletters <- turbidity %>%
  left_join(turbidity_dfletters, by = "MonitoringLocationName")

# Export full turbidity dataframe WITH sig. letters as CSV for plotting
write_csv(turbidity_sigletters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/turbidity_sigletters_outlier.csv")

################################################################################
# TOTAL SUSPENDED SOLIDS (TSS) STATISTICS
################################################################################

# Filter for selected columns and only rows with valid TSS values
TSS <- eal_numeric %>%
  filter(!is.na(TSS)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         TSS)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
TSS_shapiro <- TSS %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(TSS)
# BRB p < 0.05 = BRB does not have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST + DUNN'S POST HOC #####

# Kruskal-Wallis test for differences between locations
TSS_kruskal <- kruskal_test(TSS ~ MonitoringLocationName, data = TSS)
# p = 0.601 = NO sig. difference between locations

################################################################################
# PHOSPHORUS STATISTICS
################################################################################

# Filter for selected columns and only rows with valid phosphorus values
phosphorus <- eal_numeric %>%
  filter(!is.na(Phosphorus)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         Phosphorus)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
phosphorus_shapiro <- phosphorus %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(Phosphorus)
# YHG p = 0.04982 (borderline) = ANOVA assumption of normality is still technically violated (even though value is borderline), so use KW test just to be safe

##### KRUSKAL WALLIS TEST + DUNN'S POST HOC #####

# Kruskal-Wallis test for differences between locations
phosphorus_kruskal <- kruskal_test(Phosphorus ~ MonitoringLocationName, data = phosphorus)
# p = 0.449 = NO sig. difference between sites

################################################################################
# NITROGEN STATISTICS
################################################################################

# Filter for selected columns and only rows with valid nitrogen values
nitrogen <- eal_numeric %>%
  filter(!is.na(Nitrogen)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         Nitrogen)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
nitrogen_shapiro <- nitrogen %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(Nitrogen)
# all p-values > 0.05 = all normally distributed data = ANOVA assumption of normality is met

# Homogeneity of variances
nitrogen_levene <- leveneTest(Nitrogen ~ MonitoringLocationName, data = nitrogen)
nitrogen_bartlett <- bartlett.test(Nitrogen ~ MonitoringLocationName, data = nitrogen)
# both p-values > 0.05 = all homoscedastic data = ANOVA assumption of homoscedasticity is met

##### ONE-WAY ANOVA TEST + TUKEY'S HSD POST HOC #####

# One-way ANOVA test for differences between locations
nitrogen_anova <- aov(Nitrogen ~ MonitoringLocationName, data = nitrogen)
summary(nitrogen_anova)
#                         Df  Sum Sq  Mean Sq   F value   Pr(>F)  
# MonitoringLocationName  2   0.0778  0.0389    4.418     0.0165 *
# Residuals               57  0.5018  0.0088

# Tukey’s HSD post hoc test
nitrogen_tukey <- TukeyHSD(nitrogen_anova)
print(nitrogen_tukey)
# KRM x YHG p.adj = 0.0223793

# Convert Tukey HSD results to a data frame
nitrogen_tukey_df <- as.data.frame(nitrogen_tukey$MonitoringLocationName)

# Add comparison labels (row names become column names)
nitrogen_tukey_df <- nitrogen_tukey_df %>%
  rownames_to_column("comparison")

# Export Tukey's HSD test results
write_csv(nitrogen_tukey_df, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/nitrogen_tukeyshsdtest.csv")

##### TUKEY'S HSD POST HOC SIGNIFICANCE LETTERS #####

# Generate compact letters (YHG "a", BRB "ab", KRM "b" = indicates that while YHG and KRM are significantly different from each other but BRB is not significantly different from either of them aka BRB is an intermediate group)
nitrogen_letters <- multcompLetters4(nitrogen_anova, nitrogen_tukey)

# Extract only the "Letters" element (a named character vector)
nitletters_vec <- nitrogen_letters$MonitoringLocationName$Letters

# Convert to tibble
nitrogen_dfletters <- tibble(
  MonitoringLocationName = names(nitletters_vec),
  sig_letter = nitletters_vec
)

# Debug check
print(nitrogen_dfletters)

# Join back to dataset
nitrogen_sigletters <- nitrogen %>%
  left_join(nitrogen_dfletters, by = "MonitoringLocationName")

# Export full nitrogen dataframe WITH sig. letters as CSV for plotting
write_csv(nitrogen_sigletters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/nitrogen_sigletters.csv")

################################################################################
# ENTEROCOCCUS STATISTICS
################################################################################

# Filter for selected columns and only rows with valid Enterococcus values
enterococcus <- eal_numeric %>%
  filter(!is.na(Enterococcus)) %>%
  select(MonitoringLocationName,
         MonitoringLocationIdentifier,
         Latitude,
         Longitude,
         DateTime_AST,
         Enterococcus)

##### TEST ANOVA ASSUMPTIONS #####

# Normality of residuals
enterococcus_shapiro <- enterococcus %>%
  group_by(MonitoringLocationName) %>%
  shapiro_test(Enterococcus)
# BRB and YHG p-values > 0.05 = BRB and YHG do not have normally distributed data = ANOVA assumption of normality is violated

##### KRUSKAL WALLIS TEST + DUNN'S POST HOC #####

# Kruskal-Wallis test for differences between locations
enterococcus_kruskal <- kruskal_test(Enterococcus ~ MonitoringLocationName, data = enterococcus)
# p = 0.061 = NO sig. difference between locations
