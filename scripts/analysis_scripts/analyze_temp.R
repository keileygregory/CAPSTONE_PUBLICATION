################################################################################
# This script analyzes water temperature data collected hourly by temp loggers. A summary table is created containing the calculated mean and standard error of the mean (SEM) of water temperature for each location across all the entire study period. The presence of statistically significant differences in the measured water temperature values is assessed both spatially (between locations) and temporarily (over time at each location) using a Generalized Additive Model (GAM).
# This is script number 2 for the water temperature data (1. tidy_temp.R, 2. analyze_temp.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(lubridate)    # for dates/times
library(mgcv)         # for GAM
library(gratia)       # diagnostics + smooth plots
library(emmeans)      # post-hoc comparisons
library(patchwork)    # visualization assembly

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
# WATER TEMPERATURE STATISTICS (GAM)
################################################################################

# Since we are comparing water quality across sites, we need to make location a factor (ALREADY DONE in beginning of script). *For GAM, also need to create new 'time_hours' column.
temp_gam <- temp_tidy %>%
  mutate(time_hours = as.numeric(Datetime_UTC - min(Datetime_UTC, na.rm = TRUE)) / 3600)

# Fit the GAM (nonlinear change over time by location). This model tests mean differences between locations (parametric Location term) AND allows separate nonlinear time trends per location (s(time_hours, by = Location)).
GAM <- mgcv::gam(
  Temp_C ~ 
    s(time_hours, by = Location, k = 40) +  # location-specific smooths
    Location,                               # main effect of location
  data = temp_gam, method = "REML"
)
summary(GAM)

#-------------------------------------------------------------------
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Temp_C ~ s(time_hours, by = Location, k = 40) + Location
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 27.127367   0.006639 4086.06   <2e-16 ***
#   LocationKRM  0.544165   0.009390   57.95   <2e-16 ***
#   LocationYHG  0.330136   0.009389   35.16   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(time_hours):LocationBRB 29.08  33.92 104.9  <2e-16 ***
#   s(time_hours):LocationKRM 34.25  37.63 168.6  <2e-16 ***
#   s(time_hours):LocationYHG 34.09  37.54 129.1  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.739   Deviance explained = 74.3%
# -REML =   1745  Scale est. = 0.094147  n = 6407
#-------------------------------------------------------------------

# PARAMETRIC TERMS (SPATIAL DIFFERENCES): BRB (reference location) mean ≈ 27.13 °C. KRM is sig. hotter (+0.54 °C) than BRB (p < 2e-16). YHG is also sig. hotter (+0.33 °C) than BRB (p < 2e-16). 

# SMOOTH TERMS (NONLINEAR CHANGE OVER TIME AT EACH LOCATION): All three locations have *strong, nonlinear temporal patterns*. edf >> 1 means curves are decidedly non-linear. p < 2e-16 for all locations = temperature varied significantly over time at each site.

# MODEL FIT: R²(adj) ≈ 0.74 and deviance explained ≈ 74%. Good performance for environmental time-series.

##############################################################################
# Save fitted GAM model object for use in plotting script
##############################################################################

# NOTE: GAM model CANNOT be saved to CSV because CSV can only store table-like numeric/text values. RDS files store one R object (a model, list, dataframe, plot object, etc.) and keep everything about the object intact (structure, data, model coefficients, fitted values, formulas, etc.).

# Export GAM model as RDS file 
saveRDS(GAM, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/GAM_results/GAM_model_temp.rds")

##############################################################################
# GAM diagnostic plots
##############################################################################

# Set R to show multipanel plot layout for the following plots (4 plots in grid)
par(mfrow = c(2, 2))

# gam.check() produces four diagnostic plots for each GAM model: 1) Residuals vs fitted, 2) QQ-plot of residuals, 3) Response vs fitted, 4) Histogram of residuals
gam.check(GAM)

# Remove multipanel layout for subsequent plots
par(mfrow = c(1, 1))

#### QQ plot: points lie very close to the line; slight upward deviation in the upper tail only = normality is fine. 
#### Histogram of residuals: symmetric, bell-shaped, centered around 0 = good. 
#### Residuals vs linear predictor: striping/banding, but no big funnel shape or curvature; that banding is just because Temp_C is recorded to a fixed decimal and there are lots of points at similar fitted values = no obvious heteroskedasticity problem. 
#### Observed vs fitted: clean increasing relationship; points cluster around the diagonal = model captures the structure well. OVERALL residuals look good for a Gaussian GAM.

##############################################################################
# GAM significance letters 
##############################################################################
library(emmeans)
library(multcomp)
library(multcompView)

# Post hoc test to determine specific differences btwn sites and assign significance letters (or symbols in this case for now)
emm <- emmeans(GAM, "Location")

letters_df <- multcomp::cld(
  emm, adjust = "sidak", Letters = multcompView::multcompLetters)

letters_df

# Location  emmean  SE      df    lower.CL upper.CL .group
# BRB       27.41   0.0358  6307  27.32    27.49    <    
# YHG       27.91   0.0388  6307  27.82    28.00    0   
# KRM       28.36   0.0388  6307  28.27    28.46    .  

# Confidence level used: 0.95 
# Conf-level adjustment: sidak method for 3 estimates 
# P value adjustment: sidak method for 3 tests 
# significance level used: alpha = 0.05 
# NOTE: If two or more means share the same grouping symbol, then we cannot show them to be different. But we also did not show them to be the same. 

# Change sig letters from </0/. to a/b/c to match normal formatting/other plots
clean_letters <- letters_df %>%
  mutate(
    group_trim = trimws(.group),       
    sig_letter = case_when(
      group_trim == "<" ~ "a",
      group_trim == "0" ~ "b",
      group_trim == "." ~ "c",
      TRUE ~ group_trim
    )
  ) %>%
  dplyr::select(Location, sig_letter)

# BRB a, KRM c, YHG b
clean_letters

# Join significance letters to temp_tidy
temp_with_letters <- temp_tidy %>%
  left_join(clean_letters, by = "Location")

# Export as CSV
write_csv(temp_with_letters, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/significance_letters/temp_sigletters.csv.csv")
