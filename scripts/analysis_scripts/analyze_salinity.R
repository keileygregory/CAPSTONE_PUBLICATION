################################################################################
# This script analyzes salinity data collected during each sampling event. Summary tables are created containing the calculated mean and standard error of the mean (SEM) of salinity for each location-sampling event group and for each location across all sampling events. The presence of statistically significant differences in the measured salinity values is assessed both spatially (between locations) and temporarily (over time at each location) using a Generalized Additive Mixed Model (GAMM).
# This is script number 1 (first script) for the salinity data (1. analyze_salinity.R).

# Created by Keiley Gregory on September 23, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(lubridate)    # for dates/times
library(mgcv)         # for GAM
library(gratia)       # diagnostics + smooth plots
library(emmeans)      # post-hoc comparisons (not used here because no significance)
library(patchwork)    # visualization assembly
library(gamm4)        # for GAMM with random effects

# Load salinity data (did not require any tidying)
salinity <- read_csv("~/CAPSTONE_PUBLICATION/data/raw_data/salinity_raw.csv")

# Since we are comparing water quality across sites, we need to make location a factor
salinity <- salinity %>%
  mutate(Location = as.factor(Location))

################################################################################
# SUMMARY TABLE (MEAN & SEM) BY LOCATION-SAMPLING EVENT GROUP
################################################################################

# Calculate mean and SEM for each location during each sampling event date
summary_datelocation <- salinity %>%
  group_by(Location, Date) %>%
  summarise(
    mean_salinity = mean(Salinity_PPT, na.rm = TRUE),
    SEM_salinity = sd(Salinity_PPT, na.rm = TRUE) / sqrt(n())
  )

# Export summary table as CSV
write_csv(summary_datelocation, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/salinity_summarytable_datelocation.csv")

################################################################################
# SUMMARY TABLE (MEAN & SEM) BY LOCATION ONLY
################################################################################

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
# SALINITY STATISTICS (GAMM)
################################################################################

# !!NOTE!!: Using GAMM for salinity (instead of normal GAM) because raw salinity data being used for stat calculations includes *REPLICATES* (6 measurements recorded at same location on same date due to 6 transects at each site). 

# Prepare data for GAMM by making time numeric and grouping by sampling event
gammdf <- salinity %>%
  mutate(
    day = as.numeric(Date - min(Date)),  # numeric time (days since first sampling date)
    Sampling_event = interaction(Location, Date, drop = TRUE))  # sampling event factor: each Location × Date combo is one event

# Fit GAMM (salinity ~ location + smooth of day (by location)) with random intercept for sampling event (location:date)
GAMM_salinity <- gamm4(
  Salinity_PPT ~ Location + s(day, by = Location, k = 6),
  random = ~(1 | Sampling_event),
  data = gammdf
)

# Extract the GAM component (fixed effects + smooths)
GAMM <- GAMM_salinity$gam
summary(GAMM)

#-------------------------------------------------------------------
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Salinity_PPT ~ Location + s(day, by = Location, k = 6)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   34.833      1.672  20.828   <2e-16 ***
# LocationKRM   -2.111      2.365  -0.893    0.374    
# LocationYHG   -1.167      2.365  -0.493    0.623    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(day):LocationBRB 1.000  1.000 1.000   0.320
# s(day):LocationKRM 1.442  1.442 0.650   0.624
# s(day):LocationYHG 1.000  1.000 0.078   0.781
# 
# R-sq.(adj) =  0.176   
# lmer.REML = 298.96  Scale est. = 0.47037   n = 108
#-------------------------------------------------------------------

##############################################################################
# Save fitted GAM model object for use in plotting script
##############################################################################

# NOTE: GAM model CANNOT be saved to CSV because CSV can only store table-like numeric/text values. RDS files store one R object (a model, list, dataframe, plot object, etc.) and keep everything about the object intact (structure, data, model coefficients, fitted values, formulas, etc.).

# Export GAMM model as RDS file 
saveRDS(GAMM, "~/CAPSTONE_PUBLICATION/data/analyzed_data/drivers_analyzed/GAM_results/GAMM_model_salinity.rds")

##############################################################################
# GAM diagnostic plots
##############################################################################

# Set R to show multipanel plot layout for the following plots (4 plots in grid)
par(mfrow = c(2, 2))

# gam.check() produces four diagnostic plots for each GAM model: 1) Residuals vs fitted, 2) QQ-plot of residuals, 3) Response vs fitted, 4) Histogram of residuals
gam.check(GAMM)

# Remove multipanel layout for subsequent plots
par(mfrow = c(1, 1))

# Check for concurvity (nonlinear collinearity in smooths)
concurvity(GAMM, full = TRUE)
