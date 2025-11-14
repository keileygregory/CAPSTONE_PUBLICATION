################################################################################
# This script calculates the Taxon Richness for all tiles analyzed at each location during each sampling event, and uses a Generalized Additive Model (GAM) to determine statistically significant differences between locations and sampling events (time).
# This is script number 2B for the biofouling taxon ID data (1. tidy_taxa.R, 2B. analyze_taxa_richness.R).

# Created by Abigail Lewine on October 28, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(mgcv)  # for GAM

# Import cleaned data csv file
taxa_tidy <- read_csv("~/CAPSTONE_PUBLICATION/data/tidy_data/taxa_tidy.csv")

# Reorder columns so nonliving taxon categories are at the end
taxa_reordered <- taxa_tidy %>% 
  relocate(Open_space, Sediment, .after = Turf)

##############################################################################
# Calculate Taxon Richness
##############################################################################

# Calculate Taxon Richness (the number of taxa with nonzero presence per sample)
richness <- taxa_reordered %>%
  rowwise() %>%  # ensure row-wise operations
  mutate(Taxon_Richness = sum(c_across(Anemones:Turf) > 0)) %>%  # count nonzero LIVING taxa per row
  group_by(Location, Sampling_date, Sampling_interval_day) %>%
  summarize(Taxon_Richness = sum(Taxon_Richness), .groups = "drop")  # sum richness per group

# Export full taxa dataframe with Taxon Richness calculations as CSV
write_csv(richness, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/taxa_richness.csv")

##############################################################################
# Generalized Additive Model (GAM) for Taxon Richness
##############################################################################

# Create clean data frame for GAM
gamdf <- richness %>%
  ungroup() %>%  # just in case any grouping is hanging around
  mutate(Location = factor(Location)) %>%  # ensure location is a factor
  as.data.frame()

# Fit GAM (separate smooth of Sampling_interval_day for each Location)
GAM <- mgcv::gam(
  Taxon_Richness ~ 
    s(Sampling_interval_day, by = Location, k = 5) +  # location-specific smooths
    Location,                                         # main effect of location
  data = gamdf, method = "REML"
)
summary(GAM)

# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Taxon_Richness ~ s(Sampling_interval_day, by = Location, k = 5) + 
#   Location
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  11.3333     0.8577  13.214 3.34e-08 ***
#   LocationKRM  -2.3333     1.2129  -1.924    0.080 .  
# LocationYHG   0.6667     1.2129   0.550    0.593    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value  
# s(Sampling_interval_day):LocationBRB 1.742  2.144 2.902   0.101  
# s(Sampling_interval_day):LocationKRM 1.000  1.000 2.913   0.116  
# s(Sampling_interval_day):LocationYHG 1.000  1.000 6.849   0.024 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =   0.51   Deviance explained = 67.5%
# -REML =  31.82  Scale est. = 4.4136    n = 18

# GAM INTERPRETAION ------------------------------------------------------------

# Difference among locations: BRB is the reference site (Estimate = 11.3333). KRM has slightly lower richness (-2.3333) than BRB (p 0.080 = not significant); YHG has very slightly greater richness (+0.6667) than BRB (p 0.593 = not significant).

# Changes over time (smooth terms): BRB and KRM sites show NO statistically significant change in richness over sampling interval (BRB p = 0.101, KRM p = 0.116). **YHG site shows a (slightly) statistically significant change in richness over sampling interval (YHG p = 0.024 *).

# Model fit: The model accounts for 67.5% of the overall pattern in the data (Deviance explained = 67.5%) and explains 51% of the variability in taxon richness after adjusting for model complexity (R²(adj) = 0.51). Low sample size (n = 18) which limits statistical power.

# OVERALL: The GAM shows that Taxon Richness differs insignificantly between locations. Temporal patterns are weak at BRB and KRM, but **YHG shows a significant change in richness over time, indicating that richness varies with sampling interval at that site**. Overall, this model performs moderately well and the fit is strong enough to be informative, but the low sample size limits statistical power and makes weaker patterns harder to detect.

##############################################################################
# Save fitted GAM model object for use in plotting script
##############################################################################

# NOTE: GAM model CANNOT be saved to CSV because CSV can only store table-like numeric/text values. RDS files store one R object (a model, list, dataframe, plot object, etc.) and keep everything about the object intact (structure, data, model coefficients, fitted values, formulas, etc.).

# Export GAM model as RDS file 
saveRDS(GAM, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_model_richness.rds")

# Export gamdf as RDS file (optional, do if you want to reuse it)
saveRDS(gamdf, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_gamdf_richness.rds")

##############################################################################
# GAM diagnostic plots
##############################################################################

# Set R to show multipanel plot layout for the following plots (4 plots in grid)
par(mfrow = c(2, 2))

# gam.check() produces four diagnostic plots for each GAM model: 1) Residuals vs fitted, 2) QQ-plot of residuals, 3) Response vs fitted, 4) Histogram of residuals
gam.check(GAM)

# Remove multipanel layout for subsequent plots
par(mfrow = c(1, 1))

# gam.check() PLOTS INTERPRETAION (see analyze_taxa_richness for more info on plots) -----------------------------------------------------

# (1) QQ-plot of Deviance Residuals: This model’s residuals are approximately normal, with mild left-tail deviation (this is not severe and is common in ecological models with small sample sizes). No major violation of normality (minor skewness is acceptable and expected).

# (2) Residuals vs Linear Predictor: The model fits reasonably well (no strong pattern suggests missing smoothing or missing predictors). The slight fan-shape is a very mild hint of increasing variance at high fitted values, but with n ≈ 18, this pattern is weak. Residual independence and homoscedasticity are acceptable.

# (3) Histogram of Residuals: The skew is mild (given small dataset size, histogram shape is likely never going to be completely perfect). Residual distribution is acceptable and consistent with Gaussian family assumptions.

# (4) Response vs Fitted Values: The model is capturing the overall trend quite well. The moderate scatter reflects natural ecological variability, small sample size (n=18), and the fact that GAMs don’t force fitted = observed. Model fits the data adequately; strong evidence that the GAM captures the true structure.

# FINAL INTERPRETATION SUMMARY

#### This GAM is well-behaved. There are no serious violations of assumptions, and the diagnostics support that: (1) The Gaussian error distribution is appropriate, (2) The smoothing structure is sufficient, (3) The model captures the response pattern, and (4) Deviations are small and likely due to limited sample size, not model failure. This GAM is valid.