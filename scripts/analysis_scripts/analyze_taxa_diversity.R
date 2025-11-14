################################################################################
# This script calculates the Shannon Diversity Index for all tiles analyzed at each location during each sampling event, and uses a Generalized Additive Model (GAM) to determine statistically significant differences between locations and over time at each location.
# This is script number 2A for the biofouling taxon ID data (1. tidy_taxa.R, 2A. analyze_taxa_diversity.R).

# Created by Abigail Lewine on October 28, 2025.
# Last edited by Keiley Gregory on November 13, 2025.
################################################################################

library(tidyverse)
library(vegan) # for Shannon Diversity Index
library(mgcv)  # for GAM

# Import cleaned data csv file
taxa_tidy <- read_csv("~/CAPSTONE_PUBLICATION/data/tidy_data/taxa_tidy.csv")

# Reorder columns so nonliving taxon categories are at the end
taxa_reordered <- taxa_tidy %>% 
  relocate(Open_space, Sediment, .after = Turf)

##############################################################################
# Calculate Shannon Diversity Index
##############################################################################

# Calculate Shannon Diversity Index (SDI) by location and sampling date using ONLY living taxa columns (creates new 'Shannon_Diversity_Index' column that has SDI value for each row (aka each location and sampling date))
shannon_diversity_allcols <- taxa_reordered %>%
  rowwise() %>%
  mutate(Shannon_Diversity_Index = diversity(c_across(Anemones:Turf), index = "shannon"))

# Select only relevant columns for SDI and reorder columns so 'Shannon_Diversity_Index' is directly after location and date columns
shannon_diversity <- shannon_diversity_allcols %>%
  dplyr::select(
    Location,
    Sampling_date,
    Sampling_interval_day,
    Shannon_Diversity_Index,
    Count_total_points,
    Count_valid_points
  )

# Export Shannon Diversity Index calculations as CSV
write_csv(shannon_diversity, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/taxa_diversity.csv")

##############################################################################
# Generalized Additive Model (GAM) for Shannon Diversity Index
##############################################################################

# Create clean data frame for GAM
gamdf <- shannon_diversity %>%
  ungroup() %>%  # just in case any grouping is hanging around
  mutate(Location = factor(Location)) %>%  # ensure location is a factor
  as.data.frame()

# Fit GAM (separate smooth of Sampling_interval_day for each Location)
GAM <- mgcv::gam(
  Shannon_Diversity_Index ~ 
    s(Sampling_interval_day, by = Location, k = 5) +  # location-specific smooths
    Location,                                         # main effect of location
  data = gamdf, method = "REML"
)
summary(GAM)

#-------------------------------------------------------------------
# Family: gaussian 
# Link function: identity 
# 
# Formula:
#   Shannon_Diversity_Index ~ s(Sampling_interval_day, by = Location, 
#                               k = 5) + Location
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.71573    0.09865  17.392 3.81e-08 ***
#   LocationKRM -0.27346    0.13951  -1.960   0.0822 .  
# LocationYHG  0.05614    0.13951   0.402   0.6969    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value
# s(Sampling_interval_day):LocationBRB 1.842  2.264 2.214   0.179
# s(Sampling_interval_day):LocationKRM 1.737  2.139 0.923   0.444
# s(Sampling_interval_day):LocationYHG 2.573  3.082 2.188   0.146
# 
# R-sq.(adj) =  0.485   Deviance explained = 73.2%
# -REML = 8.0036  Scale est. = 0.058393  n = 18
#-------------------------------------------------------------------

# Difference among locations: BRB is the reference site (Estimate = 1.71573). KRM has slightly lower SDI (-0.27346) than BRB (p 0.0822 = not significant); YHG has very slightly greater SDI (+0.05614) than BRB (p 0.6969 = not significant).

# Changes over time (smooth terms): None of the sites show a statistically significant change in SDI over time (BRB p = 0.179, KRM p = 0.444, YHG p = 0.146).

# Model fit: The model accounts for 73.2% of the overall pattern in the data (Deviance explained = 73.2%) and explains 48.5% of the variability in taxon richness after adjusting for model complexity (R²(adj) = 0.485). Low sample size (n = 18) which limits statistical power.

# OVERALL: The GAM shows that Shannon Diversity Index differs insignificantly between locations and over time at each location. Overall, this model performs moderately well and the fit is strong enough to be informative, but the low sample size limits statistical power and makes weaker patterns harder to detect.

##############################################################################
# Save fitted GAM model object for use in plotting script
##############################################################################

# NOTE: GAM model CANNOT be saved to CSV because CSV can only store table-like numeric/text values. RDS files store one R object (a model, list, dataframe, plot object, etc.) and keep everything about the object intact (structure, data, model coefficients, fitted values, formulas, etc.).

# Export GAM model as RDS file 
saveRDS(GAM, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_model_diversity.rds")

# Export gamdf as RDS file (optional, do if you want to reuse it)
saveRDS(gamdf, "~/CAPSTONE_PUBLICATION/data/analyzed_data/taxa_analyzed/GAM_results/GAM_gamdf_diversity.rds")

##############################################################################
# GAM diagnostic plots
##############################################################################

# Set R to show multipanel plot layout for the following plots (4 plots in grid)
par(mfrow = c(2, 2))

# gam.check() produces four diagnostic plots for each GAM model: 1) Residuals vs fitted, 2) QQ-plot of residuals, 3) Response vs fitted, 4) Histogram of residuals
gam.check(GAM)

# Remove multipanel layout for subsequent plots
par(mfrow = c(1, 1))

# gam.check() PLOTS INTERPRETAION ------------------------------------------------------------

# (1) QQ-plot of Deviance Residuals (top left panel)
#### Purpose: Compares your model residuals to a perfect Gaussian distribution.
#### Interpretation of your plot: Points follow the red line moderately well in the center; Slight deviation in the lower tail (left side) = a few residuals slightly more negative than expected under a perfect Gaussian; Upper tail matches well.
#### What this means: No serious violation of normality; Mild tail deviation is expected with n = 18 and ecological count/percent data; QQ-plot does not indicate a need for a different error family.
#### Verdict: Residuals are approximately normal = no concern.

# (2) Residuals vs Linear Predictor (top right panel)
#### Purpose: A random scatter around zero indicated no relationship between fitted values and residuals.
#### Interpretation of your plot: Points are scattered vertically around zero; No funnel shape (= homoscedasticity is OK); No curve or pattern (= no nonlinearity left in residuals is evident); Spread looks constant across the x-axis.
#### What this means: The GAM captured most temporal structure; There’s no systematic bias in the fitted values.
#### Verdict: No heteroscedasticity, no systematic bias.

# (3) Histogram of Residuals (bottom left panel)
#### Purpose: Should show a roughly bell-shaped distribution.
#### Interpretation of your plot: Almost symmetric around zero; Slight negative skew (few slightly low predictions); No strong heavy tails; With n = 18, histograms are coarse and not highly diagnostic.
#### Verdict: Residual distribution is acceptable, no red flags.

# (4) Response vs Fitted Values (bottom right panel)
#### Purpose: Observed values vs predicted values should align roughly in a straight pattern with scatter around it.
#### Interpretation of your plot: Most points cluster closely around a rising pattern → GAM fits general trend correctly; A few points deviate slightly, but within expected range for ecological data; No major curvature suggesting missing terms.
#### What this means: The GAM’s fitted values correspond well to observed Shannon diversity values.
#### Verdict: Good correspondence, the model is not underfitting or overfitting.

# ⭐⭐ FINAL INTERPRETATION SUMMARY ⭐⭐

#### This GAM passes all standard diagnostic checks: ✔ Residuals are roughly normal, ✔ No heteroscedasticity, ✔ No obvious underfitting, ✔ No obvious overfitting, ✔ Smooth terms behave appropriately, ✔ Model form is appropriate for the data

#### The only limitation is the extremely small sample size, which inflates p-values and reduces the sensitivity to detect temporal changes.

#### The model is: ✔ Structurally valid, ✔ Statistically appropriate, ✔ Interpretable and stable, ✔ Diagnostically clean

#### BUT power is low, ❗️❗️ so lack of significance should be interpreted with caution ❗️❗️
