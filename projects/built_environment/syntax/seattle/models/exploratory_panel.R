library(tidyverse)
library(lavaan)
library(semPlot)

load("./data/derived/be_panel_wide.RData")

panel_model <- '
concentrated_disadvantage_00 ~ concentrated_disadvantage_90
ethnicity_immiggration_00    ~ ethnicity_immigration_90
resid_stability_00                          ~ residential_stability_90

collective_efficacy_02_03 ~ eb_soccap_90 + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00                     

violent_crime_rate_03_05 ~ collective_efficacy_02_03 + violent_crime_rate_90  + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00                     

violent_crime_rate_90 ~ eb_soccap_90 + concentrated_disadvantage_90 + ethnicity_immigration_90 + residential_stability_90

vacant_pct_04_mean ~ vacant_pct_91_mean + collective_efficacy_02_03 + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00
street_lighting_02_04_mean ~ street_lighting_97_99_mean + collective_efficacy_02_03 + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00
public_housing_n_02_04_mean ~ public_housing_n_89_91_mean + collective_efficacy_02_03 + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00
public_facilities_02_03_count ~ public_facilities_89_90_count + collective_efficacy_02_03 + concentrated_disadvantage_00 + ethnicity_immiggration_00 + resid_stability_00

vacant_pct_91_mean ~ eb_soccap_90 + concentrated_disadvantage_90 + ethnicity_immigration_90 + residential_stability_90
street_lighting_97_99_mean ~ eb_soccap_90 + concentrated_disadvantage_90 + ethnicity_immigration_90 + residential_stability_90
public_housing_n_89_91_mean ~ eb_soccap_90 + concentrated_disadvantage_90 + ethnicity_immigration_90 + residential_stability_90
public_facilities_89_90_count ~ eb_soccap_90 + concentrated_disadvantage_90 + ethnicity_immigration_90 + residential_stability_90
'
panel_model_fit <- sem(panel_model, data = be_panel_wide %>% mutate(across(-TRACT_90, standardize)))
semPaths(panel_model_fit)
summary(panel_model_fit)
