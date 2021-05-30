library(tidyverse)
library(lme4)
load("./data/seattle/derived/be_panel_long.RData")

be_panel_analytical <- be_panel_long %>%
  mutate(across(-TRACT_1980, ~ standardize(.))) %>%
  mutate(TRACT_1980 = factor(TRACT_1980))


summary(lmer(violent_crime_rate ~ 
               collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + TIME + (1|TRACT_1980),
             data = be_panel_analytical))

summary(lmer(violent_crime_rate ~ 
               collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + (1|TRACT_1980) +
               arterial_roads_mean  + mixed_land_use_mean + vacant_pct_mean + PHYDIS_vacant,
             data = be_panel_analytical))

summary(lmer(VICT_prop_2 ~ 
               collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + TIME + (1|TRACT_1980),
             data = be_panel_analytical))

summary(lmer(VICT_prop_2 ~ 
               collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + (1|TRACT_1980) +
               arterial_roads_mean + street_lighting_mean + public_housing_n_mean + mixed_land_use_mean + vacant_pct_mean + public_facilities_mean + police_fire_mean +
               BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks,
             data = be_panel_analytical))

library(fixest)

summary(femlm(VICT_prop_2 ~ 
                collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density  +
                street_lighting_mean + public_housing_n_mean + mixed_land_use_mean + vacant_pct_mean + public_facilities_mean + police_fire_mean +
                BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks | TRACT_1980,
              family = "gaussian",
              data = be_panel_analytical))

summary(femlm(VICT_viol_2 ~ 
                collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density  +
                street_lighting_mean + public_housing_n_mean + mixed_land_use_mean + vacant_pct_mean + public_facilities_mean + police_fire_mean | TRACT_1980,
              family = "gaussian",
              data = be_panel_analytical))

summary(femlm(violent_crime_rate ~ 
                collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density   | TRACT_1980,
              family = "gaussian",
              data = be_panel_analytical))

summary(femlm(violent_crime_rate ~ 
                collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density  +
                street_lighting_mean + public_housing_n_mean + mixed_land_use_mean + vacant_pct_mean + public_facilities_mean + police_fire_mean | TRACT_1980,
              family = "gaussian",
              data = be_panel_analytical))