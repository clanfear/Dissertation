library(tidyverse)
library(lme4)
library(modelsummary)
library(broom.mixed)
library(corrplot)
source("./syntax/project_functions.R")
# TRACT LEVEL

load("./data/seattle/derived/be_panel_long.RData")

be_panel_analytical <- be_panel_long %>%
  mutate(across(-TRACT_1980, ~ standardize(.)))

be_panel_analytical %>% select(-TIME, -TRACT_1980, -PERC_EFF_government, -property_victimization, -violent_victimization, -violent_crime_count) %>% cor() %>% corrplot::corrplot()

summary(lmer(violent_crime_rate ~ 
               collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + TIME + (1|TRACT_1980),
     data = be_panel_analytical))

summary(lmer(violent_crime_rate ~ 
       collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + (1|TRACT_1980) +
       arterial_roads_mean + street_lighting_mean + public_housing_n_mean + mixed_land_use_mean + vacant_pct_mean + public_facilities_mean + police_fire_mean +
         BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks,
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

# SEGMENT LEVEL
load("./data/seattle/derived/be_panel_segments_long.RData")


be_panel_segments_analytical <- be_panel_segments_long %>%
 mutate(across(-c(TRACT_1980, crime_count, STREET_ID, TIME, arterial_roads), ~ standardize(.)))


library(mgcv)

gam_segment_sd <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + arterial_roads +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
# summary(gam_segment_sd)

gam_segment_be <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
# summary(gam_segment_be)

gam_segment_be_survbe <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
# summary(gam_segment_be_no_arterial)

lapply(list(gam_segment_sd, gam_segment_be, gam_segment_be_survbe), broom:::tidy.gam, parametric=TRUE)

gam_segment_be_interactions <- gam(
  crime_count ~ 
    collective_efficacy*street_lighting + concentrated_disadvantage*street_lighting + ethnicity_immigration + residential_stability + density*street_lighting + TIME + 
    arterial_roads + mixed_land_use + arterial_roads*street_lighting + public_housing_n + mixed_land_use*vacant_pct + public_facilities + police_fire +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
summary(gam_segment_be_interactions)


femlm_segment_sd <- femlm(crime_count ~ 
                            collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME  | STREET_ID, 
                          data = be_panel_segments_analytical, family = "negbin")
summary(femlm_segment_sd)

femlm_segment_be <- femlm(crime_count ~ 
                                 collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
                                  vacant_pct + public_housing_n + street_lighting | STREET_ID, 
                               data = be_panel_segments_analytical, family = "negbin")
summary(femlm_segment_be)

femlm_segment_be_survbe <- femlm(crime_count ~ 
                            collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
                            vacant_pct + public_housing_n + street_lighting +
                              BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks | STREET_ID, 
                          data = be_panel_segments_analytical, family = "negbin")
summary(femlm_segment_be_survbe)

lapply(list(femlm_segment_sd, femlm_segment_be, femlm_segment_be_survbe), broom::tidy, parametric=TRUE)

#----
# SPATIAL
load("./data/seattle/derived/seattle_vor_df.RData")
street_subset  <- seattle_vor_df %>%
  filter(STREET_ID %in% be_panel_segments_analytical$STREET_ID)

seattle_weisb_neighbors <- street_subset %>%
  spdep::poly2nb()
attr(seattle_weisb_neighbors, "region.id") <- street_subset$STREET_ID
names(seattle_weisb_neighbors) <- street_subset$STREET_ID
be_panel_segments_analytical <- be_panel_segments_analytical %>% mutate(STREET_ID = factor(STREET_ID))

gam_segment_be <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)

gam_segment_be_mrf <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks +
    s(STREET_ID, bs = 're') + s(STREET_ID, bs = "mrf", xt = list(nb = seattle_weisb_neighbors), k = 10),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)

# be_panel_segments_analytical_centered <- be_panel_segments_analytical %>%
#   group_by(STREET_ID) %>%
#   mutate(across(-c(TRACT_1980, TIME), ~ mean(.), .names = "{.col}_sm")) %>%
#   mutate(across(-c(TRACT_1980, TIME), ~ (. - mean(.)), .names = "{.col}_centered"))
# 
# gam_segment_be_centered <- gam(
#   crime_count ~ 
#     collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
#     arterial_roads + street_lighting_centered + public_housing_n_centered + vacant_pct_centered + public_facilities_centered + police_fire_centered +
#     street_lighting_sm + public_housing_n_sm + vacant_pct_sm + public_facilities_sm + police_fire_sm +
#     s(STREET_ID, bs = 're'),
#   data = be_panel_segments_analytical_centered, family = "nb",
#   method = 'REML'
# )
# summary(gam_segment_be_centered)
