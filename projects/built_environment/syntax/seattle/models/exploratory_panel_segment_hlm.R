library(tidyverse)
library(lme4)
library(modelsummary)
library(broom.mixed)
library(corrplot)
library(mgcv)
library(fixest)

source("./syntax/project_functions.R")
# TRACT LEVEL

# SEGMENT LEVEL
load("./data/derived/be_panel_segments_long.RData")

be_panel_segments_analytical <- be_panel_segments_long %>% mutate(TIME = factor(ifelse(TIME==2, "2003", "1990")))


gam_segment_sd <- gam(
  crime_count ~ 
    concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
# summary(gam_segment_sd)

gam_segment_be <- gam(
  crime_count ~ 
    concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire + 
    retail_sales + residential_pop + bus_stops + employees_count +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)
# summary(gam_segment_be)

gam_segment_surv <- gam(
  crime_count ~ 
    concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks + PHYDIS_vacant +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)

gam_segment_be_surv <- gam(
  crime_count ~ 
    concentrated_disadvantage + ethnicity_immigration + residential_stability + density + TIME + 
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire + 
    retail_sales + residential_pop + bus_stops + employees_count +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks + PHYDIS_vacant +
    s(STREET_ID, bs = 're'),
  data = be_panel_segments_analytical, family = "nb",
  method = 'REML'
)

# summary(gam_segment_be_no_arterial)

output_list <- lapply(list(gam_segment_sd, gam_segment_be, gam_segment_surv, gam_segment_be_surv), function(x) broom:::tidy.gam(x, parametric=TRUE) %>% select(term, estimate))

output_list[[1]] %>% 
  rename(estimate_1 = estimate) %>% 
  full_join(output_list[[2]], by = "term") %>%
  rename(estimate_2 = estimate) %>% 
  full_join(output_list[[3]], by = "term") %>%
  rename(estimate_3 = estimate) %>% 
  full_join(output_list[[4]], by = "term") %>%
  rename(estimate_4 = estimate) %>% print(n=100)

###
# CROSS SECTION
be_panel_segments_analytical_2003 <- be_panel_segments_analytical %>% filter(TIME=="2003") %>% mutate(TRACT_1980 = as.numeric(TRACT_1980))

gam_segment_cs_1 <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + s(TRACT_1980, bs = 're'),
  data = be_panel_segments_analytical_2003, family = "nb",
  method = 'REML'
)

gam_segment_cs_2 <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks + PHYDIS_vacant + s(TRACT_1980, bs = 're'),
  data = be_panel_segments_analytical_2003, family = "nb",
  method = 'REML'
)

gam_segment_cs_3 <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density +
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire + 
    retail_sales + residential_pop + bus_stops + employees_count + s(TRACT_1980, bs = 're'),
  data = be_panel_segments_analytical_2003, family = "nb",
  method = 'REML'
)


gam_segment_cs_4 <- gam(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density +
    arterial_roads + street_lighting + public_housing_n + vacant_pct + public_facilities + police_fire + 
    retail_sales + residential_pop + bus_stops + employees_count +
    BE_school_3_blocks + BE_bar_3_blocks + BE_park_3_blocks + BE_shopping_3_blocks + BE_hotel_3_blocks + PHYDIS_vacant + s(TRACT_1980, bs = 're'),
  data = be_panel_segments_analytical_2003, family = "nb",
  method = 'REML'
)

# lapply(list(gam_segment_cs_1, gam_segment_cs_2, gam_segment_cs_3), function(x) print(broom:::tidy.gam(x, parametric=TRUE), n=50))[[3]] %>% print(n=50)
 
output_list <- lapply(list(gam_segment_cs_1, gam_segment_cs_2, gam_segment_cs_3, gam_segment_cs_4), function(x) broom:::tidy.gam(x, parametric=TRUE) %>% select(term, estimate))

output_list[[1]] %>% 
  rename(estimate_1 = estimate) %>% 
  full_join(output_list[[2]], by = "term") %>%
  rename(estimate_2 = estimate) %>% 
  full_join(output_list[[3]], by = "term") %>%
  rename(estimate_3 = estimate) %>% 
  full_join(output_list[[4]], by = "term") %>%
  rename(estimate_4 = estimate) %>% print(n=100)

###


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
