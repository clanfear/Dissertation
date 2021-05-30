library(tidyverse)
library(mgcv)
library(lme4)
library(broom.mixed)
source("./syntax/project_functions.R")
load("./data/seattle/derived/be_panel_segments_long.RData")

be_crosssection_segments_analytical <- be_panel_segments_long %>%
  mutate(across(-c(TRACT_1980, crime_count, STREET_ID, TIME, arterial_roads ), ~ standardize(.))) %>%
  mutate(across(c(STREET_ID, TRACT_1980), ~factor(.))) %>%
  filter(TIME==2)

time_start <- Sys.time()
glmer_segment_sd <- glmer.nb(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + arterial_roads +
    (1|TRACT_1980),
  data = be_crosssection_segments_analytical, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

time_end <- Sys.time()
time_end - time_start

time_start <- Sys.time()
glmer_segment_be <- glmer.nb(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + arterial_roads +
    arterial_roads  + vacant_pct + (1|TRACT_1980),
  data = be_crosssection_segments_analytical, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

time_end <- Sys.time()
time_end - time_start

# summary(gam_segment_be)
time_start <- Sys.time()
glmer_segment_be_survbe <- glmer.nb(
  crime_count ~ 
    collective_efficacy + concentrated_disadvantage + ethnicity_immigration + residential_stability + density + arterial_roads +
    arterial_roads + vacant_pct + (1|TRACT_1980) +
    BE_bar_3_blocks + mixed_land_use,
  data = be_crosssection_segments_analytical, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=4e5))
)
time_end - time_start

be_crosssection_segments_analytical %>% 
  select(collective_efficacy, concentrated_disadvantage, ethnicity_immigration, residential_stability, density, arterial_roads,
         arterial_roads, vacant_pct,
         BE_school_3_blocks, BE_bar_3_blocks, BE_park_3_blocks, BE_shopping_3_blocks, BE_hotel_3_blocks) %>% summary()
# summary(gam_segment_be_no_arterial)
time_end <- Sys.time()
time_end - time_start
lapply(list(glmer_segment_sd, glmer_segment_be, glmer_segment_be_survbe), broom:::tidy.gam, parametric=TRUE)