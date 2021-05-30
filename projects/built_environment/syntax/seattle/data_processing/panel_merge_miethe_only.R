library(tidyverse)
load("./data/derived/sncs_neighb_eb_miethe_subsample.RData")
load("./data/derived/miethe_neighb_eb.RData")
load("./data/derived/weisburd_tract_90_00.RData")
load("./data/derived/weisburd_points_90_00.RData")
load("./data/derived/nbhd_1990.RData")

# TRACT LEVEL

be_panel_wide_miethe_only <- miethe_neighb_eb %>%
  left_join(nbhd_1990) %>%
  left_join(sncs_neighb_eb_miethe_subsample) %>%
  left_join(weisburd_tract_90_00)

save(be_panel_wide_miethe_only, file = "./data/derived/be_panel_wide_miethe_only.RData")

be_panel_long_miethe_only <- 
  be_panel_wide_miethe_only %>%
  select(TRACT_1980,
         collective_efficacy_02_03, 
         collective_efficacy_90 = eb_soccap_90,
         PERC_EFF_government_00,
         concentrated_disadvantage_00, concentrated_disadvantage_90,
         ethnicity_immigration_00, ethnicity_immigration_90,
         residential_stability_00, residential_stability_90,
         density_00, density_90,
         VICT_viol_2_90, VICT_viol_2_00,
         VICT_prop_2_90, VICT_prop_2_00,
         violent_crime_rate_03_05, violent_crime_rate_90,
         violent_crime_count_03_05,
         property_victimization_02_03,
         violent_victimization_02_03,
         street_lighting_97_99_mean, street_lighting_02_04_mean,
         public_housing_n_89_91_mean, public_housing_n_02_04_mean,
         mixed_land_use_91_mean, mixed_land_use_04_mean,
         vacant_pct_91_mean, vacant_pct_04_mean,
         public_facilities_89_90_mean, public_facilities_02_03_mean,
         police_fire_89_90_mean, police_fire_02_03_mean,
         arterial_roads_mean, arterial_roads_count,
         public_housing_n_89_91_count, public_housing_n_02_04_count,
         BE_school_3_blocks_90, BE_school_3_blocks_00,
         BE_bar_3_blocks_90, BE_bar_3_blocks_00,
         BE_park_3_blocks_90, BE_park_3_blocks_00,
         BE_shopping_3_blocks_90, BE_shopping_3_blocks_00,
         BE_hotel_3_blocks_90, BE_hotel_3_blocks_00,
         BE_busstop_3_blocks_90, BE_busstop_3_blocks_00
         ) %>%
    pivot_longer(-c(TRACT_1980, arterial_roads_mean, arterial_roads_count)) %>%
    mutate(TIME = ifelse(str_detect(name, "00|02|03|04"), 2, 1)) %>%
    mutate(name = str_remove_all(name, "_([0-9][0-9])")) %>%
    pivot_wider()

save(be_panel_long_miethe_only, file = "./data/derived/be_panel_long_miethe_only.RData")

# STREET SEGMENT LEVEL

be_panel_segments_wide_miethe_only <- miethe_neighb_eb %>%
  left_join(nbhd_1990) %>%
  left_join(sncs_neighb_eb_miethe_subsample) %>%
  left_join(weisburd_points_90_00)

save(be_panel_segments_wide_miethe_only, file = "./data/derived/be_panel_segments_wide_miethe_only.RData")

be_panel_segments_long_miethe_only <- 
  be_panel_segments_wide_miethe_only %>%
  mutate(crime_count_89_91 = CNT89 + CNT90 + CNT91,
         crime_count_02_04 = CNT02 + CNT03 + CNT04) %>%
  select(TRACT_1980, 
         STREET_ID, 
         segment_length,
         arterial_roads,
         residential_pop,
         employees_count,
         bus_stops,
         retail_sales,
         active_voters_pct_end,
         collective_efficacy_90 = eb_soccap_90, collective_efficacy_02_03,    
         concentrated_disadvantage_90, concentrated_disadvantage_00,
         ethnicity_immigration_90,     ethnicity_immigration_00,     
         residential_stability_90,     residential_stability_00,
         density_90,                   density_00,
         BE_school_3_blocks_90,        BE_school_3_blocks_00,
         BE_bar_3_blocks_90,           BE_bar_3_blocks_00,
         BE_park_3_blocks_90,          BE_park_3_blocks_00,
         BE_shopping_3_blocks_90,      BE_shopping_3_blocks_00,
         BE_hotel_3_blocks_90,         BE_hotel_3_blocks_00,
         BE_busstop_3_blocks_90,       BE_busstop_3_blocks_00,
         VICT_viol_2_90,               VICT_viol_2_00,
         VICT_prop_2_90,               VICT_prop_2_00,
         violent_crime_rate_90,        violent_crime_rate_03_05,
         crime_count_89_91,            crime_count_02_04,
         crimert_avg_89_91,            crimert_avg_02_04,
         street_lighting_97_99,        street_lighting_02_04,
         public_housing_n_89_91,       public_housing_n_02_04,
         mixed_land_use_91,            mixed_land_use_04,
         vacant_pct_91,                vacant_pct_04,
         public_facilities_89_90,      public_facilities_02_03,
         police_fire_89_90,            police_fire_02_03,
         property_value_91,            property_value_04,
         lag_crimert_avg_89_91,        lag_crimert_avg_02_04
         
  ) %>%
  pivot_longer(-c(TRACT_1980, STREET_ID, arterial_roads, segment_length, residential_pop, residential_pop, employees_count, bus_stops, retail_sales, active_voters_pct_end)) %>%
  mutate(TIME = ifelse(str_detect(name, "00|02|03|04"), 2, 1)) %>%
  mutate(name = str_remove_all(name, "_([0-9][0-9])")) %>%
  pivot_wider()

save(be_panel_segments_long_miethe_only, file = "./data/derived/be_panel_segments_long_miethe_only.RData")
