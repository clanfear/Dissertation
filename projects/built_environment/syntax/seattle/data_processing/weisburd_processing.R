library(tidyverse)
library(sf)
library(ggplot2)

drive_letter    <- "F:/"
weisburd_dir    <- "SecureData/Seattle_Explaining_Developmental_Crime_Trajectories_at_Places_ICPSR_28161/"
aspatial_file   <- "DS0001/28161-0001-Data-REST.sav"
# centerline_file <- "DS0002/28161-0002-Zipped_package.zip" 
# midpoint_file   <- "DS0003/28161-0003-Zipped_package.zip"
# unzip(paste0(drive_letter, weisburd_dir, centerline_file), exdir = paste0(drive_letter, weisburd_dir, "DS0002/unzipped"))
# unzip(paste0(drive_letter, weisburd_dir, midpoint_file), exdir = paste0(drive_letter, weisburd_dir, "DS0003/unzipped"))
centerline_shapefile <- "DS0002/unzipped/UofA_lines.shp"
midpoint_shapefile   <- "DS0003/unzipped/UofA_pts.shp"

aspatial_data   <- haven::read_sav(paste0(drive_letter, weisburd_dir, aspatial_file))
centerline_data <- read_sf(paste0(drive_letter, weisburd_dir, centerline_shapefile))
midpoint_data   <- read_sf(paste0(drive_letter, weisburd_dir, midpoint_shapefile))

street_segments <- centerline_data %>%
    select(STNAME, STREET_ID = Street_Id) %>%
    inner_join(aspatial_data, by="STREET_ID")

street_points <- midpoint_data %>%
  select(STREET_ID) %>%
  inner_join(aspatial_data, by="STREET_ID")

#---
king_county <- tigris::tracts("WA", "King", class = "sf")
seattle_buffer <- king_county %>% mutate(tract = as.numeric(TRACTCE)) %>%
  filter(tract < 15000) %>%
  st_transform(st_crs(street_points)) %>% 
  st_union() %>% st_combine() %>%
  st_buffer(dist = units::set_units(500, m))


# glimpse(seattle_snd)
# glimpse(seattle_int)
# ggplot(seattle_snd, aes(color = ARTERIAL_C)) + geom_sf()

seattle_vor_df <-
  street_points %>% 
  select(geometry) %>% 
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  data.frame(index = seq_along(.), 
             geometry = .) %>% 
  st_as_sf() %>%
  st_intersection(seattle_buffer) %>%
  mutate(area = st_area(.)) %>%
  st_join(street_points %>% select(STREET_ID, geometry))


save(seattle_vor_df, file = "./data/seattle/derived/seattle_vor_df.RData")

#---

# data_labels <- lapply(aspatial_data, function(x) attr(x, "label"))
# tibble(varname=names(data_labels), label=unlist(data_labels)) %>% print(n=200)

# tract_boundaries_1990 <- read_sf("C:/Users/cclan/Dropbox/Dissertation/data/census_data/1990/1990_tract_boundaries/tr53_d90.shp") %>%
#   filter(CO=="033") %>%
#   select(TRACT_NAME)
# plot(tract_boundaries_1990)
# 
# tract_boundaries_2000 <- read_sf("C:/Users/cclan/Dropbox/Dissertation/data/census_data/2000/2000_tract_boundaries/tr53_d00.shp") %>%
#   filter(COUNTY=="033") %>%
#   select(TRACT_NAME=NAME)
# plot(tract_boundaries_2000)
tract_boundaries_1980 <- st_read("D:/Projects/dissertation_data/seattle/tracts_1980/Census_Tracts_1980.shp") %>%
  select(TRACT_1980 = TRACT80, geometry) %>%
  mutate(TRACT_1980 = as.numeric(TRACT_1980)) %>%
  st_transform(3689)

# tract_boundaries_1990 <- tigris::tracts("53", "033", year = 1990, cb = TRUE) %>% 
#   st_as_sf() %>% 
#   mutate(TRACT = paste0(TRACTBASE, TRACTSUF)) %>% 
#   select(TRACT = TRACT, geometry) %>%
#   mutate(year = 1990) %>%
#   mutate(TRACT = as.numeric(TRACT)) %>%
#   filter(TRACT < 20000)
# 
# tract_boundaries_2000 <- tigris::tracts("53", "033", year = 2000, cb = TRUE) %>% 
#   st_as_sf() %>% 
#   select(TRACT = TRACT, geometry) %>%
#   mutate(year = 2000) %>%
#   mutate(TRACT = as.numeric(TRACT)) %>%
#   filter(TRACT < 20000)

# bind_rows(tract_boundaries_1980, tract_boundaries_1990, tract_boundaries_2000) %>% ggplot() + facet_wrap(~year) + geom_sf() + geom_sf_text(aes(label = TRACT))
# Assign street segments to 1990 and 2000 tracts

street_points_tracts <- street_points %>%
  st_transform(3689) %>%
  st_join(tract_boundaries_1980, join = st_nearest_feature) %>%
  st_drop_geometry()

weisburd_points_90_00 <- street_points_tracts %>%
  select(TRACT_1980, STREET_ID,
         active_voters_pct_end   = ACTVOT_PC_E,
         retail_sales            = TOT_SAL98_100,
         bus_stops               = BUSSTP_B_3,
         employees_count         = EMP_B,
         residential_pop         = RES_B,
         arterial_roads          = ARTERIAL,
         segment_length          = LENGTH_100,
         vacant_pct_91           = VACIND_B,
         vacant_pct_04           = VACIND_E,
         high_risk_juv_92_94     = JUVD_B_3,
         high_risk_juv_02_04     = JUVD_E_3,
         truant_students_93_95   = TRNT_B3,
         truant_students_02_04   = TRNT_E3,
         phys_dis_n_93_95        = P_DISORDER_B,
         phys_dis_n_02_04        = P_DISORDER_E,
         public_housing_n_89_91  = PHSE_B3,
         public_housing_n_02_04  = PHSE_E3,
         mixed_land_use_91       = MIXED_B,
         mixed_land_use_04       = MIXED_E,
         property_value_91       = PROPV_B,
         property_value_04       = PROPV_E,
         crimert_avg_89_91       = CRIME_89_91,
         crimert_avg_02_04       = CRIME_02_04,
         street_lighting_97_99   = LGHT_B_3,
         street_lighting_02_04   = LGHT_E_3,
         police_fire_89_90       = FIREPOL_1320_CT_B,
         police_fire_02_03       = FIREPOL_1320_CT_E,
         public_facilities_89_90 = NOBUS_1320_CT_B,
         public_facilities_02_03 = NOBUS_1320_CT_E,
         lag_crimert_avg_89_91   = CR1_LAG,
         lag_crimert_avg_02_04   = CR2_LAG,
         CNT89, CNT90, CNT91, CNT02, CNT03, CNT04) %>%
  mutate(TRACT_1980 = as.character(TRACT_1980)) %>%
  mutate(crime_count_89_91 = CNT89 + CNT90 + CNT91,
         crime_count_02_04 = CNT02 + CNT03 + CNT04) %>%
  select(-starts_with("CNT"))

save(weisburd_points_90_00, file = "./data/derived/weisburd_points_90_00.RData")

# Note one might summarize differently than means
## Might make more sense to do sums of crime events year-by-year?

weisburd_tract_90_00 <- weisburd_points_90_00 %>%
  group_by(TRACT_1980) %>%
  summarize(across(c(street_lighting_97_99, street_lighting_02_04, 
                     public_housing_n_89_91, public_housing_n_02_04,
                     crimert_avg_89_91, crimert_avg_02_04, 
                     mixed_land_use_91, mixed_land_use_04, 
                     vacant_pct_91, vacant_pct_04,
                     property_value_91, property_value_04,
                     public_facilities_89_90, public_facilities_02_03,
                     police_fire_89_90, police_fire_02_03,
                     arterial_roads) , ~ mean(., na.rm=T), .names = "{.col}_mean"),
            across(c(public_housing_n_89_91, public_housing_n_02_04, 
                     arterial_roads, 
                     ) , ~ sum(., na.rm=T), .names = "{.col}_count")) %>%
  mutate(TRACT_1980 = as.character(TRACT_1980))

weisburd_tract_90_00 <- weisburd_tract_90_00 %>% mutate(across(-TRACT_1980, ~ standardize(.)))
# Output
save(weisburd_tract_90_00, file = "./data/derived/weisburd_tract_90_00.RData")
