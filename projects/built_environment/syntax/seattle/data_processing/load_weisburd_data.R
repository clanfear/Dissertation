library(dplyr)
library(sf)
library(haven)
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

tract_boundaries_1990 <- tigris::tracts("53", "033", year = 1990, cb = TRUE) %>% st_as_sf() %>% mutate(TRACT = paste0(TRACTBASE, TRACTSUF)) %>% select(TRACT_AREA_1990 = AREA, TRACT_1990 = TRACT)
tract_boundaries_2000 <- tigris::tracts("53", "033", year = 2000, cb = TRUE) %>% st_as_sf() %>% select(TRACT_AREA_2000 = AREA, TRACT_2000 = TRACT)

# Assign street segments to 1990 and 2000 tracts

street_points_tracts <- street_points %>%
  st_transform(st_crs(tract_boundaries_1990)) %>%
  st_join(tract_boundaries_1990) %>%
  st_join(tract_boundaries_2000)

street_points_tracts_subset <- street_points_tracts %>%
  select(TRACT_AREA_2000, TRACT_AREA_1990, 
         TRACT_2000, TRACT_1990,
         arterial_roads = ARTERIAL,
         vacant_pct_91 = VACIND_B,
         vacant_pct_04 = VACIND_E,
         high_risk_juv_92_94 = JUVD_B_3,
         high_risk_juv_02_04 = JUVD_E_3,
         truant_students_93_95 = TRNT_B3,
         truant_students_02_04 = TRNT_E3,
         phys_dis_n_93_95 = P_DISORDER_B,
         phys_dis_n_02_04 = P_DISORDER_E,
         public_housing_n_89_91 = PHSE_B3,
         public_housing_n_02_04 = PHSE_E3,
         mixed_land_use_91 = MIXED_B,
         mixed_land_use_04 = MIXED_E,
         property_value_91 = PROPV_B,
         property_value_04 = PROPV_E,
         crimert_avg_89_91 = CRIME_89_91,
         crimert_avg_02_04 = CRIME_02_04,
         street_lighting_97_99 = LGHT_B_3,
         street_lighting_02_04 = LGHT_E_3,
         police_fire_89_90 = FIREPOL_1320_CT_B,
         police_fire_02_03 = FIREPOL_1320_CT_E,
         public_facilities_89_90 = NOBUS_1320_CT_B,
         public_facilities_02_03 = NOBUS_1320_CT_E,
         starts_with("CNT"))

# Note one might summarize differently than means
## Might make more sense to do sums of crime events year-by-year?

street_points_nb_1990 <- street_points_tracts_subset %>%
  st_drop_geometry() %>%
  select(-TRACT_AREA_2000, - TRACT_2000) %>%
  filter(!is.na(TRACT_1990)) %>%
  group_by(TRACT_1990) %>%
  summarize_all( ~ mean(., na.rm=T))

street_points_nb_2000 <- street_points_tracts_subset %>%
  st_drop_geometry() %>%
  select(-TRACT_AREA_1990, - TRACT_1990) %>%
  filter(!is.na(TRACT_2000)) %>%
  group_by(TRACT_2000) %>%
  summarize_all( ~ mean(., na.rm=T))


# Output
output_dir <- paste0(drive_letter, "SecureData/derived_data/weisburd/")
save(street_points_tracts, file = paste0(output_dir, "street_points_tracts.RData"))
save(street_points_nb_1990, file = paste0(output_dir, "street_points_nb_1990.RData"))
save(street_points_nb_2000, file = paste0(output_dir, "street_points_nb_2000.RData"))