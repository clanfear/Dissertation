library(tidyverse)
source("../shared/syntax/project_functions.R")

load("./data/chicago/derived/ccahs_block_sso_crime.RData")
load("../shared/data/chicago/derived/CE_PE_LC_PV_eb_nc_1995.RData")
load("../shared/data/chicago/derived/CE_PE_LC_PV_eb_nc_2001.RData")
load("../shared/data/chicago/derived/ltdb_factors_wide.RData")

# Should look at other bar / liquor store measures

ccahs_block_analytical_unstd <- ccahs_block_sso_crime %>%
  mutate(BE_pr_max_bars_liquor_onstreet_block_2001 = 
           ifelse(BE_pr_bar_onstreet_block_2001 > BE_pr_liquor_onstreet_block_2001, 
                  BE_pr_bar_onstreet_block_2001, BE_pr_liquor_onstreet_block_2001)) %>%
  left_join(CE_PE_LC_PV_eb_nc_1995, by = "phdcn_nc") %>%
  left_join(CE_PE_LC_PV_eb_nc_2001, by = "ccahs_nc") %>%
  left_join(ltdb_factors_wide, by = c("phdcn_nc", "ccahs_nc"))  %>%
  select(CRIME_homicide_2004_2006,
         CRIME_assault_battery_gun_2004_2006,
         CRIME_robbery_2004_2006,
         CRIME_violent_2004_2006,
         CRIME_property_2004_2006,
         CE_hlm_1995,
         CE_hlm_2001,
         FAC_disadv_2000,
         FAC_stability_2000, 
         FAC_hispimm_2000, 
         density_ltdb_nc_2000,
         BE_pr_bar_onstreet_block_2001,
         BE_pr_liquor_onstreet_block_2001,
         BE_pr_vacant_onstreet_block_2001,
         BE_pr_abandoned_bld_onstreet_block_2001,
         BE_pr_commer_dest_onstreet_block_2001,
         BE_pr_recreation_block_2001,
         BE_pr_parking_block_2001,
         MIXED_LAND_USE_2001,
         CE_hlm_2001,
         orig_density_block = density_block,
         ccahs_nc,
         census_block,
         ccahs_tract) %>%
  mutate(FAC_disadv_2000_2 = FAC_disadv_2000^2,
         density_block     = poly(orig_density_block, 2)[,1],
         density_block_2   = poly(orig_density_block, 2)[,2])

save(ccahs_block_analytical_unstd, file = "./data/chicago/derived/ccahs_block_analytical_unstd.RData")

ccahs_block_analytical <- ccahs_block_analytical_unstd %>%
  mutate(across(-matches("ccahs_nc|CRIME|census_block|ccahs_tract"), ~ standardize(., scale = 1))) 

save(ccahs_block_analytical, file = "./data/chicago/derived/ccahs_block_analytical.RData")


