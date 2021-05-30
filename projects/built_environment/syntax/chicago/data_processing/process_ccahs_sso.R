library(tidyverse)


ccahs_dir    <- "F:/SecureData/CCAHS/"

# Note CCAHS has a .do file to change missings (e.g. -2, -5) to .
ccahs_individual_raw <- haven::read_dta(paste0(ccahs_dir, "DS0001/31142-0001-Data-REST.dta"))
ccahs_sso_raw <- haven::read_dta(paste0(ccahs_dir, "DS0002/31142-0002-Data-REST.dta"))

ccahs_sso <- ccahs_sso_raw %>%
  select(CMBID,
         FACE_ID,
         STREET_ID     = STRT_ID,
         BLOCK_ID,
         BLOCKGROUP_ID = GROUP_ID,
         TRACT_ID,
         NC_ID,
         LAND_USE_7_CAT          = S4110,
         # BE_eb_bars_liquor_block = ESB7040, # Bar/liquor store empirical bayes scale; this did not converge at block level
         # BE_eb_bars_liquor_bg    = ESG7040,
         # BE_eb_bars_liquor_tract = EST7040,
         # BE_eb_bars_liquor_nc    = ESC7040,
         # BE_eb_alc_avail_block   = ESB7010, # availability is the above plus advertising for alcohol
         # BE_eb_alc_avail_bg      = ESG7010,
         # BE_eb_alc_avail_tract   = EST7010,
         # BE_eb_alc_avail_nc      = ESC7010,
         # NC pr
         # BE_pr_residential_nc = PFC1001,
         # BE_pr_commercial_nc  = PFC1002,
         # BE_pr_industrial_nc  = PFC1003,
         # BE_pr_parking_nc     = PFC1004,
         # BE_pr_vacant_lot_nc     = PFC1005,
         # BE_pr_institution_nc = PFC1006,
         # BE_pr_recreation_nc  = PFC1007,
         # # Tract
         # BE_pr_residential_tract = PFT1001,
         # BE_pr_commercial_tract  = PFT1002,
         # BE_pr_industrial_tract  = PFT1003,
         # BE_pr_parking_tract     = PFT1004,
         # BE_pr_vacant_lot_tract  = PFT1005,
         # BE_pr_institution_tract = PFT1006,
         # BE_pr_recreation_tract  = PFT1007,
         # # BG
         # BE_pr_residential_bg = PFG1001,
         # BE_pr_commercial_bg  = PFG1002,
         # BE_pr_industrial_bg  = PFG1003,
         # BE_pr_parking_bg     = PFG1004,
         # BE_pr_vacant_lot_bg  = PFG1005,
         # BE_pr_institution_bg = PFG1006,
         # BE_pr_recreation_bg  = PFG1007,  
         # Block
         BE_pr_residential_block            = PFB1001, # pr face in block with residential use
         BE_pr_commercial_block             = PFB1002, # pr face in block with commercial/professional use
         BE_pr_industrial_block             = PFB1003, # pr face in block with industrial/warehouse use
         BE_pr_parking_block                = PFB1004, # pr face in block with parking lots
         BE_pr_vacant_lot_block             = PFB1005, # pr face in block with vacant lots or open space
         BE_pr_institution_block            = PFB1006, # pr face in block with institutional land use
         BE_pr_recreation_block             = PFB1007, # pr face in block with recreational land use
         # On streets
         # BE_pr_vacant_onstreet_nc           = PSC0618, 
         # BE_pr_commer_dest_onstreet_nc      = PSC4031,
         # BE_pr_abandoned_bld_onstreet_nc    = PSC0617,
         # BE_pr_vacant_onstreet_tract        = PST0618,
         # BE_pr_commer_dest_onstreet_tract   = PST4031,
         # BE_pr_abandoned_bld_onstreet_tract = PST0617,
         BE_pr_abandoned_bld_onstreet_block = PSB0617, # pr of streets in block with abandoned/burned out/boarded up building
         BE_pr_vacant_onstreet_block        = PSB0618, # pr of streets in block with vacant but usable house/building
         BE_pr_commer_dest_onstreet_block   = PSB4031, # pr of streets in block with commercial destination
         BE_pr_liquor_onstreet_block        = PSB0615, # pr of streets in block with liquor store
         BE_pr_bar_onstreet_block           = PSB0614) # pr of streets in block with bar/cocktail lounge

ccahs_sso_block <- ccahs_sso %>%
  mutate(NC_ID = as.character(NC_ID),
         BLOCK_ID = as.character(BLOCK_ID),
         TRACT_ID = as.character(TRACT_ID)) %>%
  select(NC_ID, BLOCK_ID, TRACT_ID, matches("_block|_face"), LAND_USE_7_CAT) %>%
  mutate(MIXED_LAND_USE = ifelse(LAND_USE_7_CAT %in% c(2,4), 1, 0)) %>%
  select(-LAND_USE_7_CAT) %>%
  group_by(NC_ID, TRACT_ID, BLOCK_ID) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE))) %>%
  ungroup()

ccahs_sso_block %>% select(where(is.numeric)) %>% cor()

save(ccahs_sso_block, file = "./data/chicago/derived/ccahs_sso_block.RData")

# This file is large
# ccahs_sso <- haven::read_dta(paste0(ccahs_dir, "DS0002/31142-0002-Data-REST.dta"))

# There is a third file which is imputation file?
# Appears to be 5 rows per individual of multiple imputation data
# ccahs_impute <- haven::read_dta(paste0(ccahs_dir, "DS0003/31142-0003-Data-REST.dta"))

# ccahs_sso
## Has GROUP_ID (census block group), TRACT_ID, NC_ID (cluster)
## NOTE: Codebook for SSO data is excellent; see the end of PDF in the dir
## Already have summaries of variables from block-face to cluster level
## Codebook seems to note many scales have convergence issues and there may be some 
## data fixes required before some measures can be used (e.g. physcn)