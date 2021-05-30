library(tidyverse)


ccahs_dir    <- "F:/SecureData/CCAHS/"

# Note CCAHS has a .do file to change missings (e.g. -2, -5) to .
ccahs_individual_raw <- haven::read_dta(paste0(ccahs_dir, "DS0001/31142-0001-Data-REST.dta"))
ccahs_sso_raw <- haven::read_dta(paste0(ccahs_dir, "DS0002/31142-0002-Data-REST.dta"))

ccahs_sso <- ccahs_sso_raw %>%
  select(CMBID,
         FACE_ID,
         STREET_ID = STRT_ID,
         BLOCK_ID,
         BLOCKGROUP_ID = GROUP_ID,
         TRACT_ID,
         NC_ID,
         LAND_USE_7_CAT        = S4110,
         BE_eb_bars_liquor_block = ESB7040, # Need in PHDCN
         BE_eb_bars_liquor_bg = ESG7040,
         BE_eb_bars_liquor_tract = EST7040,
         BE_eb_bars_liquor_nc = ESC7040,
         BE_eb_alc_avail_block = ESB7010,
         BE_eb_alc_avail_bg = ESG7010,
         BE_eb_alc_avail_tract = EST7010,
         BE_eb_alc_avail_nc = ESC7010,
         # NC pr
         BE_pr_residential_nc = PFC1001,
         BE_pr_commercial_nc  = PFC1002,
         BE_pr_industrial_nc  = PFC1003,
         BE_pr_parking_nc     = PFC1004,
         BE_pr_vacant_lot_nc     = PFC1005,
         BE_pr_institution_nc = PFC1006,
         BE_pr_recreation_nc  = PFC1007,
         # Tract
         BE_pr_residential_tract = PFT1001,
         BE_pr_commercial_tract  = PFT1002,
         BE_pr_industrial_tract  = PFT1003,
         BE_pr_parking_tract     = PFT1004,
         BE_pr_vacant_lot_tract  = PFT1005,
         BE_pr_institution_tract = PFT1006,
         BE_pr_recreation_tract  = PFT1007,
         # BG
         BE_pr_residential_bg = PFG1001,
         BE_pr_commercial_bg  = PFG1002,
         BE_pr_industrial_bg  = PFG1003,
         BE_pr_parking_bg     = PFG1004,
         BE_pr_vacant_lot_bg  = PFG1005,
         BE_pr_institution_bg = PFG1006,
         BE_pr_recreation_bg  = PFG1007,  
         # Block
         BE_pr_residential_block = PFB1001,
         BE_pr_commercial_block  = PFB1002,
         BE_pr_industrial_block  = PFB1003,
         BE_pr_parking_block     = PFB1004,
         BE_pr_vacant_lot_block  = PFB1005,
         BE_pr_institution_block = PFB1006,
         BE_pr_recreation_block  = PFB1007, 
         # On streets
         BE_pr_vacant_onstreet_nc = PSC0618,
         BE_pr_commer_dest_onstreet_nc = PSC4031,
         BE_pr_abandoned_bld_onstreet_nc = PSC0617,
         BE_pr_vacant_onstreet_tract     = PST0618,
         BE_pr_commer_dest_onstreet_tract = PST4031,
         BE_pr_abandoned_bld_onstreet_tract = PST0617,
         BE_pr_abandoned_bld_onstreet_block = PSB0617,
         BE_pr_vacant_onstreet_block     = PSB0618,
         BE_pr_commer_dest_onstreet_block = PSB4031,
         BE_pr_liquor_onstreet_block = PSB0615,
         BE_pr_bar_onstreet_block = PSB0614)

ccahs_sso_ids <- ccahs_sso %>% 
  select(NC_ID, TRACT_ID, BLOCKGROUP_ID, BLOCK_ID)
save(ccahs_sso_ids, file = "./data/chicago/derived/ccahs_sso_ids.RData")

ccahs_sso_nc <- ccahs_sso %>%
  mutate(NC_ID = as.character(NC_ID)) %>%
  select(NC_ID, matches("_nc"), LAND_USE_7_CAT) %>%
  mutate(MIXED_LAND_USE = ifelse(LAND_USE_7_CAT %in% c(2,4), 1, 0)) %>%
  select(-LAND_USE_7_CAT) %>%
  group_by(NC_ID) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE)))

ccahs_sso_block <- ccahs_sso %>%
  mutate(BLOCK_ID = as.character(BLOCK_ID),
         TRACT_ID = as.character(TRACT_ID)) %>%
  select(BLOCK_ID, TRACT_ID, matches("_block|_face"), LAND_USE_7_CAT) %>%
  mutate(MIXED_LAND_USE = ifelse(LAND_USE_7_CAT %in% c(2,4), 1, 0)) %>%
  select(-LAND_USE_7_CAT) %>%
  group_by(TRACT_ID, BLOCK_ID) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE)))

save(ccahs_sso_block, file = "./data/chicago/derived/ccahs_sso_block.RData")
# V5 is cluster number
# V6 is census tract
# V7 is block


# ccahs_search <- function(string){
#   cbind(varname = names(ccahs_individual)[str_which(sapply(ccahs_individual, function(x) attr(x, "label")), string)], 
#         label = str_subset(sapply(ccahs_individual, function(x) attr(x, "label")), string))
# }
# 
# ccahs_search("nc")


ccahs_individual <- ccahs_individual_raw %>%
  select(NC_ID          = V5,
         TRACT_ID       = V6,
         BLOCK_ID       = V7,
         COHTR_2001     = V2816,
         INF_2001       = V2818,
         CLOSE_2001     = V2820,
         EXCH_2001      = V2822,
         PERC_DIS_2001  = V2826,
         PERC_VIOL_2001 = V2828,
         ORG_PART_2001  = V2832,
         VICT_2001      = V2834,
         CE_2001        = V2836,
         DISADV_2000    = V3805,
         AFFLUENCE_2000 = V3806,
         HISP_FOR_2000  = V3807,
         OLDER_2000      = V3808,
         RESSTAB_2000   = V3884,
         FRIEND_KIN_2001 = V2806,
         #  SSO_RES_SECURITY_2001   = EFC7210,
         #  SSO_COM_SECURITY_4_2001 = EFC7110,
         SSO_BAR_LIQ_2001    = ESC7040,
         # SSO_LIT_GRAF_2001   = EFC7150,
         PERC_EFF_self_2001 = V520,
         PHY_DIS_vacants_quantity_2001        = V533,
         afraid_to_call_police_2001 = V485,
         detect_strangers_2001 = V491,
         CE_library_fire_station_2001 = V484,
         FEAR_night_3blocks_2001 = V486,
         FEAR_night_2001 = V487,
         SSO_PHY_DIS_2001    = EFC7140,
         SSO_PHY_DECAY_2001  = ESC7120,
         COHTR_1995_2001     = V2852,
         INF_1995_2001       = V2856,
         CLOSE_1995_2001     = V2860,
         EXCH_1995_2001      = V2864,
         PERC_DIS_1995_2001  = V2872,
         PERC_VIOL_1995_2001 = V2876,
         ORG_PART_1995_2001  = V2884,
         VICT_1995_2001      = V2888,
         CE_1995_2001        = V2892,
         DENSITY_2000        = V3812,
         POP_2000            = V3811) %>%
  mutate(NC_ID = as.character(NC_ID),
         TRACT_ID = as.character(TRACT_ID),
         BLOCK_ID = as.character(BLOCK_ID))


# All the precalculated indices are at the NC level
ccahs_nc <- ccahs_individual %>%
  select(-TRACT_ID, -BLOCK_ID) %>%
  group_by(NC_ID) %>%
  summarize_all(~mean(.)) %>% 
  left_join(
    ccahs_sso_nc, by = "NC_ID")

ccahs_nc_no_sso <- ccahs_individual %>%
  select(-TRACT_ID, -BLOCK_ID) %>%
  group_by(NC_ID) %>%
  summarize_all(~mean(.))

save(ccahs_nc_no_sso, file = "./data/chicago/derived/ccahs_nc_no_sso.RData")
# Tracts per NC:
# <int> <int>
# 1        124
# 2        130
# 3         66
# 4         22
# 5          1

ccahs_survey_nc_tract <- ccahs_individual_raw %>%
  select(NC_ID = V5,
         TRACT_ID = V6) %>%
  mutate(TRACT_ID = round(TRACT_ID, 0)) %>%
  mutate_all(~as.character(.)) %>%
  distinct() %>%
  arrange(NC_ID, TRACT_ID)
# Don't have crime for 2001

save(ccahs_survey_nc_tract, file = "./data/chicago/derived/ccahs_survey_nc_tract.RData")
save(ccahs_nc, file = "./data/chicago/derived/ccahs_nc.RData")

# ccahs_tract <- ccahs_individual %>%
#   mutate(TRACT_ID = as.character(TRACT_ID)) %>%
#   group_by(TRACT_ID) %>%
#   summarize_all(~mean(.))
# 
# save(ccahs_tract, file = "./data/chicago/derived/ccahs_tract.RData")

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

# ccahs_individual doesn't have a pre-aggregated community-level data set.