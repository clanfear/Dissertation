library(tidyverse)
library(lme4)
load("../shared/data/chicago/derived/ccahs_individual_wide.RData")
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical.RData")

indiv_block <- ccahs_individual_wide %>%
  left_join(ccahs_block_analytical, by = c("NC_ID" = "ccahs_nc", "TRACT_ID" = "ccahs_tract", "BLOCK_ID" = "census_block"))

glm(VICT_violent_ever ~ FEMALE + FAM_married + FAM_sepdiv + 
       FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
       years_in_home + SES + 
        CE_hlm_2001 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + density_ltdb_nc_2000 +
      BE_pr_bar_onstreet_block_2001 +
      BE_pr_liquor_onstreet_block_2001 +
      BE_pr_vacant_onstreet_block_2001 +
      BE_pr_abandoned_bld_onstreet_block_2001 +
      BE_pr_commer_dest_onstreet_block_2001 +
      BE_pr_recreation_block_2001 + 
      BE_pr_parking_block_2001 +
      MIXED_LAND_USE_2001 + 
      density_block, data = indiv_block,
      family="binomial") %>% summary()