library(tidyverse)
source("./syntax/project_functions.R")
# Just need to get a descriptive on how much shit changes from 1995 to 2001 in BE

# Need:
## PHDCN SSO
## CCAHS SSO
# Mixed land use, abandoneds, vacants, bar/liquor
# PHDCN is only identified down to the NC level, so that's the best we can do
load("./data/chicago/derived/phdcn_ccahs_nc.RData")

be_1995_2001 <- phdcn_ccahs_nc %>%
  select(NC_ID, CE_2001, CE_1995_2001, COHTR_1995, INF_1995, 
         BE_abandoned_any_1995, BE_pr_abandoned_bld_onstreet_nc_2001,
         BE_bars_liquor_any_1995, BE_eb_bars_liquor_nc_2001,
         MIXED_LAND_USE_1995, MIXED_LAND_USE_2001,
         BE_vacant_lot_any_1995, BE_pr_vacant_lot_nc_2001) %>%
  mutate(CE_1995_change = standardize(CE_2001 - CE_1995_2001),
         CE_1995_avg = (standardize(COHTR_1995) + standardize(INF_1995))/2) %>%
  na.omit()

be_1995_2001 %>%
  select(starts_with("BE"), starts_with("MIXED")) %>%
  cor(use = "pairwise.complete")

# Abandoned ~ 0.55
# Bars / Liquor ~ 0.49
# Vacant ~ 0.71
# Mixed ~ 0.46

summary(lm(BE_pr_abandoned_bld_onstreet_nc_2001 ~ BE_abandoned_any_1995 + CE_1995_avg, data = be_1995_2001))
summary(lm(BE_eb_bars_liquor_nc_2001 ~ BE_bars_liquor_any_1995 + CE_1995_avg, data = be_1995_2001))
summary(lm(MIXED_LAND_USE_2001 ~ MIXED_LAND_USE_1995 + CE_1995_avg, data = be_1995_2001))
summary(lm(BE_pr_vacant_lot_nc_2001 ~ BE_vacant_lot_any_1995 + CE_1995_avg, data = be_1995_2001))

# WEISBURD DATA
load("./data/seattle/derived/weisburd_points_90_00.RData")
glimpse(weisburd_points_90_00)
weisburd_points_90_00 %>%
  select(vacant_pct_91, vacant_pct_04,
         street_lighting_97_99, street_lighting_02_04,
         mixed_land_use_91, mixed_land_use_04,
         public_housing_n_89_91, public_housing_n_02_04) %>%
  cor()

# vacant ~ .68 (91-04)
# street lights ~ 0.98 (97-04)
# mixed land use ~ 0.87 (91-04)
# public housing ~ 0.96 (89-04)