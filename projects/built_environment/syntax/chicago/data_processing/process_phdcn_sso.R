library(tidyverse)


phdcn_cs_dir    <- "F:/SecureData/PHDCN_Community_Survey_9497_ICPSR_02766/"
phdcn_cs_community_raw <- haven::read_sav(paste0(phdcn_cs_dir, "DS0002/02766-0002-Data-REST.sav"))

# PHDCN Community is already aggregated and ready to go.

phdcn_sso_dir <- "F:/SecureData/PHDCN_Systematic_Social_Observation_ICPSR_13578/"
phdcn_sso_raw <- haven::read_por(paste0(phdcn_sso_dir, "DS0001/13578-0001-Data.por"))

phdcn_neighb_raw <- haven::read_sav("F:/SecureData/da02766-0002_Matsueda_02062019.sav")
phdcn_crosswalk <- haven::read_sav("F:/SecureData/Matsueda-tract_linknc.sav")

phdcn_sso <- phdcn_sso_raw %>%
  select(NC_ID = LINK_NC, 
         IDPUBLIC,
         LAND_USE                = OQ14,
         BE_vacant_house_any     = D1_4,
         BE_vacant_lot_any       = D1_5,
         BE_school_church_any    = D1_6,
         # BAR_upscale_any         = D15_1,
         # BAR_cocktails_any       = D15_2,
         # BAR_live_music_any      = D15_3,
         # BAR_local_bar_any       = D15_4,
         # BAR_run_down_any        = D15_5,
         # BAR_strip_any           = D15_6,
         # BAR_biker_any           = D15_7, #none
         # BAR_coffee_house_any    = D15_8,
         # BAR_sports_any          = D15_9,
         # BAR_teen_bar_any        = D15_10, # nearly none
         # BAR_other_alcohol_any   = D15_11,
         # BAR_none_any            = D15_12,
         BE_public_transit_any   = D21,
         BE_bank_cash_checking_n = D60,
         BE_bars_n               = D62,
         BE_drug_stores_n        = D69,
         BE_restaurants_n        = D71,
         BE_electronics_stores_n = D72,
         BE_fast_food_n          = D74,
         BE_gas_stations_n       = D79,
         BE_delis_n              = D81,
         BE_laundromats_n        = D85,
         BE_liquor_stores_n      = D86,
         BE_abandoned_house_n    = D121,
         BE_abandoned_comm_n     = D122,
         ) %>%
  mutate(across(matches("_any$"), ~ case_when(
    is.na(.) ~ NA_real_,
    . == -99 ~ NA_real_,
    . == 2 ~ 0,
    . == 1 ~ 1,
    TRUE ~ 99
  ))) %>%
  mutate(across(matches("_n$"), ~case_when(
    is.na(.) ~ NA_real_,
    . == 0 ~ 0,
    . > 0 ~ 1,
    TRUE ~ 99
  ))) %>%
  mutate(NC_ID = as.character(NC_ID), 
         MIXED_LAND_USE = as.numeric(LAND_USE == 3)) %>%
  select(-LAND_USE) %>%
  # mutate(BE_any_bar = ifelse(BAR_none_any == 0, 1, 0))
  mutate(
    BE_abandoned_any = ifelse(BE_abandoned_house_n > 0 | BE_abandoned_comm_n > 0, 1, 0),
    BE_ab_vac_bld_any = ifelse(BE_abandoned_any == 1 | BE_vacant_house_any == 1, 1, 0),
    BE_ab_vac_bld_lot_any = ifelse(BE_ab_vac_bld_any == 1 | BE_vacant_lot_any == 1, 1, 0),
    BE_bars_liquor_any = ifelse(BE_bars_n == 1 | BE_liquor_stores_n == 1, 1, 0)
    ) %>%
  pivot_longer(matches("_n$")) %>%
  group_by(IDPUBLIC) %>%
  mutate(BE_sum = ifelse(all(is.na(value)), NA, sum(value, na.rm=TRUE))) %>%
  ungroup() %>%
  pivot_wider()

ave(phdcn_sso, file = "./data/chicago/derived/phdcn_sso.RData")

phdcn_sso_nc <- phdcn_sso %>%
  select(-IDPUBLIC) %>%
  group_by(NC_ID) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE))) %>%
  mutate(across(-NC_ID, ~ifelse(is.nan(.), NA, .)))

save(phdcn_sso_nc, file = "./data/chicago/derived/phdcn_sso_nc.RData")
