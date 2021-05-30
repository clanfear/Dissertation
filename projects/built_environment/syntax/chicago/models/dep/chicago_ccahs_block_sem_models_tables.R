library(tidyverse)
library(piecewiseSEM)
library(gt)

load("./data/chicago/derived/psem_hlm_list_summary.RData")
load("./data/chicago/derived/psem_hlm_int_list_summary.RData")
load("./data/chicago/derived/psem_hlm_nobece_list_summary.RData")
load("./data/chicago/derived/psem_hlm_nobece_int_list_summary.RData")

# DO THE NO BLOCK COVARS MODELS 

# value = paste0(Estimate, " (", Std.Error, ")")
psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error)) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)")) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  #select(-Estimate, -Std.Error) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
  mutate(
    row_group = ifelse(str_detect(Predictor, "^(BE_|MIX|dens)"), "Block", "Tract"),
    Predictor = fct_recode(Predictor,
    "Coll. Eff (2001)" = "CE_hlm_2001",
    "Disadv." = "DISADV_2000_1",
    "Disadv.^2" = "DISADV_2000_2",
    "Affluence" = "AFFLUENCE_2000",
    "Conc. Immig." = "HISP_FOR_2000",
    "Density (Tract)" = "DENSITY_2000",
    "Density (Block)" = "density_1",
    "Density (Block)^2" = "density_2",
    "Bars & Liquor" = "BE_pr_mean_bars_liquor_onstreet_block_2001",
    "Vacant" = "BE_pr_vacant_onstreet_block_2001",
    "Abandoned" = "BE_pr_abandoned_bld_onstreet_block_2001",
    "Mixed Use" = "MIXED_LAND_USE_2001")
  ) %>%
  group_by(row_group) %>%
  gt() %>%
  fmt_number(matches("Estimate|Std"), decimals = 2) %>%
  cols_merge_uncert(col_val = vars(Estimate_homicide), 
                    col_uncert = vars(Std.Error_homicide)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_assault_battery_gun), 
                    col_uncert = vars(Std.Error_assault_battery_gun)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_robbery), 
                    col_uncert = vars(Std.Error_robbery)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_violent), 
                    col_uncert = vars(Std.Error_violent)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_property), 
                    col_uncert = vars(Std.Error_property)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_homicide), 
                                   rows    = p_sig_homicide)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_assault_battery_gun), 
                                   rows    = p_sig_assault_battery_gun)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_robbery), 
                                   rows    = p_sig_robbery)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_violent), 
                                   rows    = p_sig_violent)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_property), 
                                   rows    = p_sig_property)) %>%
  cols_hide(matches("p_sig")) %>%
  cols_label(.list = list("Estimate_homicide" = "Homicide", 
                          "Estimate_assault_battery_gun" = "Gun Assault",
                          "Estimate_robbery" = "Robbery",
                          "Estimate_violent" = "Violent",
                          "Estimate_property" = "Property")) %>%
  cols_align("left", columns = 1) %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Crime", subtitle = "Log-counts from second-stage negative binomial mixed models")

psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "^CRIME|^~~")) %>%
  mutate(Std.Error = as.numeric(Std.Error)) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  #select(-Estimate, -Std.Error) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
  mutate(
    row_group = ifelse(str_detect(Predictor, "^(BE_|MIX|dens)"), "Block", "Tract"),
    Predictor = fct_recode(Predictor,
                           "Coll. Eff (1995)" = "CE_hlm_1995",
                           "Disadv." = "DISADV_2000_1",
                           "Disadv.^2" = "DISADV_2000_2",
                           "Affluence" = "AFFLUENCE_2000",
                           "Conc. Immig." = "HISP_FOR_2000",
                           "Density (Tract)" = "DENSITY_2000",
                           "Density (Block)" = "density_1",
                           "Density (Block)^2" = "density_2",
                           "Bars & Liquor" = "BE_pr_mean_bars_liquor_onstreet_block_2001",
                           "Vacant"    = "BE_pr_vacant_onstreet_block_2001",
                           "Abandoned" = "BE_pr_abandoned_bld_onstreet_block_2001",
                           "Mixed Use" = "MIXED_LAND_USE_2001")
  ) %>%
  group_by(row_group) %>%
  gt() %>%
  fmt_number(matches("Estimate|Std"), decimals = 2) %>%
  cols_merge_uncert(col_val    = vars(Estimate_CE_hlm_2001), 
                    col_uncert = vars(Std.Error_CE_hlm_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_mean_bars_liquor_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_mean_bars_liquor_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_vacant_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_vacant_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_abandoned_bld_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_abandoned_bld_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_MIXED_LAND_USE_2001), 
                    col_uncert = vars(Std.Error_MIXED_LAND_USE_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_CE_hlm_2001), 
                                   rows    = p_sig_CE_hlm_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_mean_bars_liquor_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_mean_bars_liquor_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_vacant_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_vacant_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_abandoned_bld_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_abandoned_bld_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_MIXED_LAND_USE_2001), 
                                   rows    = p_sig_MIXED_LAND_USE_2001)) %>%
  cols_hide(matches("p_sig")) %>%
  cols_label(.list = list("Estimate_CE_hlm_2001" = "Coll. Eff. (2001)", 
                          "Estimate_BE_pr_mean_bars_liquor_onstreet_block_2001" = "Bars & Liquor",
                          "Estimate_BE_pr_vacant_onstreet_block_2001" = "Vacant",
                          "Estimate_BE_pr_abandoned_bld_onstreet_block_2001" = "Abandoned",
                          "Estimate_MIXED_LAND_USE_2001" = "Mixed Use")) %>%
  cols_align("left", columns = 1) %>%
  fmt_missing(columns = everything(), missing_text = "---") %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Mediators", subtitle = "From first-stage linear mixed models")

#---

psem_hlm_int_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error)) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)")) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  #select(-Estimate, -Std.Error) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
  mutate(
    row_group = ifelse(str_detect(Predictor, "^(BE_|MIX|dens)"), "Block", "Tract"),
    Predictor = str_replace_all(Predictor,c(
                                "CE_hlm_2001"  = "Coll. Eff (2001)",
                                "DISADV_2000_1" = "Disadv.",
                                "DISADV_2000_2" = "Disadv.^2",
                                "AFFLUENCE_2000" = "Affluence",
                                "HISP_FOR_2000" =  "Conc. Immig.",
                                "DENSITY_2000"="Density (Tract)",
                                "density_1"="Density (Block)" ,
                                "density_2"="Density (Block)^2",
                                "BE_pr_mean_bars_liquor_onstreet_block_2001"="Bars & Liquor",
                                "BE_pr_vacant_onstreet_block_2001"="Vacant" ,
                                "BE_pr_abandoned_bld_onstreet_block_2001"="Abandoned",
                                "MIXED_LAND_USE_2001"="Mixed Use"))
  ) %>%
  group_by(row_group) %>%
  gt() %>%
  fmt_number(matches("Estimate|Std"), decimals = 2) %>%
  cols_merge_uncert(col_val = vars(Estimate_homicide), 
                    col_uncert = vars(Std.Error_homicide)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_assault_battery_gun), 
                    col_uncert = vars(Std.Error_assault_battery_gun)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_robbery), 
                    col_uncert = vars(Std.Error_robbery)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_violent), 
                    col_uncert = vars(Std.Error_violent)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_property), 
                    col_uncert = vars(Std.Error_property)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_homicide), 
                                   rows    = p_sig_homicide)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_assault_battery_gun), 
                                   rows    = p_sig_assault_battery_gun)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_robbery), 
                                   rows    = p_sig_robbery)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_violent), 
                                   rows    = p_sig_violent)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_property), 
                                   rows    = p_sig_property)) %>%
  cols_hide(matches("p_sig")) %>%
  cols_label(.list = list("Estimate_homicide" = "Homicide", 
                          "Estimate_assault_battery_gun" = "Gun Assault",
                          "Estimate_robbery" = "Robbery",
                          "Estimate_violent" = "Violent",
                          "Estimate_property" = "Property")) %>%
  cols_align("left", columns = 1) %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Crime", subtitle = "Log-counts from second-stage negative binomial mixed models")
