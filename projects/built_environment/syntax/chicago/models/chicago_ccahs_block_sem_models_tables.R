library(tidyverse)
library(piecewiseSEM)
library(gt)

load("./data/chicago/derived/psem_hlm_list_summary.RData")
load("./data/chicago/derived/psem_hlm_int_list_summary.RData")

# SECOND STAGE 
hlm_data <- psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CRIME")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         row_group  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "NC"),
  ) %>%
  # mutate(across(c(Estimate, Std.Error), ~ round(as.numeric(.), 2))) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = fct_recode(Predictor,
                                          "Coll. Eff (2001)"  = "CE_hlm_2001",
                                          "Disadv."           = "FAC_disadv_2000",
                                          "Stability"         = "FAC_stability_2000",
                                          "Hisp./Immig."      = "FAC_hispimm_2000",
                                          "Density (NC)"      = "density_ltdb_nc_2000",
                                          "Density (Block)"   = "density_block",
                                          "Density (Block)^2" = "density_block_2",
                                          "Bars"              = "BE_pr_bar_onstreet_block_2001",
                                          "Liquor"            = "BE_pr_liquor_onstreet_block_2001",
                                          "Vacant"            = "BE_pr_vacant_onstreet_block_2001",
                                          "Abandoned"         = "BE_pr_abandoned_bld_onstreet_block_2001",
                                          "Commercial Dest."  = "BE_pr_commer_dest_onstreet_block_2001",
                                          "Recreation"        = "BE_pr_recreation_block_2001",
                                          "Parking"           = "BE_pr_parking_block_2001",
                                          "Mixed Use"         = "MIXED_LAND_USE_2001")) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96)

second_stage <- hlm_data %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
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
  cols_label(.list = list("Estimate_homicide"            = "Homicide", 
                          "Estimate_assault_battery_gun" = "Gun Assault",
                          "Estimate_robbery"             = "Robbery",
                          "Estimate_violent"             = "Violent",
                          "Estimate_property"            = "Property")) %>%
  cols_align("left", columns = 1) %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Crime", 
             subtitle = "Log-counts from second-stage negative binomial mixed models") %>% as_rtf()

# Plot

hlm_data %>%
  mutate(lb = Estimate - (1.96*Std.Error),
         ub = Estimate + (1.96*Std.Error)) %>%
  filter(str_detect(Predictor, "^(Coll|Mix|Aba|Bar|Comm|Liq|Park|Recr|Vac)")) %>%
  ggplot(aes( x = Estimate, y = Predictor, alpha = p_sig)) +
  scale_alpha_manual(values = c("TRUE"=1, "FALSE"=0.3)) +
  facet_grid(.~Response) + 
  geom_point() + 
  geom_vline(xintercept = 0) +
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) + theme_minimal()

# FIRST STAGE

psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "^CRIME|^~~|^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error)) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  #select(-Estimate, -Std.Error) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
  mutate(
    row_group           = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "NC"),
    Predictor           = fct_recode(Predictor,
                                     "Coll. Eff (1995)"  = "CE_hlm_1995",
                                     "Disadv."           = "FAC_disadv_2000",
                                     "Stability"         = "FAC_stability_2000",
                                     "Hisp./Immig."      = "FAC_hispimm_2000",
                                     "Density (NC)"      = "density_ltdb_nc_2000",
                                     "Density (Block)"   = "density_block",
                                     "Density (Block)^2" = "density_block_2")
  ) %>%
  group_by(row_group) %>%
  gt() %>%
  fmt_number(matches("Estimate|Std"), decimals = 2) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_bar_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_bar_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_liquor_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_liquor_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_commer_dest_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_commer_dest_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_recreation_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_recreation_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_parking_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_parking_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_vacant_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_vacant_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_BE_pr_abandoned_bld_onstreet_block_2001), 
                    col_uncert = vars(Std.Error_BE_pr_abandoned_bld_onstreet_block_2001)) %>%
  cols_merge_uncert(col_val    = vars(Estimate_MIXED_LAND_USE_2001), 
                    col_uncert = vars(Std.Error_MIXED_LAND_USE_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_bar_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_bar_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_liquor_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_liquor_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_commer_dest_onstreet_block_2001), 
                                   rows    = p_sig_BE_pr_commer_dest_onstreet_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_recreation_block_2001), 
                                   rows    = p_sig_BE_pr_recreation_block_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_BE_pr_parking_block_2001), 
                                   rows    = p_sig_BE_pr_parking_block_2001)) %>%
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
  cols_label(.list = list("Estimate_BE_pr_bar_onstreet_block_2001" = "Bars",
                          "Estimate_BE_pr_liquor_onstreet_block_2001" = "Liquor",
                          "Estimate_BE_pr_commer_dest_onstreet_block_2001" = "Commercial Dest.",
                          "Estimate_BE_pr_recreation_block_2001" = "Recreation",
                          "Estimate_BE_pr_parking_block_2001" = "Parking",
                          "Estimate_BE_pr_vacant_onstreet_block_2001" = "Vacant",
                          "Estimate_BE_pr_abandoned_bld_onstreet_block_2001" = "Abandoned",
                          "Estimate_MIXED_LAND_USE_2001" = "Mixed Use")) %>%
  cols_align("left", columns = 1) %>%
  fmt_missing(columns = everything(), missing_text = "---") %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Mediators", subtitle = "From first-stage linear mixed models")

# CE 2001

psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "CE_hlm_2001")) %>%
  mutate(Std.Error = as.numeric(Std.Error)) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  #select(-Estimate, -Std.Error) %>% 
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response) %>%
  mutate(
    row_group           = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "NC"),
    Predictor = str_replace_all(Predictor,c(
      "CE_hlm_1995"  = "Coll. Eff (1995)",
      "FAC_disadv_2000$" = "Disadv.",
      "FAC_disadv_2000_2" = "Disadv.^2",
      "FAC_stability_2000" = "Stability",
      "FAC_hispimm_2000" =  "Hisp./Immig.",
      "density_ltdb_nc_2000"="Density (NC)",
      "density_block$"="Density (Block)" ,
      "density_block_2"="Density (Block)^2",
      "BE_pr_bar_onstreet_block_2001"="Bars",
      "BE_pr_liquor_onstreet_block_2001"="Liquor",
      "BE_pr_vacant_onstreet_block_2001"="Vacant" ,
      "BE_pr_abandoned_bld_onstreet_block_2001"="Abandoned",
      "BE_pr_commer_dest_onstreet_block_2001" = "Commercial Dest.",
      "BE_pr_recreation_block_2001" = "Recreation",
      "BE_pr_parking_block_2001" = "Parking",
      "MIXED_LAND_USE_2001"="Mixed Use"))
  ) %>%
  group_by(row_group) %>%
  gt() %>%
  fmt_number(matches("Estimate|Std"), decimals = 2) %>%
  cols_merge_uncert(col_val    = vars(Estimate_CE_hlm_2001), 
                    col_uncert = vars(Std.Error_CE_hlm_2001)) %>%
  tab_style(cell_text(weight = "bold"), 
            locations = cells_body(columns = vars(Estimate_CE_hlm_2001), 
                                   rows    = p_sig_CE_hlm_2001)) %>%
  cols_hide(matches("p_sig")) %>%
  cols_label(.list = list("Estimate_CE_hlm_2001" = "Collective Eff. 2001")) %>%
  cols_align("left", columns = 1) %>%
  fmt_missing(columns = everything(), missing_text = "---") %>%
  tab_spanner(label = "Outcome", columns = matches("Estimate")) %>%
  tab_header("Estimated Effects on Collective Efficacy", subtitle = "From first-stage linear mixed models")

# INTERACTIONS

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
    row_group = ifelse(str_detect(Predictor, "^(BE_|MIX|density_b)"), "Block", "Neighborhood Cluster"),
    Predictor = str_replace_all(Predictor,c(
                                "CE_hlm_2001"  = "Coll. Eff (2001)",
                                "FAC_disadv_2000$" = "Disadv.",
                                "FAC_disadv_2000_2" = "Disadv.^2",
                                "FAC_stability_2000" = "Stability",
                                "FAC_hispimm_2000" =  "Hisp./Immig.",
                                "density_ltdb_nc_2000"="Density (NC)",
                                "density_block$"="Density (Block)" ,
                                "density_block_2"="Density (Block)^2",
                                "BE_pr_bar_onstreet_block_2001"="Bars",
                                "BE_pr_liquor_onstreet_block_2001"="Liquor",
                                "BE_pr_vacant_onstreet_block_2001"="Vacant" ,
                                "BE_pr_abandoned_bld_onstreet_block_2001"="Abandoned",
                                "BE_pr_commer_dest_onstreet_block_2001" = "Commercial Dest.",
                                "BE_pr_recreation_block_2001" = "Recreation",
                                "BE_pr_parking_block_2001" = "Parking",
                                "MIXED_LAND_USE_2001"="Mixed Use"))
  ) %>%
  mutate(Predictor = str_replace(Predictor, "\\:", " x ")) %>%
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
