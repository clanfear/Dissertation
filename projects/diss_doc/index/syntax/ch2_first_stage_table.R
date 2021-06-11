load("../../built_environment/data/chicago/derived/psem_hlm_list_summary.RData")
load("../../built_environment/data/chicago/derived/psem_hlm_int_list_summary.RData")

first_stage_data <-  psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "^CRIME|^~~|^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff. (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Coll. Eff. (1995)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Density (Block)"      ,
             "Density (Block)^2"    )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

ce_data <-  psem_hlm_list_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(str_detect(Response, "^CE")) %>%
  mutate(Std.Error = as.numeric(Std.Error),
         Level  = ifelse(str_detect(Predictor, "^(BE_|MIX|density_block)"), "Block", "Neighborhood"),
  ) %>%
  mutate(Response = str_remove_all(Response, "(CRIME_|_2004_2006)"),
         Predictor           = 
           fct_relevel(
             fct_recode(Predictor,
                        "Coll. Eff. (1995)"      = "CE_hlm_1995",
                        "Disadv."               = "FAC_disadv_2000",
                        "Stability"             = "FAC_stability_2000",
                        "Hispanic /\nImmigrant"      = "FAC_hispimm_2000",
                        "Density (Neighb.)"     = "density_ltdb_nc_2000",
                        "Abandoned"             = "BE_pr_abandoned_bld_onstreet_block_2001",
                        "Bars"                  = "BE_pr_bar_onstreet_block_2001",
                        "Commer. Dest."         = "BE_pr_commer_dest_onstreet_block_2001",
                        "Liquor"                = "BE_pr_liquor_onstreet_block_2001",
                        "Mixed Use"             = "MIXED_LAND_USE_2001",
                        "Parking"               = "BE_pr_parking_block_2001",
                        "Recreation"            = "BE_pr_recreation_block_2001",
                        "Vacant"                = "BE_pr_vacant_onstreet_block_2001",
                        "Density (Block)"       = "density_block",
                        "Density (Block)^2"     = "density_block_2"),
             "Coll. Eff. (1995)"     ,
             "Disadv."              ,
             "Stability"            ,
             "Hispanic /\nImmigrant",
             "Density (Neighb.)"    ,
             "Abandoned"            ,
             "Bars"                 ,
             "Commer. Dest."        ,
             "Liquor"               ,
             "Mixed Use"            ,
             "Parking"              ,
             "Recreation"           ,
             "Vacant"               ,
             "Density (Block)"      ,
             "Density (Block)^2"    )) %>%
  mutate(p_sig = abs(Estimate) > Std.Error*1.96) %>%
  pivot_wider(values_from = c(Estimate, Std.Error, p_sig), names_from = Response, names_glue = "{Response}_{.value}") %>%
  arrange(desc(Level), Predictor)

# BOTH

r2vals_both <- c(" "="R2", "", setNames(round(psem_hlm_list_summary$R2$Marginal[c(6,10,7,11,8,14,13,12,9)], 2), 
                                    str_remove_all(psem_hlm_list_summary$R2$Response[c(6,10,7,11,8,14,13,12,9)], 
                                                   "^BE_pr_|_block|_2001.*")))
s1_table <- 
  ce_data %>% 
  full_join(first_stage_data) %>%
  as_grouped_data(groups = "Level") %>%
  flextable(col_keys    = c("Level",
                            "Predictor", 
                            "Collective\nEfficacy", 
                            "Abandoned", 
                            "Bar", 
                            "Commercial\nDestination", 
                            "Liquor", 
                            "Mixed\nLand\nUse", 
                            "Parking", 
                            "Recreation", 
                            "Vacant")) %>%
  compose(j = "Collective\nEfficacy",    value = as_paragraph(CE_hlm_2001_Estimate, "\n", "(", CE_hlm_2001_Std.Error, ")")) %>%
  compose(j = "Abandoned",               i = ~ !is.na(BE_pr_abandoned_bld_onstreet_block_2001_Estimate),              value = as_paragraph(BE_pr_abandoned_bld_onstreet_block_2001_Estimate, "\n", "(", BE_pr_abandoned_bld_onstreet_block_2001_Std.Error, ")")) %>%
  compose(j = "Bar",                     i = ~ !is.na(BE_pr_bar_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_bar_onstreet_block_2001_Estimate,           "\n", "(", BE_pr_bar_onstreet_block_2001_Std.Error,           ")")) %>%
  compose(j = "Commercial\nDestination", i = ~ !is.na(BE_pr_commer_dest_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_commer_dest_onstreet_block_2001_Estimate,   "\n", "(", BE_pr_commer_dest_onstreet_block_2001_Std.Error,   ")")) %>%
  compose(j = "Liquor",                  i = ~ !is.na(BE_pr_liquor_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_liquor_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_liquor_onstreet_block_2001_Std.Error,        ")")) %>%
  compose(j = "Mixed\nLand\nUse",         i = ~ !is.na(MIXED_LAND_USE_2001_Estimate), value = as_paragraph(MIXED_LAND_USE_2001_Estimate,                     "\n", "(", MIXED_LAND_USE_2001_Std.Error,                     ")")) %>%
  compose(j = "Parking",                 i = ~ !is.na(BE_pr_parking_block_2001_Estimate), value = as_paragraph(BE_pr_parking_block_2001_Estimate,                "\n", "(", BE_pr_parking_block_2001_Std.Error,                ")")) %>%
  compose(j = "Recreation",              i = ~ !is.na(BE_pr_recreation_block_2001_Estimate), value = as_paragraph(BE_pr_recreation_block_2001_Estimate,             "\n", "(", BE_pr_recreation_block_2001_Std.Error,             ")")) %>%
  compose(j = "Vacant",                  i = ~ !is.na(BE_pr_vacant_onstreet_block_2001_Estimate), value = as_paragraph(BE_pr_vacant_onstreet_block_2001_Estimate,        "\n", "(", BE_pr_vacant_onstreet_block_2001_Std.Error,        ")")) %>%
  set_header_labels(Level                     = "",
                    Predictor                 = "Predictor", 
                    `Collective\nEfficacy`    = "Collec.\nEffic.",
                    Abandoned                 = "Abandoned", 
                    Bar                       = "Bar", 
                    `Commercial\nDestination` = "Commer.\nDest.", 
                    Liquor                    = "Liquor", 
                    `Mixed\nLand\nUse`        = "Mixed\nLand\nUse", 
                    Parking                   = "Parking", 
                    Recreation                = "Recreation", 
                    Vacant                    = "Vacant"  ) %>%
  merge_h_range(i = c(1,7), j1 = 1, j2 = 10) %>%
  bold(i = ~ CE_hlm_2001_p_sig == TRUE, j = "Collective\nEfficacy") %>%
  bold(i = ~ BE_pr_abandoned_bld_onstreet_block_2001_p_sig == TRUE, j = "Abandoned") %>%
  bold(i = ~ BE_pr_bar_onstreet_block_2001_p_sig == TRUE, j = "Bar") %>%
  bold(i = ~ BE_pr_commer_dest_onstreet_block_2001_p_sig == TRUE, j = "Commercial\nDestination") %>%
  bold(i = ~ BE_pr_liquor_onstreet_block_2001_p_sig == TRUE, j = "Liquor") %>%
  bold(i = ~ MIXED_LAND_USE_2001_p_sig == TRUE, j = "Mixed\nLand\nUse") %>%
  bold(i = ~ BE_pr_parking_block_2001_p_sig == TRUE, j = "Parking") %>%
  bold(i = ~ BE_pr_recreation_block_2001_p_sig == TRUE, j = "Recreation") %>%
  bold(i = ~ BE_pr_vacant_onstreet_block_2001_p_sig == TRUE, j = "Vacant") %>%
  compose(j = 2, i = 17, value = as_paragraph("Density (Block)", as_sup("2"))) %>%
  align(i = c(2:6, 8:17), j = 3:11, align = "center", part = "body") %>%
  align(j = 3:11, align = "center", part = "header") %>%
  italic(i = c(1,7)) %>%
  add_footer_row(values = r2vals_both , colwidths = rep(1, length(r2vals_both))) %>%
  align(j = 3:11, align = "center", part = "footer") %>%
  font(fontname = "Latin Modern Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  set_table_properties(layout = "fixed")  %>%
  border_remove() %>%
  width(j = 1, width = 0) %>%
  width(j = 2, width = 0.6) %>%
  width(j = 3:10, width = 0.48) %>%
  height(height = 0.35, part = "body") %>%
  hline_bottom(border = officer::fp_border(width = 0.5)) %>%
  hline_bottom(border = officer::fp_border(width = 1), part = "footer") %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))
