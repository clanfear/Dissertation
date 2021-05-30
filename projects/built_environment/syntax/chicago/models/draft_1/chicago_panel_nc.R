library(mgcv)
library(tidyverse)
library(lme4)
library(lavaan)
library(modelsummary)
source("./syntax/project_functions.R")
load("./data/chicago/derived/phdcn_ccahs_nc.RData")

chicago_panel_analytical <- phdcn_ccahs_nc %>%
  select(NC_ID,
         INF_1995,                                   INF_2001,
         COHTR_1995,                                 COHTR_2001,
         homicide_1995 = CNT_MURDER_1995,            homicide_2004,
         violent_1995 = VIOLENT_CRIME_1995,          violent_2004,
         perc_violence_1995 = PERC_VIOL_1995,        perc_violence_2001 = PERC_VIOL_2001,
         hispanic_immig_1990 = HISPIMMIG_1990,       hispanic_immig_2000 = HISP_FOR_2000,
         stability_1990 = RESSTAB_1990,              stability_2000 = RESSTAB_2000,
         disadvantage_1990 = CONC_DISADV_1990,       disadvantage_2000 = DISADV_2000,
         mixed_land_1995 = MIXED_LAND_USE_1995,      mixed_land_2001 = MIXED_LAND_USE_2001,
         vacant_1995 = BE_vacant_lot_any_1995,       vacant_2001 = BE_pr_vacant_lot_nc_2001,
         abandoned_1995 =  BE_abandoned_any_1995,    abandoned_2001 = BE_pr_abandoned_bld_onstreet_nc_2001,
         bars_liquor_1995 = BE_bars_liquor_any_1995, bars_liquor_2001 = BE_eb_bars_liquor_nc_2001,
         population_1995 = POPULATION_1995,          population_2000 = POP_2000,
         victimization_2001 = VICT_2001, victimization_1995 = VICT_EVER_1995
  ) %>%
  mutate(violent_count_2004 = violent_2004) %>%
  mutate(across(-c(NC_ID, homicide_1995, homicide_2004, violent_count_2004), ~standardize(.))) %>%
  mutate(CE_1995 = (INF_1995 + COHTR_1995)/2,
         CE_2001 = (INF_2001 + COHTR_2001)/2) %>%
  filter(!is.na(mixed_land_1995) & !is.na(vacant_1995) & !is.na(abandoned_1995) & !is.na(bars_liquor_1995)) %>%
  pivot_longer(-NC_ID) %>%
  mutate(TIME = ifelse(str_detect(name, "(00|01|02|03|04)$"), 2, 1)) %>%
  mutate(name = str_remove_all(name, "_[0-9]+$")) %>%
  mutate(NC_ID = factor(NC_ID)) %>%
  pivot_wider()

hom_models <- list(
  "Homicide Base" = MASS::glm.nb(homicide ~ 
                         CE + disadvantage + hispanic_immig + stability + population +
                         TIME, data = chicago_panel_analytical),
  "Homicide BE" = MASS::glm.nb(homicide ~ 
           CE + disadvantage + hispanic_immig + stability + population +
           mixed_land + vacant + abandoned + bars_liquor  + TIME, data = chicago_panel_analytical)
)


perc_viol_models <- list(
  "Perc. Viol. Base" = lm(perc_violence ~ 
               CE + disadvantage + hispanic_immig + stability + population +
               TIME, data = chicago_panel_analytical),
  "BE" = lm(perc_violence ~ 
              CE + disadvantage + hispanic_immig + stability + population +
              mixed_land + abandoned + bars_liquor + vacant + TIME, data = chicago_panel_analytical)
)

violent_models <- list(
  "Violent Base" = lm(violent ~ 
               CE + disadvantage + hispanic_immig + stability + population +
               TIME, data = chicago_panel_analytical),
  "Violent BE" = lm(violent ~ 
       CE + disadvantage + hispanic_immig + stability + population +
       mixed_land + abandoned + vacant +  bars_liquor + TIME, data = chicago_panel_analytical)
  )

table_panelnc_hom <- modelsummary(hom_models,
                                    output="gt",
                                    coef_map = c("CE" = "Coll. Eff.",
                                                 "abandoned" = "Abandoned",
                                                 "vacant" = "Vacant",
                                                 "bars_liquor" = "Bars & Liquor",
                                                 "mixed_land" = "Mixed Land Use",
                                                 "disadvantage" = "Disadvantage",
                                                 "stability" = "Stability",
                                                 "hispanic_immig" = "Heterogeneity",
                                                 "population" = "Population",
                                                 "TIME" = "Wave"))

table_panelnc_perc_viol <- modelsummary(perc_viol_models,
                                  output="gt",
                                  coef_map = c("CE" = "Coll. Eff.",
                                               "abandoned" = "Abandoned",
                                               "vacant" = "Vacant",
                                               "bars_liquor" = "Bars & Liquor",
                                               "mixed_land" = "Mixed Land Use",
                                               "disadvantage" = "Disadvantage",
                                               "stability" = "Stability",
                                               "hispanic_immig" = "Heterogeneity",
                                               "population" = "Population",
                                               "TIME" = "Wave"))


table_panelnc_violent <- modelsummary(violent_models,
                                        output="gt",
                                        coef_map = c("CE" = "Coll. Eff.",
                                                     "abandoned" = "Abandoned",
                                                     "vacant" = "Vacant",
                                                     "bars_liquor" = "Bars & Liquor",
                                                     "mixed_land" = "Mixed Land Use",
                                                     "disadvantage" = "Disadvantage",
                                                     "stability" = "Stability",
                                                     "hispanic_immig" = "Heterogeneity",
                                                     "population" = "Population",
                                                     "TIME" = "Wave"))

panel_nc_table <- modelsummary(c(hom_models, perc_viol_models, violent_models),
                               output="gt",
                               coef_map = c("CE" = "Coll. Eff.",
                                            "abandoned" = "Abandoned",
                                            "vacant" = "Vacant",
                                            "bars_liquor" = "Bars & Liquor",
                                            "mixed_land" = "Mixed Land Use",
                                            "disadvantage" = "Disadvantage",
                                            "stability" = "Stability",
                                            "hispanic_immig" = "Heterogeneity",
                                            "population" = "Population",
                                            "TIME" = "Wave"), gof_omit = "Log.Lik|F")
save(panel_nc_table, file = "./output/chicago/models/panel_nc_table.RData")
