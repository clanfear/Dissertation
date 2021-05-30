library(mgcv)
library(tidyverse)
library(lme4)
library(lavaan)
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

summary(gam(homicide ~ 
                 CE + disadvantage + hispanic_immig + stability + population + TIME + s(NC_ID, bs = 're'),
                 data = chicago_panel_analytical, family = "nb",
                 method = 'REML'))

summary(gam(homicide ~ 
              CE + disadvantage + hispanic_immig + stability + population + TIME + abandoned + s(NC_ID, bs = 're'),
            data = chicago_panel_analytical, family = "nb",
            method = 'REML'))

summary(gam(perc_violence ~ 
              CE + disadvantage + hispanic_immig + stability + population + TIME + s(NC_ID, bs = 're'),
            data = chicago_panel_analytical, family = "gaussian",
            method = 'REML'))

summary(gam(perc_violence ~ 
              CE + disadvantage + hispanic_immig + stability + population + TIME + abandoned + s(NC_ID, bs = 're'),
            data = chicago_panel_analytical, family = "gaussian",
            method = 'REML'))

summary(sem("
            perc_violence ~ CE + disadvantage + hispanic_immig + stability + population + TIME + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability + population + TIME
            ", data = chicago_panel_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(gam(victimization ~ 
              CE + disadvantage + hispanic_immig + stability + population + TIME + s(NC_ID, bs = 're'),
            data = chicago_panel_analytical, family = "gaussian",
            method = 'REML'))

summary(gam(victimization ~ 
              CE + disadvantage + hispanic_immig + stability + population + TIME + abandoned + s(NC_ID, bs = 're'),
            data = chicago_panel_analytical, family = "gaussian",
            method = 'REML'))

summary(sem("
            victimization ~ CE + disadvantage + hispanic_immig + stability + population + TIME + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability + population + TIME
            ", data = chicago_panel_analytical %>% mutate(l_homicide = log(homicide+1))))
##

chicago_2001_analytical <- phdcn_ccahs_nc %>%
  select(NC_ID,
         INF_1995,                                   INF_2001,
         COHTR_1995,                                 COHTR_2001,
         homicide_1995 = CNT_MURDER_1995,            homicide_2004,
         violent_1995 = VIOLENT_CRIME_1995,          violent_2004,
         hispanic_immig_1990 = HISPIMMIG_1990,       hispanic_immig_2000 = HISP_FOR_2000,
         stability_1990 = RESSTAB_1990,              stability_2000 = RESSTAB_2000,
         disadvantage_1990 = CONC_DISADV_1990,       disadvantage_2000 = DISADV_2000,
         mixed_land_1995 = MIXED_LAND_USE_1995,      mixed_land_2001 = MIXED_LAND_USE_2001,
         vacant_1995 = BE_vacant_lot_any_1995,       vacant_2001 = BE_pr_vacant_lot_nc_2001,
         abandoned_1995 =  BE_abandoned_any_1995,    abandoned_2001 = BE_pr_abandoned_bld_onstreet_nc_2001,
         bars_liquor_1995 = BE_bars_liquor_any_1995, bars_liquor_2001 = BE_eb_bars_liquor_nc_2001,
         population_1995 = POPULATION_1995,          population_2000 = POP_2000,
         property_2004, robbery_2004, burglary_2004, assault_battery_2004, robbery_gun_2004, assault_battery_gun_2004, violent_gun_2004
  ) %>%
  mutate(violent_count_2004  = violent_2004,
         property_count_2004 = property_2004,
         robbery_count_2004  = robbery_2004,
         burglary_count_2004 = burglary_2004,
         assault_battery_count_2004 = assault_battery_2004,
         robbery_gun_count_2004 = robbery_gun_2004,
         assault_battery_gun_count_2004 = assault_battery_gun_2004,
         violent_gun_count_2004  = violent_gun_2004) %>%
  mutate(across(-c(NC_ID, homicide_1995, homicide_2004, matches("count")), ~standardize(.))) %>%
  mutate(CE_1995 = (INF_1995 + COHTR_1995)/2,
         CE_2001 = (INF_2001 + COHTR_2001)/2) %>%
  pivot_longer(-NC_ID) %>%
  mutate(TIME = ifelse(str_detect(name, "(00|01|02|03|04)$"), 2, 1)) %>%
  mutate(name = str_remove_all(name, "_[0-9]+$")) %>%
  filter(TIME == 2 ) %>%
  pivot_wider() %>% 
  filter(!is.na(mixed_land) & !is.na(vacant) & !is.na(abandoned) & !is.na(bars_liquor)) %>%
  mutate(l_homicide = log(homicide+1))

summary(MASS::glm.nb(homicide ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(homicide ~  CE + disadvantage + hispanic_immig + stability + population + abandoned, data = chicago_2001_analytical))

summary(sem("
            l_homicide ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(violent_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(violent_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned, data = chicago_2001_analytical))

summary(sem("
            violent ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(property_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(property_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned, data = chicago_2001_analytical))


summary(sem("
            property ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(robbery_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(robbery_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned, data = chicago_2001_analytical))


summary(sem("
            robbery ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(robbery_gun_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(robbery_gun_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned, data = chicago_2001_analytical))

summary(sem("
            robbery_gun ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(assault_battery_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(assault_battery_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned + vacant, data = chicago_2001_analytical))

summary(sem("
            assault_battery ~ CE + disadvantage + hispanic_immig + stability  + abandoned
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

summary(MASS::glm.nb(assault_battery_gun_count ~  CE + disadvantage + hispanic_immig + stability + population, data = chicago_2001_analytical))
summary(MASS::glm.nb(assault_battery_gun_count ~  CE + disadvantage + hispanic_immig + stability + population + abandoned + vacant, data = chicago_2001_analytical))

summary(sem("
            assault_battery_gun ~ CE + disadvantage + hispanic_immig + stability  + abandoned + vacant
            abandoned ~ CE + disadvantage + hispanic_immig + stability 
            vacant ~ CE + disadvantage + hispanic_immig + stability 
            ", data = chicago_2001_analytical %>% mutate(l_homicide = log(homicide+1))))

