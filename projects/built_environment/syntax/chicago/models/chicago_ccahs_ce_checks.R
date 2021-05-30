library(tidyverse)
library(lavaan)
library(lme4)
library(mgcv)
library(DHARMa)
library(piecewiseSEM)
library(ggeffects)
library(broom)
library(broom.mixed)
source("./syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical.RData")

ccahs_block_analytical_sem <- ccahs_block_analytical %>% 
  mutate(density_1 = standardize(poly(density, 2)[,1], 2), 
         density_2 = standardize(poly(density, 2)[,2], 2),
         DISADV_2000_1 = standardize(poly(DISADV_2000, 2)[,1], 2),
         DISADV_2000_2 = standardize(poly(DISADV_2000, 2)[,2], 2)) %>%
  filter(!is.na(CE_hlm_1995))


ccahs_block_analytical_sem %>% 
  select(CE_hlm_2001, CE_hlm_1995, DISADV_2000, AFFLUENCE_2000, HISP_FOR_2000, 
         DENSITY_2000, density, BE_pr_mean_bars_liquor_onstreet_block_2001, 
         BE_pr_vacant_onstreet_block_2001, BE_pr_abandoned_bld_onstreet_block_2001, 
         MIXED_LAND_USE_2001) %>% 
  rename_with(~str_remove_all(., "_2000|_2001|BE_pr_|_onstreet_block|mean_|_bld")) %>%
  na.omit() %>% cor() %>% round(2)

ccahs_block_analytical_sem %>% 
  select(CRIME_assault_battery_gun_2004_2006, CE_hlm_2001, BE_pr_abandoned_bld_onstreet_block_2001) %>% 
  mutate(CE_quant = ntile(x=CE_hlm_2001, n=4),
         Aband_quant = ntile(x=BE_pr_abandoned_bld_onstreet_block_2001,4)) %>%
  group_by(CE_quant, Aband_quant) %>%
  summarize(mean_gun_assaults = mean(CRIME_assault_battery_gun_2004_2006),
            n = n())

crime_dvs <- 
  c("CRIME_homicide_2004_2006",
    "CRIME_assault_battery_gun_2004_2006",
    "CRIME_robbery_2004_2006",
    "CRIME_violent_2004_2006",
    "CRIME_property_2004_2006")
controls <- "DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000"
ce_variants <- c("CE_2001", "CE_hlm_2001", "CE_avg_1995", "CE_hlm_1995")


gun_int_model <- glmer.nb(CRIME_assault_battery_gun_2004_2006 ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + 
           AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + 
           density_1 + density_2 + 
             BE_pr_mean_bars_liquor_onstreet_block_2001*CE_hlm_2001+  
           BE_pr_vacant_onstreet_block_2001*CE_hlm_2001 +  
             BE_pr_abandoned_bld_onstreet_block_2001*CE_hlm_2001 +  
             MIXED_LAND_USE_2001*CE_hlm_2001 + (1|ccahs_nc), 
         control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)), data = ccahs_block_analytical_sem)
ggpredict(gun_int_model, terms = c("BE_pr_abandoned_bld_onstreet_block_2001 [all]", "CE_hlm_2001")) %>% plot(facet=TRUE) + theme_minimal() + theme(legend.position = "bottom")


all_models <- paste0(paste0(rep(paste0(crime_dvs, " ~ ", controls), each = 4), " + ", ce_variants), rep(c("", " + DISADV_2000_2"), each = 20), " + (1|ccahs_nc)")

outcome    <- str_remove(all_models, " ~.*$")

all_models_out <- lapply(all_models, function(x) glmer.nb(formula(x), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)), data = ccahs_block_analytical_sem))
extract_relevant <- function(x){ 
  temp_model <- broom.mixed::tidy(x) 
  out_param  <- temp_model %>% 
    mutate(disadv_quad = any(term == "DISADV_2000_2"),
           outcome = names(x@frame)[1]) %>%
    filter(str_detect(term, "^CE_")) %>% 
    select(outcome, term, estimate, std.error, disadv_quad) %>%
    mutate(ce_sig = ifelse((abs(estimate) / std.error) > 1.96, "*", ""))
  return(out_param)
}

lapply(all_models_out[which(str_detect(all_models, "CE_hlm_2001") & str_detect(all_models, "DISADV_2000_2"))], tidy)

map_df(all_models_out, extract_relevant) %>%
  mutate(outcome = str_remove_all(outcome, "^CRIME_|_2004_2006$")) %>%
  mutate(across(where(is.numeric), ~round(.,2))) %>%
  mutate(estimate = as.character(glue::glue("{estimate} ({std.error}){ce_sig}"))) %>%
  mutate(term = case_when(
    term == "CE_2001"     ~ "CE_naive_2001",
    term == "CE_avg_1995" ~ "CE_naive_1995",
    TRUE ~ term)) %>%
  mutate(outcome = str_replace(outcome, "assault_battery_gun", "gun_assault"),
         ce_year = parse_number(term),
         ce_measure = str_remove_all(term, "^CE_|_2001$|_1995")) %>%
  select(outcome, ce_measure, ce_year, estimate, disadv_quad) %>%
  pivot_wider(names_from = outcome, values_from = estimate) %>% 
  arrange(desc(ce_year), desc(ce_measure), disadv_quad)

block_ivs <- " + density_1 + density_2 + BE_pr_mean_bars_liquor_onstreet_block_2001+  BE_pr_vacant_onstreet_block_2001 +  BE_pr_abandoned_bld_onstreet_block_2001 +  MIXED_LAND_USE_2001"
all_block_models <- paste0(all_models, block_ivs)
all_block_models_out <- lapply(all_block_models, function(x) glmer.nb(formula(x), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)), data = ccahs_block_analytical_sem))

map_df(all_block_models_out, extract_relevant) %>%
  mutate(outcome = str_remove_all(outcome, "^CRIME_|_2004_2006$")) %>%
  mutate(across(where(is.numeric), ~round(.,2))) %>%
  mutate(estimate = as.character(glue::glue("{estimate} ({std.error}){ce_sig}"))) %>%
  mutate(term = case_when(
    term == "CE_2001"     ~ "CE_naive_2001",
    term == "CE_avg_1995" ~ "CE_naive_1995",
    TRUE ~ term)) %>%
  mutate(outcome = str_replace(outcome, "assault_battery_gun", "gun_assault"),
         ce_year = parse_number(term),
         ce_measure = str_remove_all(term, "^CE_|_2001$|_1995")) %>%
  select(outcome, ce_measure, ce_year, estimate, disadv_quad) %>%
  pivot_wider(names_from = outcome, values_from = estimate) %>% 
  arrange(desc(ce_year), desc(ce_measure), disadv_quad)

summary(glmer.nb(CRIME_homicide_2004_2006            ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_assault_battery_gun_2004_2006 ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_robbery_2004_2006             ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_violent_2004_2006 ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_property_2004_2006 ~ CE_hlm_2001 + DISADV_2000_1 + DISADV_2000_2 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))

summary(glmer.nb(CRIME_homicide_2004_2006            ~ CE_2001 + DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_assault_battery_gun_2004_2006 ~ CE_2001 + DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_robbery_2004_2006             ~ CE_2001 + DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_violent_2004_2006             ~ CE_2001 + DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
summary(glmer.nb(CRIME_property_2004_2006            ~ CE_2001 + DISADV_2000_1 + AFFLUENCE_2000 + HISP_FOR_2000 + DENSITY_2000 + (1|ccahs_nc), data = ccahs_block_analytical_sem, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))))
