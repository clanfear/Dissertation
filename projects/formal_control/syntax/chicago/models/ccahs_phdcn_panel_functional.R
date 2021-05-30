# Hokay, so let's make some infrastructure to run every model specification
# sequentially provided just a formula.

library(tidyverse)
library(fixest)
library(lme4)
library(lavaan)
library(mgcv)
library(broom.mixed)
source("./syntax/chicago/models/panel_model_estimates_function.R")
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/nc_analytical_wide.RData")
load("./data/chicago/derived/nc_analytical_long.RData")
load("../shared/data/chicago/derived/nc_boundaries.RData")

outcomes  <- c("Homicide" = "CNT_MURDER", "Victimization" = "VICT_hlm", "Violent Crime" = "VIOLENT_CRIME", "Perceived Violence" = "PV_hlm")
iv_specs  <- c("Base" = "CE_hlm + PE_hlm + FAC_disadv + FAC_stability + FAC_hispimm + DENSITY + year",
               "Year Int" = "CE_hlm*year + PE_hlm*year +  FAC_disadv + FAC_stability + FAC_hispimm + DENSITY + year",
               "CE x PE" = "CE_hlm*PE_hlm*year +  FAC_disadv + FAC_stability + FAC_hispimm + DENSITY + year")
fit_specs <- c("RE", "FE", "Lag RE", "MRF", "MRF Polydis", "IV")

model_specifications <- expand_grid(dv = outcomes, iv_formula = iv_specs, fit_spec = fit_specs) %>%
  filter(!(dv=="CNT_MURDER" & fit_spec=="FE")) %>%
  filter(!(fit_spec == "IV" & iv_formula!=iv_specs["Base"])) %>%
  mutate(iv_spec_name = invert(iv_specs)[iv_formula],
         outcome = invert(outcomes)[dv],
         model_formula = paste0(dv, " ~ ", iv_formula)) %>%
  select(outcome, fit_spec, iv_spec_name, model_formula) 

all_models <- model_specifications %>%
  mutate(models = map2(.$model_formula, .$fit_spec, ~run_model(.x, .y)))

save(all_models, file = "./output/all_models.RData")

