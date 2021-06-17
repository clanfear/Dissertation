library(tidyverse)
library(lavaan)
library(lme4)
library(mgcv)
library(DHARMa)
library(piecewiseSEM)
library(ggeffects)
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical.RData")


# Models:

# (1) Direct effect NB models
#    (1a) With no past CE effect
#    (1b) WIth past CE effect
# (2) BE models
# (3) CE model
dvs <- c("CRIME_homicide_2004_2006",
         "CRIME_assault_battery_gun_2004_2006",
         "CRIME_robbery_2004_2006",
         "CRIME_violent_2004_2006",
         "CRIME_property_2004_2006")
ce_1995 <- "CE_hlm_1995"
ce_2001 <- "CE_hlm_2001"
ivs_nc <- c("FAC_disadv_2000",
            "FAC_stability_2000", 
            "FAC_hispimm_2000", 
            "density_ltdb_nc_2000")
be_block <- c("BE_pr_bar_onstreet_block_2001",
              "BE_pr_liquor_onstreet_block_2001",
              "BE_pr_vacant_onstreet_block_2001",
              "BE_pr_abandoned_bld_onstreet_block_2001",
              "BE_pr_commer_dest_onstreet_block_2001",
              "BE_pr_recreation_block_2001",
              "BE_pr_parking_block_2001",
              "MIXED_LAND_USE_2001")
be_block_int <- paste0(be_block, "*", "CE_hlm_2001")
be_block_int_disadv <- paste0(be_block, "*", "FAC_disadv_2000")
density_block <- c("density_block",
                   "density_block_2")
res <- "(1|ccahs_nc)"

main_hlm_formulas            <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block, density_block, res), collapse = " + "))
main_hlm_formulas_nore       <- str_remove(main_hlm_formulas, " \\+ \\(1\\|ccahs_nc\\)")
int_hlm_formulas             <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block_int, density_block, res), collapse = " + "))
int_hlm_formulas_nore        <- str_remove(int_hlm_formulas, " \\+ \\(1\\|ccahs_nc\\)")
int_hlm_formulas_disadv      <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, be_block_int_disadv, density_block, res), collapse = " + "))
int_hlm_formulas_nore_disadv <- str_remove(int_hlm_formulas_disadv, " \\+ \\(1\\|ccahs_nc\\)")
main_hlm_formulas_nobe            <- paste0(dvs, " ~ ", paste(c(ce_2001, ivs_nc, density_block, res), collapse = " + "))
main_hlm_formulas_nobe_nore       <- str_remove(main_hlm_formulas_nobe, " \\+ \\(1\\|ccahs_nc\\)")

ce_2000_formula         <- paste0(ce_2001, " ~ ", paste(c(ce_1995, ivs_nc, be_block, density_block), collapse = " + "))
be_formulas             <- paste0(be_block, " ~ ", paste(c(ce_1995, ivs_nc, density_block, res), collapse = " + "))


interaction_second_stage_list <- list(
  "Homicide"    =   MASS::glm.nb(formula(int_hlm_formulas_nore[1]), data = ccahs_block_analytical),
  "Gun Assault" =   glmer.nb(formula(int_hlm_formulas[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Robbery"     =   glmer.nb(formula(int_hlm_formulas[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Violent"     =   glmer.nb(formula(int_hlm_formulas[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Property"    =   glmer.nb(formula(int_hlm_formulas[5]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
)
save(interaction_second_stage_list, file = "./data/chicago/derived/interaction_second_stage_list.RData")

interaction_disadv_second_stage_list <- list(
  "Homicide"    =   MASS::glm.nb(formula(int_hlm_formulas_nore_disadv[1]), data = ccahs_block_analytical),
  "Gun Assault" =   glmer.nb(formula(int_hlm_formulas_disadv[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Robbery"     =   glmer.nb(formula(int_hlm_formulas_disadv[3]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Violent"     =   glmer.nb(formula(int_hlm_formulas_disadv[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6))),
  "Property"    =   glmer.nb(formula(int_hlm_formulas_disadv[5]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
)
save(interaction_disadv_second_stage_list, file = "./data/chicago/derived/interaction_disadv_second_stage_list.RData")

# Interaction plot
library(ggeffects)
gun_int_model <- glmer.nb(formula(int_hlm_formulas[2]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
  ggpredict(gun_int_model, terms = c("BE_pr_abandoned_bld_onstreet_block_2001", "CE_hlm_2001[-1,1]")) %>% plot()

vio_int_model <- glmer.nb(formula(int_hlm_formulas[4]), data = ccahs_block_analytical, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e6)))
  ggpredict(vio_int_model, terms = c("BE_pr_abandoned_bld_onstreet_block_2001", "CE_hlm_2001[-1,1]")) %>% plot()
  