library(tidyverse)
library(lavaan)
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/nc_analytical_wide.RData")

lm(PE_hlm_2001 ~ PE_hlm_1995  + CE_hlm_1995 , data = nc_analytical_wide) %>% summary()
+ FAC_disadv_2000  + + LOG_HOM_RATE_1995   + FAC_hispimm_2000
crosslag_data <- nc_analytical_wide %>% 
  select(matches("^(PE|CE|LOG_HOM_RATE|VICT|FAC|TE|AT|KT|DENSITY)")) %>% 
  mutate(across(everything(), ~ standardize(.)))

sem_hom_current_formula <- "
  LOG_HOM_RATE_2003 ~ PE_hlm_2001 + CE_hlm_2001 + LOG_HOM_RATE_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 
    PE_hlm_2001 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
    CE_hlm_2001 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + TE_hlm_2001 

  LOG_HOM_RATE_1995 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    PE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + AT_hlm_1995
    CE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + TE_hlm_1995 + AT_hlm_1995

  PE_hlm_1995 ~~ CE_hlm_1995
  PE_hlm_2001 ~~ CE_hlm_2001"
sem(sem_hom_current_formula, data = crosslag_data, estimator = "ML", 
    missing = "ml.x", se = "boot", test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)

sem(sem_hom_current_formula, data = crosslag_data, estimator = "ML", 
    missing = "ml.x", se = "boot", test = "Satorra.Bentler") %>% modindices() %>% arrange(desc(mi))

sem_vict_current_formula <- "
  VICT_hlm_2001 ~ PE_hlm_2001 + CE_hlm_2001 + VICT_hlm_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 
    PE_hlm_2001 ~ PE_hlm_1995 + CE_hlm_1995 + VICT_hlm_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
    CE_hlm_2001 ~ PE_hlm_1995 + CE_hlm_1995 + VICT_hlm_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + TE_hlm_2001 

  VICT_hlm_1995 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    PE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + AT_hlm_1995
    CE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + AT_hlm_1995 + TE_hlm_1995

  PE_hlm_1995 ~~ CE_hlm_1995
  PE_hlm_2001 ~~ CE_hlm_2001"
sem(sem_vict_current_formula, data = crosslag_data, estimator = "ML", missing = "ml.x", se = "boot",test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)

# STRUCTURAL

sem_hom_current_iv_struct_formula <- "
  LOG_HOM_RATE_2003 ~ PE_hlm_2001 + CE_hlm_2001 + LOG_HOM_RATE_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
    PE_hlm_2001 ~ PE_hlm_1995 + ce*CE_hlm_1995 + LOG_HOM_RATE_1995 + dpe*FAC_disadv_2000 + spe*FAC_stability_2000 + hpe*FAC_hispimm_2000
    CE_hlm_2001 ~ pe*PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1995 + dce*FAC_disadv_2000 + sce*FAC_stability_2000 + hce*FAC_hispimm_2000 + TE_hlm_2001 

    FAC_disadv_2000    ~ ped*PE_hlm_1995 + ced*CE_hlm_1995 + LOG_HOM_RATE_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_stability_2000 ~ pes*PE_hlm_1995 + ces*CE_hlm_1995 + LOG_HOM_RATE_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_hispimm_2000   ~ peh*PE_hlm_1995 + ceh*CE_hlm_1995 + LOG_HOM_RATE_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_disadv_2000    ~~ FAC_stability_2000 + FAC_hispimm_2000
    FAC_stability_2000 ~~ FAC_hispimm_2000

  LOG_HOM_RATE_1995 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    PE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + AT_hlm_1995
    CE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + TE_hlm_1995 + AT_hlm_1995

  PE_hlm_1995 ~~ CE_hlm_1995
  PE_hlm_2001 ~~ CE_hlm_2001"
sem(sem_hom_current_iv_struct_formula, data = nc_analytical_wide, estimator = "ML", missing = "ml.x", se = "boot") %>% summary(fit.measures=TRUE)

sem_vict_current_iv_struct_formula <- "
  VICT_hlm_2001 ~ PE_hlm_2001 + CE_hlm_2001 + VICT_hlm_1995 + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
    PE_hlm_2001 ~ PE_hlm_1995 + ce*CE_hlm_1995 + VICT_hlm_1995 + dpe*FAC_disadv_2000 + spe*FAC_stability_2000 + hpe*FAC_hispimm_2000
    CE_hlm_2001 ~ pe*PE_hlm_1995 + CE_hlm_1995 + VICT_hlm_1995 + dce*FAC_disadv_2000 + sce*FAC_stability_2000 + hce*FAC_hispimm_2000 + TE_hlm_2001 

    FAC_disadv_2000    ~ ped*PE_hlm_1995 + ced*CE_hlm_1995 + VICT_hlm_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_stability_2000 ~ pes*PE_hlm_1995 + ces*CE_hlm_1995 + VICT_hlm_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_hispimm_2000   ~ peh*PE_hlm_1995 + ceh*CE_hlm_1995 + VICT_hlm_1995 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    FAC_disadv_2000    ~~ FAC_stability_2000 + FAC_hispimm_2000
    FAC_stability_2000 ~~ FAC_hispimm_2000

  VICT_hlm_1995 ~ PE_hlm_1995 + CE_hlm_1995 + LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
    PE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + AT_hlm_1995
    CE_hlm_1995 ~ LOG_HOM_RATE_1990 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990 + TE_hlm_1995 + AT_hlm_1995

  TotCEtoPE := ce + (dpe*ced) +  (hpe*ceh) + (spe*ces)
  TotPEtoCE := pe + (dce*ped) +  (hce*peh) + (sce*pes)


  PE_hlm_1995 ~~ CE_hlm_1995
  PE_hlm_2001 ~~ CE_hlm_2001"
sem(sem_vict_current_iv_struct_formula, data = nc_analytical_wide, estimator = "ML", missing = "ml.x") %>% summary(fit.measures=TRUE)
