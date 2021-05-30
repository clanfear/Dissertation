library(tidyverse)
library(lavaan)
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/nc_analytical_wide.RData")

simultaneous_data <- nc_analytical_wide %>% 
  select(matches("^(PE|CE|LOG_HOM_RATE|VICT|FAC|TE|AT|KT)")) %>% 
  mutate(across(everything(), ~ standardize(.)))

ce_pe_hom_both <-
  "
  PE_hlm_1995        ~ b*CE_hlm_1995 + LOG_HOM_RATE_1990 + AT_hlm_1995                              + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  CE_hlm_1995        ~ PE_hlm_1995 + LOG_HOM_RATE_1990 + TE_hlm_1995 + AT_hlm_1995                + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  LOG_HOM_RATE_1995  ~ c*PE_hlm_1995 + a*CE_hlm_1995  + LOG_HOM_RATE_1990                             + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990

  PE_hlm_2001        ~ e*CE_hlm_2001  + PE_hlm_1995 + LOG_HOM_RATE_1995                             + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  CE_hlm_2001        ~ PE_hlm_2001  + CE_hlm_1995 + LOG_HOM_RATE_1995 + TE_hlm_2001               + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  LOG_HOM_RATE_2003  ~ f*PE_hlm_2001  + d*CE_hlm_2001  + LOG_HOM_RATE_1995                            + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000

  tot95 := a + (b*c)
  tot03 := d + (e*f)
"
sem(ce_pe_hom_both, data = simultaneous_data, estimator = "ML", missing = "ml.x", 
    se = "boot", test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)

ce_pe_vict_both <-
  "
  PE_hlm_1995        ~ b*CE_hlm_1995 + LOG_HOM_RATE_1990 + AT_hlm_1995                              + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  CE_hlm_1995        ~ PE_hlm_1995 + LOG_HOM_RATE_1990 + TE_hlm_1995 + AT_hlm_1995                + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  VICT_hlm_1995  ~ c*PE_hlm_1995 + a*CE_hlm_1995  + LOG_HOM_RATE_1990                             + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990

  PE_hlm_2001        ~ e*CE_hlm_2001  + PE_hlm_1995 + VICT_hlm_1995                             + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  CE_hlm_2001        ~ PE_hlm_2001  + CE_hlm_1995 + VICT_hlm_1995 + TE_hlm_2001               + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  VICT_hlm_2001  ~ f*PE_hlm_2001  + d*CE_hlm_2001  + VICT_hlm_1995                            + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  
  tot95 := a + (b*c)
  tot03 := d + (e*f)
"
sem(ce_pe_vict_both, data = simultaneous_data, estimator = "ML", missing = "ml.x", 
    se = "boot", test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)

# EXCLUSION

# This is just to show in most raw form that model is identified with crosslags
# and reciprocal effects at same time due to TE (Neighboring) instrument.
ce_pe_hom_both_exc <-
  "
  PE_hlm_1995        ~ CE_hlm_1995 
  CE_hlm_1995        ~ PE_hlm_1995  + TE_hlm_1995 

  PE_hlm_2001        ~ CE_hlm_2001  + PE_hlm_1995 + CE_hlm_1995 
  CE_hlm_2001        ~ PE_hlm_2001  + CE_hlm_1995 + PE_hlm_1995 + TE_hlm_2001
"
sem(ce_pe_hom_both_exc, data = simultaneous_data, estimator = "ML", test = "mean.var.adjusted") %>% summary(fit.measures=TRUE)
