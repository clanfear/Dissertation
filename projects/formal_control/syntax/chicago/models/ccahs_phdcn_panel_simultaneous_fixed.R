library(tidyverse)
library(lavaan)
source("../shared/syntax/project_functions.R")
load("./data/chicago/derived/nc_analytical_wide.RData")

simultaneous_data <- nc_analytical_wide %>% 
  select(matches("^(PE|CE|LOG_HOM_RATE|VICT|FAC|TE|AT|KT)")) %>% 
  mutate(across(everything(), ~ standardize(.)))




# ROSS:
## (1) Try sensitivity analysis: Fix error correlation to small non-zero, then increase up to large plausible value.

# JERRY:
## (1) Fix PE -> CE from 0 to high and see how it works out.
## (2) Try ratio of paths to see if constraint buys ID


ce_pe_hom_iv_nocrime <-
  "
  PE_hlm_1995        ~ CE_hlm_1995 + LOG_HOM_RATE_1990 + AT_hlm_1995                 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  CE_hlm_1995        ~ LOG_HOM_RATE_1990 + TE_hlm_1995 + AT_hlm_1995                 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990

  PE_hlm_2001        ~ CE_hlm_2001 + PE_hlm_1995 + LOG_HOM_RATE_1995  + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  CE_hlm_2001        ~ CE_hlm_1995  + LOG_HOM_RATE_1995 + TE_hlm_2001  + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000

  PE_hlm_1995 ~~ CE_hlm_1995
  CE_hlm_2001 ~~ PE_hlm_2001
"

sem(ce_pe_hom_iv_nocrime, data = simultaneous_data, estimator = "ML", missing = "ML",
    se = "bootstrap", test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)

ce_pe_hom_iv_nocrime_crosslag <-
  "
  PE_hlm_1995        ~ CE_hlm_1995 + LOG_HOM_RATE_1990 + AT_hlm_1995                 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990
  CE_hlm_1995        ~ LOG_HOM_RATE_1990 + TE_hlm_1995 + AT_hlm_1995                 + FAC_disadv_1990 + FAC_stability_1990 + FAC_hispimm_1990

  PE_hlm_2001        ~ CE_hlm_2001  + CE_hlm_1995 + PE_hlm_1995 + LOG_HOM_RATE_1995  + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000
  CE_hlm_2001        ~ CE_hlm_1995  + PE_hlm_1995 + LOG_HOM_RATE_1995 + TE_hlm_2001  + FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000

  PE_hlm_1995 ~~ CE_hlm_1995
  CE_hlm_2001 ~~ PE_hlm_2001
"

sem(ce_pe_hom_iv_nocrime_crosslag, data = simultaneous_data, estimator = "ML", 
    se = "robust.sem", test = "Satorra.Bentler") %>% summary(fit.measures=TRUE)