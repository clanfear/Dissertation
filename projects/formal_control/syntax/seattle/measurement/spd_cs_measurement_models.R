library(tidyverse)
library(lavaan)
library(lme4)
source("./syntax/project_functions.R")
load("./data/seattle/derived/spd_cs_measurement_data.RData")

pe_formula <-
"
police_efficacy =~ pe_neighbpolice_focuses_concerns + pe_dept_available + pe_dept_focuses_concerns + 
  pe_dept_understands_issues + pe_neighbpolice_overall + pe_neighbpolice_good_resource +
  pe_neighbpolice_parks

"
pe_cfa <- cfa(pe_formula, data = spd_cs_measurement_data, estimator = "PML", missing = "pairwise")

summary(pe_cfa, fit.measures=TRUE)

pe_factor_scores <- cbind(lavPredict(pe_cfa, method="ML"), spd_cs_measurement_data)

pe_lmer <- lmer(police_efficacy ~ dispensation + call_type + race + gender + pe_police_change + (1|beat:year),
                   data=pe_factor_scores)

lme_reliability(pe_lmer)

eb_pe_beat_year <- extract_EB_res(pe_lmer, "police_efficacy", "beat:year")  %>%
  separate(`beat:year`, c("beat", "year"), ":", convert = TRUE) %>%
  as_tibble()

save(eb_pe_beat_year, file = "./data/seattle/derived/eb_pe_beat_year.RData")

