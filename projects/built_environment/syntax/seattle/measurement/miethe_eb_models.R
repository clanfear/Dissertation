library(tidyverse)
library(lavaan)
library(lme4)

source("./syntax/project_functions.R")
load("./data/derived/miethe_indiv.RData")

miethe_indiv <- miethe_indiv %>%
  mutate(across(c(TIES_block, ORGS_neighborhood), ~ ordered(as_factor(.))))

miethe_indiv %>% 
  select(matches("(EXCH|CLOSE|PHYDIS|SOCDIS|SES|VICT_viol_2|VICT_prop_2|TRACT_1980|TIES_block|ORGS_neighborhood)")) %>% 
  group_by(TRACT_1980) %>%
  mutate_all(as.numeric) %>%
  summarize(across(everything(), ~mean(., na.rm=TRUE))) %>% 
  cor(use = "pairwise.complete.obs")

# SES dropped from model: Income and education don't hang together well.

# MIETHE_specification <- '
# EXCH =~ EXCH_favor + EXCH_watch + EXCH_problem
# CLOSE =~ CLOSE_recognize + CLOSE_know
# DIS =~ PHYDIS_litter + PHYDIS_graffiti + PHYDIS_vacant
# 
# EXCH  ~~ CLOSE + DIS
# CLOSE ~~ DIS
# '

# Doing a single factor for social capital

miethe_specification <- '
SOCCAP =~ EXCH_watch + EXCH_favor + EXCH_problem + EXCH_eat + EXCH_watch_now + CLOSE_know + TIES_block + ORGS_neighborhood
'

miethe_cfa <- cfa(miethe_specification, data=miethe_indiv, estimator = "PML", missing = "available.cases")
summary(miethe_cfa, fit.measures = TRUE)


miethe_cfa_fs <- lavPredict(miethe_cfa)
miethe_fs     <- cbind(miethe_indiv, miethe_cfa_fs)



# Calculate empirical bayes residuals for tracts

lmer_soccap     <- lmer(SOCCAP ~ SES_education + SES_income + FEMALE + AGE + MARRIAGE + African_American + 
                          Other_Race + Homeowner + NCHILDREN + YRSINNEIGHB + VICT_viol_2 + VICT_prop_2 + (1|TRACT_1980),
                        data = miethe_fs)

# Extract residuals and export

miethe_fs %>%
  group_by(TRACT) %>%
  summarize(across(starts_with("VICT"), ~ mean(as.numeric(as.character(.)), na.rm=TRUE)))

miethe_neighb_eb <- extract_EB_res(lmer_soccap, "eb_soccap_90", "TRACT_1980") %>%
  left_join(miethe_fs %>%
             # mutate(TRACT = as.character(TRACT)) %>%
              group_by(TRACT_1980) %>%
              summarize(across(matches("VICT_|SOCDIS_teens|PHYDIS_vacant"), ~ mean(as.numeric(as.character(.)), na.rm=TRUE)), across(starts_with("BE_"), ~mean(., na.rm=TRUE))) %>%
              mutate(TRACT_1980 = as.character(TRACT_1980))) %>%
  rename_with(~paste0(., "_90"), .cols = matches("VICT_|BE_|SOCDIS_teens|PHYDIS_vacant"))

miethe_neighb_eb <- miethe_neighb_eb %>% mutate(across(-TRACT_1980, ~ standardize(.)))

save(miethe_neighb_eb, file = "./data/derived/miethe_neighb_eb.RData")
