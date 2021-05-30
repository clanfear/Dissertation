library(tidyverse)
library(lavaan)
source("./syntax/project_functions.R")

load("./data/derived/sncs_indiv.RData")

# sncs_specification_4 <- ' 
# EFF_gov_pol ~ COHTR + EXCH + CLOSE + ISC_ob
# EFF_group_assoc ~ COHTR + EXCH + CLOSE + ISC_ob
# ISC ~ COHTR + EXCH + CLOSE + ISC_ob
# SE ~ COHTR + EXCH + CLOSE + ISC_ob
# 
# EFF_gov_pol =~ PERC_EFF_government + PERC_EFF_police
# EFF_group_assoc =~ PERC_EFF_small_groups + PERC_EFF_neighb_assoc
# 
# SE =~  PERC_EFF_self + INTERVENE_noise
# 
# ISC =~ ISC_graffiti + ISC_skip + ISC_disrespect + ISC_fight
# ISC_ob =~ ISC_ob_graffiti + ISC_ob_skip + ISC_ob_disrespect + ISC_ob_fight
# ISC_graffiti   ~ ISC_ob_graffiti
# ISC_skip       ~ ISC_ob_skip
# ISC_disrespect ~ ISC_ob_disrespect
# ISC_fight      ~ ISC_ob_fight
# 
# COHTR =~ COHTR_help + COHTR_trust
# EXCH =~ EXCH_watch + EXCH_favor + EXCH_ask + EXCH_activities + EXCH_eat + EXCH_problem + EXCH_stop_chat
# CLOSE =~ CLOSE_watch + CLOSE_know
# '
# 
# sncs_cfa_4 <- cfa(sncs_specification_4, data=sncs_indiv, estimator = "PML", missing = "available.cases")

# sncs_specification_6 <- ' 
# PERC_EFF_government ~ COHTR + EXCH + CLOSE + ISC_ob
# PERC_EFF_small_groups ~ COHTR + EXCH + CLOSE + ISC_ob
# PERC_EFF_neighb_assoc ~ COHTR + EXCH + CLOSE + ISC_ob
# PERC_EFF_police ~ COHTR + EXCH + CLOSE + ISC_ob
# ISC ~ COHTR + EXCH + CLOSE + ISC_ob
# SE ~ COHTR + EXCH + CLOSE + ISC_ob
# 
# SE =~  PERC_EFF_self + INTERVENE_noise
# 
# ISC =~ ISC_graffiti + ISC_skip + ISC_disrespect + ISC_fight
# ISC_ob =~ ISC_ob_graffiti + ISC_ob_skip + ISC_ob_disrespect + ISC_ob_fight
# ISC_graffiti   ~ ISC_ob_graffiti
# ISC_skip       ~ ISC_ob_skip
# ISC_disrespect ~ ISC_ob_disrespect
# ISC_fight      ~ ISC_ob_fight
# 
# COHTR =~ COHTR_help + COHTR_trust
# EXCH =~ EXCH_watch + EXCH_favor + EXCH_ask + EXCH_activities + EXCH_eat + EXCH_problem + EXCH_stop_chat
# CLOSE =~ CLOSE_watch + CLOSE_know
# '

sncs_specification <- ' 
ISC            =~ ISC_graffiti + ISC_skip + ISC_disrespect + ISC_fight
ISC_ob         =~ ISC_ob_graffiti + ISC_ob_skip + ISC_ob_disrespect + ISC_ob_fight
ISC_graffiti   ~ ISC_ob_graffiti
ISC_skip       ~ ISC_ob_skip
ISC_disrespect ~ ISC_ob_disrespect
ISC_fight      ~ ISC_ob_fight

COHTR =~ COHTR_help + COHTR_trust
EXCH  =~ EXCH_watch + EXCH_favor + EXCH_ask + EXCH_activities + EXCH_eat + EXCH_problem + EXCH_stop_chat
CLOSE =~ CLOSE_watch + CLOSE_know
'


sncs_cfa <- cfa(sncs_specification, data=sncs_indiv, estimator = "PML", missing = "available.cases")

summary(sncs_cfa, fit.measures = TRUE)

sncs_cfa_fs <- lavPredict(sncs_cfa, method="ML", type = "lv", append.data = FALSE)
# sncs_cfa_6_ov <- lavPredict(sncs_cfa_6, method="ML", type = "ov", append.data = TRUE)
# sncs_cfa_6_yhat <- lavPredict(sncs_cfa_6, method="ML", type = "yhat", ETA = sncs_cfa_6_fs)

sncs_fs     <- cbind(sncs_indiv, sncs_cfa_fs)

#--
lmer_ISC <- lmer(ISC ~ SES + FEMALE + AGE + MARRIAGE + African_American + 
                                   Hispanic + Asian + Homeowner + N_Moves + YRSATADDRESS + 
                                   VICT_viol_2 + VICT_prop_2 + (1|TRACT_1980),
                                 data=sncs_fs)
lmer_COHTR <- lmer(COHTR ~ SES + FEMALE + AGE + MARRIAGE + African_American + 
                                   Hispanic + Asian + Homeowner + N_Moves + YRSATADDRESS + 
                                   VICT_viol_2 + VICT_prop_2 + (1|TRACT_1980),
                                 data=sncs_fs)
lmer_EXCH <- lmer(EXCH ~ SES + FEMALE + AGE + MARRIAGE + African_American + 
                                   Hispanic + Asian + Homeowner + N_Moves + YRSATADDRESS + 
                                   VICT_viol_2 + VICT_prop_2 + (1|TRACT_1980),
                                 data=sncs_fs)
lmer_CLOSE <- lmer(CLOSE ~ SES + FEMALE + AGE + MARRIAGE + African_American + 
                                   Hispanic + Asian + Homeowner + N_Moves + YRSATADDRESS + 
                                   VICT_viol_2 + VICT_prop_2 + (1|TRACT_1980),
                                 data=sncs_fs)


sncs_neighb_eb <- sncs_fs %>% 
  group_by(TRACT_1980) %>%
  summarize(across(c(VICT_viol_2, VICT_prop_2, starts_with("BE_")), 
                   ~ mean(., na.rm=TRUE)),
            across(c(PERC_EFF_small_groups, 
                     PERC_EFF_neighb_assoc, PERC_EFF_police, PERC_EFF_government, 
                     PERC_EFF_self, PHYDIS_vacant, SOCDIS_teens), 
                   ~ mean(as.numeric(.), na.rm=TRUE))) %>%
  left_join(extract_EB_res(lmer_ISC, "ISC", "TRACT_1980")) %>%
  left_join(extract_EB_res(lmer_COHTR, "COHTR", "TRACT_1980")) %>%
  left_join(extract_EB_res(lmer_EXCH, "EXCH", "TRACT_1980")) %>%
  left_join(extract_EB_res(lmer_CLOSE, "CLOSE", "TRACT_1980")) %>%
    mutate(CE = (ISC + COHTR)/2) %>%
  rename_with(~paste0(., "_00"), -TRACT_1980)
sncs_neighb_eb <- sncs_neighb_eb %>%
  rbind(sncs_neighb_eb %>% filter(TRACT_1980 == 2500) %>% mutate(TRACT_1980 = 3700)) # Tract 3700 was eaten by 2500 in year 2000. Must duplicate or combine. Duplicated here.

sncs_neighb_eb <- sncs_neighb_eb %>% mutate(across(-TRACT_1980, ~ standardize(.)))
sncs_neighb_eb %>% select_if(is.numeric) %>% cor()

save(sncs_neighb_eb, file = "./data/derived/sncs_neighb_eb.RData")


