# This script models individual-level legal cynicism scales in 1995 and 2003.
# This is to explore why legal cynicism doesn't appear particularly stable or 
# related to anything at the NC-level.

library(tidyverse)
library(lme4)
source("../shared/syntax/project_functions.R")
# load("./data/chicago/derived/nc_analytical_wide.RData")
# load("./data/chicago/derived/nc_analytical_long.RData")


# Individual level
load("../shared/data/chicago/derived/phdcn_cs_individual_long.RData")
load("./data/chicago/derived/nc_analytical_wide.RData")
load("../shared/data/chicago/derived/ccahs_individual_long.RData")

test_data_2003 <- ccahs_individual_long %>%
  inner_join(nc_analytical_wide, by = c("NC_ID"= "ccahs_nc"))
test_data_1995 <- phdcn_cs_individual_long %>%
  inner_join(nc_analytical_wide, by = c("NC_ID"= "phdcn_nc"))

lmer_out_1995 <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sep_div + 
                        FAM_single + HOMEOWN + RACE_latino + RACE_black + 
                        MOBILITY + AGE + YRS_IN_NEIGHB + SES + 
                        FAC_disadv_1990 + FAC_hispimm_1990 + FAC_stability_1990 + CE_hlm_1995 + PE_hlm_1995 + LOG_HOM_RATE_1995 + (1|NC_ID/RESP_ID),  
                      ## Standardize all (including binary) predictors
                      data = test_data_1995 %>% 
                        filter(str_detect(measure, "LC_")) %>%
                        mutate(across(c(-value, -measure, -NC_ID, -RESP_ID), ~standardize(.))),
                      control = lmerControl(optimizer = "bobyqa"))
summary(lmer_out_1995)

lmer_out_2003 <- lmer(value ~ measure + FEMALE + FAM_married + FAM_sepdiv + 
                      FAM_single + HOMEOWN + RACE_latino + RACE_black + AGE + 
                      years_in_home + SES + 
                        FAC_disadv_2000 + FAC_stability_2000 + FAC_hispimm_2000 + CE_hlm_2001 + PE_hlm_2001 + LOG_HOM_RATE_2003 + (1|NC_ID/RESP_ID),  
                    ## Standardize all (including binary) predictors
                    data = test_data_2003 %>% 
                      filter(str_detect(measure, "LC_")) %>%
                      mutate(across(c(-value, -measure, -NC_ID, -RESP_ID, -BLOCK_ID, -TRACT_ID), ~standardize(.))),
                    control = lmerControl(optimizer = "bobyqa"))
summary(lmer_out_2003)
