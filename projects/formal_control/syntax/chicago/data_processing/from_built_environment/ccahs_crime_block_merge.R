library(tidyverse)
source("./syntax/project_functions.R")

load("./data/chicago/derived/ccahs_nc_no_sso.RData")
load("./data/chicago/derived/cpd_crimes_block.RData")
load("./data/chicago/derived/crosswalks/complete_crosswalk.RData")
load("./data/chicago/derived/ccahs_sso_block.RData")
load("./data/chicago/derived/block_pop_estimates_2000.RData")



ccahs_block <- complete_crosswalk %>%
  select(ccahs_nc, census_tract_6, census_block, ccahs_tract, ccahs_sso_block) %>%
  inner_join(ccahs_sso_block, by = c("ccahs_tract" = "TRACT_ID" , "ccahs_sso_block" = "BLOCK_ID")) %>%
  group_by(ccahs_nc, census_tract_6, census_block, ccahs_tract) %>%
  summarize(across(-ccahs_sso_block, ~mean(.)), n_sso_blocks = n_distinct(ccahs_sso_block)) %>%
  ungroup() %>%
  left_join(cpd_crimes_block %>% 
              filter(year == 2004) %>% 
              select(-year) %>% 
              rename_with(~ paste0(., "_2004"), -c(census_tract_6, census_block))) %>%
  left_join(cpd_crimes_block %>% 
              filter(year %in% 2004:2006) %>% 
              group_by(census_tract_6, census_block) %>% 
              summarize(across(matches("CRIME"), ~sum(.))) %>%
              rename_with(~ paste0(., "_2004_2006"), -c(census_tract_6, census_block))) %>%
  rename_with(~ paste0(., "_2001"), -c(matches("[0-9]$"), ccahs_nc, census_tract_6, census_block, ccahs_tract, n_sso_blocks)) %>%
  inner_join(ccahs_nc_no_sso, by = c("ccahs_nc" = "NC_ID")) %>%
  inner_join(block_pop_estimates_2000)


# CCAHS -> crime data for one set of years -> 
ccahs_block %>% list_missing()


save(ccahs_block, file = "./data/chicago/derived/ccahs_block.RData")



# We lose CCAHS NC 102 / PHDCN NC 792 because there are no data for the PHDCN


