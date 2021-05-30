library(tidyverse)
source("./syntax/project_functions.R")

load("./data/chicago/derived/ccahs_nc.RData")
load("./data/chicago/derived/phdcn_nc.RData")
load("./data/chicago/derived/cpd_crimes_tract.RData")

load("./data/chicago/derived/nc_nc_crosswalk.RData")
load("./data/chicago/derived/ccahs_tract_crosswalk.RData")



# CCAHS -> crime data for one set of years -> 

# CRIME
ccahs_crime <- cpd_crimes_tract %>% 
  filter(year == 2001) %>% 
  select(-year) %>% 
  rename_with(~ paste0(., "_2001"), -TRACT) %>%
  full_join(cpd_crimes_tract %>% 
              filter(year == 2004) %>% 
              select(-year) %>% 
              rename_with(~ paste0(., "_2004"), -TRACT), by = "TRACT") %>%
  inner_join(ccahs_tract_crosswalk, by = "TRACT") %>%
  group_by(NC_ID) %>%
  summarize(across(-matches("TRACT|ID", ignore.case = FALSE), ~ sum(.))) %>%
  right_join(ccahs_nc, by = "NC_ID") %>%
  rename_with(~ paste0(., "_2001"), c(-matches("[0-9]$"), -NC_ID))

phdcn_ccahs_nc <- phdcn_nc %>%
  rename_with(~ paste0(., "_1995"), c(-matches("[0-9]$"), -NC_ID)) %>%
  left_join(nc_nc_crosswalk, by = c("NC_ID"="phdcn_nc")) %>% 
  left_join(ccahs_crime, by = c("ccahs_nc" = "NC_ID"))

save(phdcn_ccahs_nc, file = "./data/chicago/derived/phdcn_ccahs_nc.RData")



# We lose CCAHS NC 102 / PHDCN NC 792 because there are no data for the PHDCN


