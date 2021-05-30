library(tidyverse)
library(sf)
source("./syntax/project_functions.R")

phdcn_nc_to_ccahs_tract <- haven::read_sav("F:/SecureData/Matsueda-tract_linknc.sav") %>% 
  mutate_all(~as.character(.)) %>%
  rename(phdcn_nc = link_nc, ccahs_tract = tract)
load("./data/chicago/derived/ccahs_survey_nc_tract.RData")
ccahs_nc_to_ccahs_tract <- ccahs_survey_nc_tract %>%
  rename(ccahs_nc = NC_ID, ccahs_tract = TRACT_ID)


ccahs_tract_to_nc <- 
  full_join(phdcn_nc_to_ccahs_tract, 
            ccahs_nc_to_ccahs_tract) %>% 
  group_by(phdcn_nc) %>% 
  arrange(ccahs_nc) %>%
  fill(ccahs_nc) %>% 
  ungroup()

nc_crosswalk <-  ccahs_tract_to_nc %>% distinct(ccahs_nc, phdcn_nc)
save(nc_crosswalk, file = "./data/chicago/derived/crosswalks/nc_crosswalk.RData")

#-----
# Match ccahs tracts to real tracts
il_tract_raw <- tigris::tracts("IL", cb=TRUE, year=1990)
il_tract <- il_tract_raw %>%
  mutate(TRACT = paste0(TRACTBASE, TRACTSUF),
         FIPSSTCO = paste0(STATEFP, COUNTYFP)) %>%
  filter(FIPSSTCO == "17031") %>%
  select(TRACT, TRACTBASE, TRACTSUF, geometry)

save(il_tract, file = "./data/chicago/derived/il_tract.RData")

tract_to_nc <- ccahs_tract_to_nc %>% 
  mutate(census_tract_4 = str_pad(ccahs_tract, 4, "left", 0)) %>%
  left_join(il_tract, by = c("census_tract_4"="TRACTBASE")) %>%
  select(-geometry, -TRACTSUF) %>% 
  rename(census_tract_6 = TRACT)
  
save(tract_to_nc, file = "./data/chicago/derived/crosswalks/tract_to_nc.RData")

#----
# Add blocks

# il_blockgroup <- tigris::block_groups("IL", cb=TRUE, year=1990) %>%
#   st_drop_geometry() %>%
#   filter(COUNTYFP == "031") %>%
#   select(census_tract_6 = TRACT, census_blockgroup = BG)

load("./data/chicago/derived/il_block.RData")

# il_blocks_2000 <- tigris::blocks("IL", "Cook", 2000)
# 
# il_cook_blocks_2000 <- il_blocks_2000 %>% filter(STATEFP==17 & COUNTYFP00 == "031")
#   
# x_notin_y(il_cook_blocks_2000$BLOCKCE00, ccahs_sso_ids)

load("./data/chicago/derived/ccahs_sso_ids.RData")
ccahs_sso_ids <- ccahs_sso_ids %>%
  transmute(ccahs_nc = as.character(NC_ID),
         census_block = str_sub(BLOCK_ID, 1, 3),
         census_blockgroup = str_sub(BLOCKGROUP_ID, -1,-1),
         census_tract_4 = str_pad(TRACT_ID, side= "left", pad = "0", width = 4),
         ccahs_tract = as.character(TRACT_ID),
         ccahs_sso_block = as.character(BLOCK_ID)) %>% 
  distinct()

complete_crosswalk <- il_block %>%
  st_drop_geometry() %>% 
  filter(FIPSSTCO == "17031") %>% 
  select(census_tract_6 = TRACT, census_block = BLOCK) %>% 
  right_join(tract_to_nc) %>%
  filter(!(is.na(ccahs_nc) & is.na(phdcn_nc))) %>% 
  mutate(census_blockgroup = str_sub(census_block, 1, 1)) %>%
  mutate(census_block = ifelse(census_tract_6 == "810400" & census_block %in% c("813A","813B"), "813", census_block)) %>%
  left_join(ccahs_sso_ids) %>%
  distinct()

save(complete_crosswalk, file = "./data/chicago/derived/crosswalks/complete_crosswalk.RData")
 