library(tidyverse)
source("../shared/syntax/project_functions.R")

load("../shared/data/chicago/derived/CE_PE_LC_PV_eb_nc_1995.RData")
load("../shared/data/chicago/derived/CE_PE_LC_PV_eb_nc_2001.RData")
load("../shared/data/chicago/derived/ccahs_nc.RData")
load("../shared/data/chicago/derived/phdcn_nc.RData")
phdcn_nc <- phdcn_nc %>%
  select(NC_ID, CNT_MURDER_1989_1991, CNT_MURDER_1995, VIOLENT_CRIME_1995,
         LOG_HOM_RATE_1990, LOG_HOM_RATE_1990, HOM_RATE_1995, LOG_HOM_RATE_1995)
load("../shared/data/chicago/derived/cpd_crimes_nc_2003.RData")
load("../shared/data/chicago/derived/crosswalks/nc_crosswalk.RData")
load("../shared/data/chicago/derived/ltdb_factors_wide.RData")
load("../shared/data/chicago/derived/nc_neighbors.RData")

cpd_crimes_nc_2003 <- cpd_crimes_nc_2003 %>% 
  select(ccahs_nc, 
         CNT_MURDER_2003   = CRIME_homicide,
         CNT_VIOLENT_2003  = CRIME_violent,
         CNT_ALLCRIME_2003 = CRIME_all_crime)

# Need NC-level crime data
phdcn_ccahs_nc_wide <- nc_crosswalk %>%
  left_join(CE_PE_LC_PV_eb_nc_1995, by = "phdcn_nc") %>%
  left_join(CE_PE_LC_PV_eb_nc_2001, by = "ccahs_nc") %>%
  # left_join(ccahs_nc, by = c("ccahs_nc"  = "NC_ID")) %>%
  left_join(phdcn_nc, by = c("phdcn_nc" = "NC_ID")) %>%
  left_join(cpd_crimes_nc_2003, by = "ccahs_nc") %>%
  left_join(ltdb_factors_wide, by = c("ccahs_nc", "phdcn_nc"))

# Here I construct my own crime measures so they're uniform.
nc_analytical_wide <- phdcn_ccahs_nc_wide %>% 
  mutate(HOM_RATE_1990          = 100000*((CNT_MURDER_1989_1991/3)/population_ltdb_nc_2000),
         LOG_HOM_RATE_1990      = log(HOM_RATE_1990 + 1),
         HOM_RATE_1995          = 100000*(CNT_MURDER_1995/population_ltdb_nc_2000),
         LOG_HOM_RATE_1995      = log(HOM_RATE_1995 + 1),
         HOM_RATE_2003          = 100000*(CNT_MURDER_2003/population_ltdb_nc_2000),
         LOG_HOM_RATE_2003      = log(HOM_RATE_2003 + 1),
         ALLCRIME_RATE_2003     = 100000*(CNT_ALLCRIME_2003/population_ltdb_nc_2000),
         LOG_ALLCRIME_RATE_2003 = log(ALLCRIME_RATE_2003 + 1),
         VIOLENT_RATE_2003      = 100000*(CNT_VIOLENT_2003/population_ltdb_nc_2000),
         LOG_VIOLENT_RATE_2003  = log(VIOLENT_RATE_2003 + 1),
         DENSITY_1990           = density_ltdb_nc_1990,
         DENSITY_2000           = density_ltdb_nc_2000) %>% # Replacing included density measure, R=0.998
  mutate(across(-matches("(_nc$|^CNT|^pop|^FAC_|^LOG_HOM)"), ~ standardize(., 2)))

nc_splags <- nc_neighbors %>%
  mutate(neighbors = as.character(neighbors)) %>%
  inner_join(nc_analytical_wide %>% rename(neighbors = ccahs_nc)) %>%
  group_by(ccahs_nc) %>%
  summarize(across(matches("^(CE_|PE_|LOG_HOM_RATE_(2003|1995))"), ~ mean(.), .names = "splag_{.col}"))

nc_analytical_wide <- nc_analytical_wide %>% left_join(nc_splags)

save(nc_analytical_wide, file = "./data/chicago/derived/nc_analytical_wide.RData")

nc_analytical_long <- nc_analytical_wide %>%
  rename(VIOLENT_CRIME_2003    = VIOLENT_RATE_2003,
         LAG_LOG_HOM_RATE_1990 = LOG_HOM_RATE_1990,
         LAG_HOM_RATE_1990     = HOM_RATE_1990) %>%
  select(-CNT_MURDER_1989_1991, -LOG_VIOLENT_RATE_2003) %>%
  mutate(across(-matches("ccahs_nc|phdcn_nc|^CNT|^POP|^FAC_|^splag"), ~ standardize(.))) %>%
  select(ccahs_nc, matches("^(CE_(h|s)|PE_|LC_|PV_|TE_|KT_|VICT_hlm|LOG_H|HOM_R|PERC_V|VIOLENT_|DISADV|STABILITY|IMMIG|CNT_MURDER_(1995|2003)|LAG_|pop|DENSITY_|FAC_|splag)")) %>%
  pivot_longer(-ccahs_nc) %>%
  separate(name, into = c("name", "year"), sep = -5) %>%
  mutate(year = ifelse(str_detect(year, "19"), 0, 1)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(ccahs_nc) %>%
  mutate(LAG_HOM_RATE     = ifelse(year == 0, LAG_HOM_RATE, lag(HOM_RATE, order_by = year)),
         LAG_LOG_HOM_RATE = ifelse(year == 0, LAG_LOG_HOM_RATE, lag(LOG_HOM_RATE, order_by = year))) %>%
  ungroup()

save(nc_analytical_long, file = "./data/chicago/derived/nc_analytical_long.RData")
