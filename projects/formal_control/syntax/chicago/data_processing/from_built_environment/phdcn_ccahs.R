library(dplyr)
load("./data/chicago/derived/ccahs_nc.RData")
load("./data/chicago/derived/phdcn_nc.RData")
`%!in%` <- Negate(`%in%`)

phdcn_crosswalk <- haven::read_sav("F:/SecureData/Matsueda-tract_linknc.sav")
load("./data/chicago/derived/ccahs_nc_tract_crosswalk.RData")

phdcn_crosswalk <- phdcn_crosswalk %>% mutate_all(~as.character(.))
ccahs_nc_tract_crosswalk <- ccahs_nc_tract_crosswalk %>% mutate_all(~as.character(.))



nc_tract_crosswalk <- full_join(phdcn_crosswalk, ccahs_nc_tract_crosswalk, by = c("tract"="TRACT_ID"))

nc_tract_crosswalk <- nc_tract_crosswalk %>% 
  distinct(tract, link_nc) %>% 
  left_join(nc_tract_crosswalk %>% filter(!is.na(NC_ID)) %>% 
              distinct(link_nc, NC_ID)) %>%
  mutate(ccahs_nc = NC_ID, phdcn_nc = link_nc)

save(nc_tract_crosswalk, file = "./data/chicago/derived/nc_tract_crosswalk.RData")

nc_nc_crosswalk <- nc_tract_crosswalk %>% 
  distinct(phdcn_nc, ccahs_nc)



# We lose CCAHS NC 102 / PHDCN NC 792 because there are no data for the PHDCN
phdcn_ccahs_nc <- phdcn_nc %>% left_join(nc_nc_crosswalk, by = c("NC_ID"="phdcn_nc")) %>% left_join(ccahs_nc, by = c("ccahs_nc" = "NC_ID"))

save(phdcn_ccahs_nc, file = "./data/derived/phdcn_ccahs_nc.RData")

