library(tidyverse)
source("./syntax/project_functions.R")

nbhd_orig <- haven::read_spss("./data/raw/full data plus resids.sav")

load("./data/derived/miethe_neighb_eb.RData")

nbhd <- nbhd_orig %>% 
  select(TRACT = tract,
         ce_reciprocated_exchange_02_03         = ce_exch,
         ce_intergenerational_closure_02_03     = ce_close,
         ce_cohesion_and_trust_02_03            = ce_cohtr,
         ce_closure_and_exchange_02_03          = ce_ace,
         ce_informal_social_control_02_03       = ce_insc,
         ce_child_centered_social_control_02_03 = ce_ccsc,
         collective_efficacy_02_03              = colleff,
         social_disorder_02_03                  = socdis,
         physical_disorder_02_03                = physdis,
         violent_crime_rate_03_05               = vcr0305,
         violent_crime_count_03_05              = vcc0305,
         robbery_rate_0305                      = rbr0305,
         property_victimization_02_03           = propvict,
         violent_victimization_02_03            = violvict,
         eb_obligations_expectations_02_03      = oblex_ebinr,
         eb_informal_control_02_03              = insc_ebinr,
         eb_code_of_the_street_02_03            = code_ebinr,
         eb_pig_02_03                           = pig_ebinr,
         concentrated_disadvantage_00           = condis,
         concentrated_affluence_00              = conaff2,
         ethnicity_immigration_00               = conimm,
         residential_stability_00               = resstab,
         density_00                             = density,
         violent_crime_rate_90                  = vcr90,
         concentrated_disadvantage_90           = condis90,
         concentrated_affluence_90              = conaff90,
         density_90                             = density90,
         ethnicity_immigration_90               = conimm90,
         residential_stability_90               = resstab90,
         population_90                          = p1_1_90,
         population_00                          = p1_1a)

save(nbhd, file="./data/derived/nbhd.RData")

combine_tracts <- function(old_tract, new_tracts, nbhd_data){
  combined <- cbind(TRACT=old_tract, nbhd_data %>% 
          filter(TRACT %in% new_tracts) %>% 
          select(-TRACT) %>% 
          summarize_all(mean))
  return(combined)
}

# MERGING NEIGHB INTO INDIV

nbhd_1990 <- rbind(nbhd ,
   combine_tracts(400, c(401,402), nbhd),
   combine_tracts(8000, c(8001,8002), nbhd),
   combine_tracts(9700, c(9701,9702), nbhd)) %>%
  mutate(TRACT_1980 = as.character(TRACT)) %>%
  select(-TRACT)

nbhd_1990 <- rbind(nbhd_1990, nbhd_1990 %>% 
                     filter(TRACT_1980 == 2500) %>% 
                     mutate(TRACT_1980 = 3700)) %>% 
  filter(TRACT_1980 %in% miethe_neighb_eb$TRACT_1980)

nbhd_1990 <- nbhd_1990 %>%
  mutate(across(c(-TRACT_1980, -population_90, -population_00, -violent_crime_count_03_05), ~standardize(.)))

save(nbhd_1990, file="./data/derived/nbhd_1990.RData")

# ADJUSTING TRACTS 2000
# nbhd_2000 <- nbhd %>% select(TRACT, ends_with("2000"), ends_with("0305"), ends_with("9698"), ends_with("9901"))
# 
# nbhd_2000 <- rbind(nbhd_2000 %>% 
#                      filter(TRACT %in% Miethe_indiv$TRACT),
#                    combine_tracts(400, c(401,402), nbhd_2000),
#                    combine_tracts(8000, c(8001,8002), nbhd_2000),
#                    combine_tracts(9700, c(9701,9702), nbhd_2000))
# 
# if(any(nbhd_2000$TRACT %!in% nbhd_1990$TRACT)){
#   warning(paste("TRACT MISMATCH:", nbhd_2000$TRACT[nbhd_2000$TRACT %!in% nbhd_1990$TRACT]))
# }
# 
# if(any(nbhd_1990$TRACT %!in% nbhd_2000$TRACT)){
#   warning(paste("TRACT MISMATCH:", nbhd_1990$TRACT[nbhd_1990$TRACT %!in% nbhd_2000$TRACT]))
# }
