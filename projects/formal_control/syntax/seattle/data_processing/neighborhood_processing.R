library(tidyverse)
library(areal)
source("./syntax/project_functions.R")

nbhd_orig <- haven::read_spss("./data/seattle/raw/full data plus resids.sav")

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
         # violent_crime_rate_90                  = vcr90,
         # concentrated_disadvantage_90           = condis90,
         # concentrated_affluence_90              = conaff90,
         # density_90                             = density90,
         # ethnicity_immigration_90               = conimm90,
         # residential_stability_90               = resstab90,
         # population_90                          = p1_1_90,
         population_00                          = p1_1a)

save(nbhd, file="./data/seattle/derived/nbhd.RData")

# load("./data/seattle/derived/seattle_tract.RData")

# nbhd_geo <- seattle_tract %>%
#   right_join(nbhd %>%
#                mutate(TRACT_11 = paste0(53033, str_pad(TRACT, 6, side = "left", "0"))), 
#              by = c("GEOID"="TRACT_11"))
# 
# save(nbhd_geo, file="./data/seattle/derived/nbhd_geo.RData")
# 
# # AREAL WEIGHT
# load("./data/seattle/derived/spd_beats_shapes.RData")
# 
# nbhd_beat <- aw_interpolate(beats_2015_2017, 
#                tid = beat, source = nbhd_geo, 
#                sid = GEOID, 
#                extensive = c("population_00"), 
#                intensive = c("collective_efficacy_02_03", "violent_crime_rate_03_05", 
#                              "concentrated_disadvantage_00", "ethnicity_immigration_00",
#                              "residential_stability_00", "density_00",
#                              "concentrated_affluence_00", "eb_pig_02_03", "eb_code_of_the_street_02_03",
#                              "eb_informal_control_02_03", "ce_cohesion_and_trust_02_03", "ce_informal_social_control_02_03"),
#                weight = "sum", output = "sf")
# 
# save(nbhd_beat, file="./data/seattle/derived/nbhd_beat.RData")
