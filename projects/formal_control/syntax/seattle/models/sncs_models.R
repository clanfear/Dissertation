library(tidyverse)
load("./data/seattle/derived/sncs_indiv.RData")
load("./data/seattle/derived/nbhd.RData")


summary(lm(violent_victimization_02_03 ~ collective_efficacy_02_03 + eb_pig_02_03 + eb_code_of_the_street_02_03 + concentrated_disadvantage_00 + concentrated_affluence_00 + ethnicity_immigration_00 + residential_stability_00 + density_00, data = nbhd))
cor(nbhd)
