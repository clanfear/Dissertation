library(tidyverse)
library(lme4)
library(lavaan)
library(broom.mixed)
library(DHARMa)

load("./output/chicago/models/all_block_models.RData")

glmer_residuals <- map(glmer_models, ~ simulateResiduals(., n = 1000))

walk(glmer_residuals, ~ plot(.))

plot(glmer_residuals[[1]])

plot(simulateResiduals(hom_glmer_2, n = 1000))
plot(simulateResiduals(gun_glmer_2, n = 1000))
plotResiduals(simulateResiduals(gun_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)

plot(simulateResiduals(rob_glmer_2, n = 1000))
plotResiduals(simulateResiduals(rob_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)

plot(simulateResiduals(viol_glmer_2, n = 1000))
plotResiduals(simulateResiduals(viol_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)
plot(simulateResiduals(prop_glmer_2, n = 1000))