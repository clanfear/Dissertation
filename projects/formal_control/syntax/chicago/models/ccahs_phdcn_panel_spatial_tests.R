library(tidyverse)
library(fixest)
library(lme4)
library(broom.mixed)
library(sf)
library(spdep)
source("./syntax/project_functions.R")
load("./data/chicago/derived/nc_analytical_wide.RData")
load("./data/chicago/derived/nc_analytical_long.RData")
load("./output/all_models.RData")
load("../shared/data/chicago/derived/nc_boundaries.RData")

quiet_aug <- quietly(augment)
extract_residuals <- function(model){
  if(is(model,"fixest")){ # fixest augment requires a data argument so bypassing it
    tibble(ccahs_nc = as.character(model$fixef_id$ccahs_nc), 
           residual = residuals(model), 
           year = rep(0:1, length.out = length(model$residuals)))
  } else {
    suppressMessages(
    suppressWarnings(
    model %>%
      augment() %>% 
      left_join(nc_analytical_long) %>%
      select(residual = .resid, ccahs_nc, year)
    )
    )
}}
all_morans <- nc_boundaries %>% 
  inner_join(
    all_models %>% 
      mutate(resid_df = map(models, extract_residuals)) %>% 
      unnest(resid_df)
    ) %>%
  group_by(outcome, year, fit_spec, iv_spec_name) %>%
  nest() %>% # Some model Ns are different due to missing homicide / violent crime outcomes so need dynamic neighbor lists
  mutate(morans = map(data, ~ moran.test(.$residual, nb2listw(poly2nb(.)), alternative = "two.sided"))) %>% 
  mutate(morans_pvalue = map_dbl(morans, ~ .x$p.value),
         morans_i = map_dbl(morans, ~.x$statistic)) 

# This kicks warnings but they're irrelevant
# So this takes the boundaries, joins them to a df of all model residuals, then
# does a moran's I using each analytical subsample's neighbors list, then pulls
# p values and I stats.
# I broke them up by year because it seemed like neighbors would be weird with
# repeat observations? But what do I know.

all_morans %>% 
  ungroup() %>%
  mutate(sig = morans_pvalue < .05) %>%
  arrange(desc(morans_pvalue)) %>%
  count(fit_spec, sig)

# Still some residual spatial autocorrelation in the violent crime models