library(tidyverse)
library(lavaan)
source("./syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical.RData")

ccahs_block_analytical_sem <- ccahs_block_analytical %>% 
  mutate(density_1 = standardize(poly(density, 2)[,1]), 
         density_2 = standardize(poly(density, 2)[,2]))

mediator_specification <- 
 "# Second stage
   l_CRIME_homicide_2004_2006 ~ 
     hom_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
     hom_bar*sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + 
     hom_vac*sq_BE_pr_vacant_onstreet_block_2001 + 
     hom_aba*sq_BE_pr_abandoned_bld_onstreet_block_2001 + 
     hom_mix*sq_MIXED_LAND_USE_2001

   l_CRIME_assault_battery_gun_2004_2006 ~ 
     gun_ce*CE_2001  + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
     gun_bar*sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + 
     gun_vac*sq_BE_pr_vacant_onstreet_block_2001 + 
     gun_aba*sq_BE_pr_abandoned_bld_onstreet_block_2001 + 
     gun_mix*sq_MIXED_LAND_USE_2001

   l_CRIME_robbery_2004_2006 ~ 
     rob_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
     rob_bar*sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + 
     rob_vac*sq_BE_pr_vacant_onstreet_block_2001 + 
     rob_aba*sq_BE_pr_abandoned_bld_onstreet_block_2001 + 
     rob_mix*sq_MIXED_LAND_USE_2001

   l_CRIME_violent_2004_2006 ~ 
     vio_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
     vio_bar*sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + 
     vio_vac*sq_BE_pr_vacant_onstreet_block_2001 + 
     vio_aba*sq_BE_pr_abandoned_bld_onstreet_block_2001 + 
     vio_mix*sq_MIXED_LAND_USE_2001

   l_CRIME_property_2004_2006 ~ 
     pro_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
     pro_bar*sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + 
     pro_vac*sq_BE_pr_vacant_onstreet_block_2001 + 
     pro_aba*sq_BE_pr_abandoned_bld_onstreet_block_2001 + 
     pro_mix*sq_MIXED_LAND_USE_2001

  # First Stage
    sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~ bar_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
    sq_BE_pr_vacant_onstreet_block_2001           ~ vac_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
    sq_BE_pr_abandoned_bld_onstreet_block_2001    ~ aba_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
    sq_MIXED_LAND_USE_2001                        ~ mix_ce*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
 
  # Covariances  
    # Antecedents
      CE_2001       ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
      DISADV_2000   ~~ HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
      HISP_FOR_2000 ~~ RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
      RESSTAB_2000  ~~ DENSITY_2000 + density_1 + density_2
      DENSITY_2000      ~~ density_1 + density_2
      density_1     ~~ density_2

    # Built Environment
      sq_MIXED_LAND_USE_2001                        ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
      sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~~ sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
      sq_BE_pr_vacant_onstreet_block_2001           ~~ sq_BE_pr_abandoned_bld_onstreet_block_2001

  # Indirect Effects
    # Homicide
    hom_bar_ind := bar_ce*hom_bar
    hom_vac_ind := vac_ce*hom_vac
    hom_aba_ind := aba_ce*hom_aba
    hom_mix_ind := mix_ce*hom_mix

    # Gun Assaults
    gun_bar_ind := bar_ce*gun_bar
    gun_vac_ind := vac_ce*gun_vac
    gun_aba_ind := aba_ce*gun_aba
    gun_mix_ind := mix_ce*gun_mix

    # Robbery
    rob_bar_ind := bar_ce*rob_bar
    rob_vac_ind := vac_ce*rob_vac
    rob_aba_ind := aba_ce*rob_aba
    rob_mix_ind := mix_ce*rob_mix

    # Any Violence
    vio_bar_ind := bar_ce*vio_bar
    vio_vac_ind := vac_ce*vio_vac
    vio_aba_ind := aba_ce*vio_aba
    vio_mix_ind := mix_ce*vio_mix

    # Property
    pro_bar_ind := bar_ce*pro_bar
    pro_vac_ind := vac_ce*pro_vac
    pro_aba_ind := aba_ce*pro_aba
    pro_mix_ind := mix_ce*pro_mix
 
  # Total Effects
    # Homicide
    hom_indirect  := hom_bar_ind + hom_vac_ind + hom_aba_ind + hom_mix_ind
    hom_total     := hom_ce + hom_indirect
    hom_perc      := (hom_total-hom_ce) / hom_total
    hom_diff      := hom_total-hom_ce

    # Gun Assaults
    gun_indirect  := gun_bar_ind + gun_vac_ind + gun_aba_ind + gun_mix_ind
    gun_total     := gun_ce + gun_indirect
    gun_perc      := gun_indirect / gun_total
    gun_diff      := gun_total-gun_ce

    # Robbery
    rob_indirect  := rob_bar_ind + rob_vac_ind + rob_aba_ind + rob_mix_ind
    rob_total     := rob_ce + rob_indirect
    rob_perc      := rob_indirect / rob_total
    rob_diff      := rob_total-rob_ce

    # Any Violence
    vio_indirect  := vio_bar_ind + vio_vac_ind + vio_aba_ind + vio_mix_ind
    vio_total     := vio_ce + vio_indirect
    vio_perc      := vio_indirect / vio_total
    vio_diff      := vio_total-vio_ce

    # Property
    pro_indirect  := pro_bar_ind + pro_vac_ind + pro_aba_ind + pro_mix_ind
    pro_total     := pro_ce + pro_indirect
    pro_perc      := pro_indirect / pro_total
    pro_diff      := pro_total-pro_ce
"

lagged_restricted_specification  <- 
  "
  # Second stage
   l_CRIME_homicide_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_assault_battery_gun_2004_2006 ~ CE_2001  + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_robbery_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_violent_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_property_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
        
  # First Stage
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_BE_pr_vacant_onstreet_block_2001           ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_BE_pr_abandoned_bld_onstreet_block_2001    ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_MIXED_LAND_USE_2001                        ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2

  # CE Stability and covariances
   CE_2001 ~ CE_1995
   CE_2001 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   CE_1995 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   
  # Structural covariances
   DISADV_2000   ~~ HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   HISP_FOR_2000 ~~ RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   RESSTAB_2000  ~~ DENSITY_2000 + density_1 + density_2
   DENSITY_2000      ~~ density_1 + density_2
   density_1 ~~ density_2
   
  # BE covariances
   CE_2001 ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
   sq_MIXED_LAND_USE_2001                        ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~~ sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_vacant_onstreet_block_2001           ~~ sq_BE_pr_abandoned_bld_onstreet_block_2001
"

lagged_unrestricted_specification  <- 
  "
  # Second stage
   l_CRIME_homicide_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_assault_battery_gun_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_robbery_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_violent_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_property_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
        
  # First Stage
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_BE_pr_vacant_onstreet_block_2001           ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_BE_pr_abandoned_bld_onstreet_block_2001    ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   sq_MIXED_LAND_USE_2001                        ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2

  # CE Stability and covariances
   CE_2001 ~ CE_1995
   CE_2001 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   CE_1995 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   
  # Structural covariances
   DISADV_2000   ~~ HISP_FOR_2000 + RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   HISP_FOR_2000 ~~ RESSTAB_2000 + DENSITY_2000 + density_1 + density_2
   RESSTAB_2000  ~~ DENSITY_2000 + density_1 + density_2
   DENSITY_2000      ~~ density_1 + density_2
   density_1 ~~ density_2
   
  # BE covariances
   sq_MIXED_LAND_USE_2001                        ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~~ sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_vacant_onstreet_block_2001           ~~ sq_BE_pr_abandoned_bld_onstreet_block_2001
"

fit_block_sem <- function(spec){
  return(
    sem(
      spec,
      estimator = "ML", # WLSMV?
      se        = "bootstrap",
      test      = "bootstrap",
      bootstrap = 1000,
      std.ov    = TRUE,
      data      = ccahs_block_analytical_sem
      )
    )
}

mediator_models            <- fit_block_sem(mediator_specification)
lagged_restricted_models   <- fit_block_sem(lagged_restricted_specification)
lagged_unrestricted_models <- fit_block_sem(lagged_unrestricted_specification)

save(mediator_models, 
     lagged_restricted_models, 
     lagged_unrestricted_models, 
     file = "./output/chicago/models/all_block_sem_models.RData")
####

tidy_params <- broom::tidy(sem_sqrt_mediators_1995_only_2001_fit) %>%
  filter(op == "~") %>%
  select(term, estimate, std.error, p.value) %>%
  separate(term, c("lhs", "rhs"), sep = " ~ ") %>%
  filter(str_detect(rhs, "CE|BE|MIXED"))

tidy_params %>%  filter(str_detect(lhs, "CRIME")) %>%
  mutate(p.value = ifelse(p.value < 0.05, "*", ""),
         std.error = paste0("(",round(std.error, 3), ")"),
         estimate = round(estimate, 3),
         lhs = str_remove_all(lhs, "l_CRIME_|_2004_2006"),
         rhs = str_remove_all(rhs, "(^.*mean_)|(^.*pr_)|(_onstreet.*$)|(_20.*$)|(^sq_)")) %>%
  mutate(estimate = paste(estimate, std.error, p.value)) %>%
  select(-std.error, -p.value) %>%
  pivot_wider(names_from = lhs, values_from = estimate)