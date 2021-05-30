library(tidyverse)
library(lavaan)
source("./syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block_analytical.RData")


sem_sqrt_mediators_1995_both  <- 
  "
   # Second stage
   l_CRIME_homicide_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_assault_battery_gun_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_robbery_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_violent_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_property_2004_2006 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
        
   # First Stage
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_BE_pr_vacant_onstreet_block_2001           ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_BE_pr_abandoned_bld_onstreet_block_2001    ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_MIXED_LAND_USE_2001                        ~ CE_2001 + CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2

   # CE Stability and covariances
   CE_2001 ~ CE_1995
   CE_2001 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   CE_1995 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   
   # Structural covariances
   DISADV_2000   ~~ HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   HISP_FOR_2000 ~~ RESSTAB_2000 + POP_2000 + density_1 + density_2
   RESSTAB_2000  ~~ POP_2000 + density_1 + density_2
   POP_2000      ~~ density_1 + density_2
   density_1 ~~ density_2
   
   # BE covariances
   sq_MIXED_LAND_USE_2001                        ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~~ sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_vacant_onstreet_block_2001           ~~ sq_BE_pr_abandoned_bld_onstreet_block_2001
"
sem_sqrt_mediators_1995_both_fit <- sem(sem_sqrt_mediators_1995_both,
                                        estimator = "ML", # WLSMV?
                                        se = "bootstrap",
                                        test = "bootstrap",
                                        std.ov=TRUE,
                                        data = ccahs_block_analytical %>% 
                                          mutate(density_1 = poly(density, 2)[,1], 
                                                 density_2 = poly(density, 2)[,2]))

summary(sem_sqrt_mediators_1995_both_fit, fit.measures=T)

tidy_params <- broom::tidy(sem_sqrt_mediators_1995_both_fit) %>%
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

tidy_params %>%  filter(str_detect(lhs, "BE|MIXED")) %>%
  mutate(p.value = ifelse(p.value < 0.05, "*", ""),
         std.error = paste0("(",round(std.error, 3), ")"),
         estimate = round(estimate, 3),
         lhs = str_remove_all(lhs, "l_CRIME_|_2004_2006"),
         lhs = str_remove_all(lhs, "(^.*mean_)|(^.*pr_)|(_onstreet.*$)|(_20.*$)|(^sq_)")) %>%
  mutate(estimate = paste(estimate, std.error, p.value)) %>%
  select(-std.error, -p.value) %>%
  pivot_wider(names_from = lhs, values_from = estimate)

sem_sqrt_mediators_1995_only_2001  <- 
  "
   # Second stage
   l_CRIME_homicide_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_assault_battery_gun_2004_2006 ~ CE_2001  + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_robbery_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_violent_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001

   l_CRIME_property_2004_2006 ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2 +
             sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
        
   # First Stage
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_BE_pr_vacant_onstreet_block_2001           ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_BE_pr_abandoned_bld_onstreet_block_2001    ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   sq_MIXED_LAND_USE_2001                        ~  CE_1995 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2

   # CE Stability and covariances
   CE_2001 ~ CE_1995
   CE_2001 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   CE_1995 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   
   # Structural covariances
   DISADV_2000   ~~ HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + density_1 + density_2
   HISP_FOR_2000 ~~ RESSTAB_2000 + POP_2000 + density_1 + density_2
   RESSTAB_2000  ~~ POP_2000 + density_1 + density_2
   POP_2000      ~~ density_1 + density_2
   density_1 ~~ density_2
   
   # BE covariances
   CE_2001 ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001 + sq_MIXED_LAND_USE_2001
   sq_MIXED_LAND_USE_2001                        ~~ sq_BE_pr_mean_bars_liquor_onstreet_block_2001 + sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_mean_bars_liquor_onstreet_block_2001 ~~ sq_BE_pr_vacant_onstreet_block_2001 + sq_BE_pr_abandoned_bld_onstreet_block_2001
   sq_BE_pr_vacant_onstreet_block_2001           ~~ sq_BE_pr_abandoned_bld_onstreet_block_2001
"
sem_sqrt_mediators_1995_only_2001_fit <- sem(sem_sqrt_mediators_1995_only_2001,
            estimator = "ML", # WLSMV?
            se = "bootstrap",
            test = "bootstrap",
            std.ov=TRUE,
            data = ccahs_block_analytical %>% 
              mutate(density_1 = poly(density, 2)[,1], 
                     density_2 = poly(density, 2)[,2]))

summary(sem_sqrt_mediators_1995_only_2001_fit, fit.measures=T)


tidy_params <- broom::tidy(sem_sqrt_mediators_1995_only_2001_fit) %>%
  filter(op == "~") %>%
  select(term, estimate, std.error, p.value) %>%
  separate(term, c("lhs", "rhs"), sep = " ~ ") %>%
  filter(str_detect(rhs, "CE|BE|MIXED"))

modificationindices(sem_sqrt_mediators_1995_only_2001_fit) %>% tibble() %>% arrange(desc(mi))

tidy_params %>%  filter(str_detect(lhs, "CRIME")) %>%
  mutate(p.value = ifelse(p.value < 0.05, "*", ""),
         std.error = paste0("(",round(std.error, 3), ")"),
         estimate = round(estimate, 3),
         lhs = str_remove_all(lhs, "l_CRIME_|_2004_2006"),
         rhs = str_remove_all(rhs, "(^.*mean_)|(^.*pr_)|(_onstreet.*$)|(_20.*$)|(^sq_)")) %>%
  mutate(estimate = paste(estimate, std.error, p.value)) %>%
  select(-std.error, -p.value) %>%
  pivot_wider(names_from = lhs, values_from = estimate)

tidy_params %>%  filter(str_detect(lhs, "BE|MIXED")) %>%
  mutate(p.value = ifelse(p.value < 0.05, "*", ""),
         std.error = paste0("(",round(std.error, 3), ")"),
         estimate = round(estimate, 3),
         lhs = str_remove_all(lhs, "l_CRIME_|_2004_2006"),
         lhs = str_remove_all(lhs, "(^.*mean_)|(^.*pr_)|(_onstreet.*$)|(_20.*$)|(^sq_)")) %>%
  mutate(estimate = paste(estimate, std.error, p.value)) %>%
  select(-std.error, -p.value) %>%
  pivot_wider(names_from = lhs, values_from = estimate)

anova(sem_sqrt_mediators_1995_both_fit, sem_sqrt_mediators_1995_only_2001_fit)
