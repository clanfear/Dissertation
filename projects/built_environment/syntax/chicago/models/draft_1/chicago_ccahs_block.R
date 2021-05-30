library(tidyverse)
library(lme4)
library(lavaan)
library(broom.mixed)
source("./syntax/project_functions.R")
load("./data/chicago/derived/ccahs_block.RData")
load("./data/chicago/derived/phdcn_ccahs_nc.RData")


library(DHARMa)

ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

# Should look at other bar / liquor store measures

phdcn_measures <- phdcn_ccahs_nc %>%
  select(NC_ID, INF_1995, COHTR_1995) %>%
  mutate(CE_1995 = standardize((standardize(INF_1995) + standardize(COHTR_1995))/2, 2))

ccahs_block_analytical <- ccahs_block %>%
  mutate(across(where(is.character), ~ factor(.))) %>%
  mutate(BE_pr_mean_bars_liquor_onstreet_block_2001 = (BE_pr_bar_onstreet_block_2001 + BE_pr_liquor_onstreet_block_2001)/2 ) %>%
  mutate(across(c(starts_with("BE"), -matches("_eb_")), ~ as.numeric(. > 0), .names = "{.col}_bin")) %>%
  mutate(across(c(starts_with("CRIME"), starts_with("BE"), population, area, density, -matches("_eb_"), MIXED_LAND_USE_2001), ~ ihs(.), .names = "l_{.col}")) %>%
  mutate(across(c(starts_with("CRIME"), starts_with("BE"), population, area, density, -matches("_eb_|l_"), MIXED_LAND_USE_2001), ~ sqrt(.), .names = "sq_{.col}")) %>%
  mutate(across(c(CE_2001, DISADV_2000, HISP_FOR_2000, POP_2000, DENSITY_2000, MIXED_LAND_USE_2001, AFFLUENCE_2000,
                  starts_with("BE"), -matches("_bin"), 
                  population, area, density, starts_with("l_"), starts_with("sq_")), ~ standardize(., scale = 2))) %>%
  left_join(phdcn_measures, by = c("phdcn_nc" = "NC_ID"))



save(ccahs_block_analytical, file = "./data/chicago/derived/ccahs_block_analytical.RData")

dv_list <- c("CRIME_homicide_2004_2006",
             "CRIME_assault_battery_gun_2004_2006",
             "CRIME_robbery_2004_2006",
             "CRIME_violent_2004_2006",
             "CRIME_property_2004_2006")
             
iv_list <- c("CE_2001", 
        "DISADV_2000", 
        "AFFLUENCE_2000", 
        "HISP_FOR_2000", 
        "DENSITY_2000", 
        "sq_BE_pr_mean_bars_liquor_onstreet_block_2001", 
        "sq_BE_pr_vacant_onstreet_block_2001",
        "sq_BE_pr_abandoned_bld_onstreet_block_2001",
        "sq_MIXED_LAND_USE_2001",
        "poly(density,2)")

block_model <- function(dv, iv, type = "glmer.nb", tidy = TRUE, its = 2e5, df = ccahs_block_analytical){
  message("Fitting model on ", dv)
  if(type == "glmer.nb"){
    form <- formula(paste0(dv, " ~ ", paste0(iv, collapse= " + "), "+ (1|ccahs_nc)"))
    out  <- lme4::glmer.nb(form, data = df, control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=its)))
    if (tidy == TRUE){
      out <- filter(select(tidy(out), -effect, -group), term != "sd__(Intercept)")
    }
  } else if (type == "glm.nb"){
    form <- formula(paste0(dv, " ~ ", paste0(iv, collapse= " + ")))
    out  <- MASS::glm.nb(formula = form, data = df)
    if (tidy == TRUE){
      out <- tidy(out)
    }
  }
  out$outcome <- dv
  return(out)
}

glm_models_tidy <- map_dfr(dv_list, ~ block_model(., iv = iv_list, type = "glm.nb", tidy=TRUE))
glmer_models_tidy <- map_dfr(dv_list, ~ block_model(., iv = iv_list, type = "glmer.nb", tidy=TRUE))
glm_models <- map_dfr(dv_list, ~ block_model(., iv = iv_list, type = "glm.nb", tidy=FALSE))
glmer_models <- map_dfr(dv_list, ~ block_model(., iv = iv_list, type = "glmer.nb", tidy=FALSE))

save(glm_models_tidy, 
     glmer_models_tidy,
     glm_models, 
     glmer_models, 
     file = "./output/chicago/models/all_block_models.RData")




plot(simulateResiduals(hom_glmer_2, n = 1000))
plot(simulateResiduals(gun_glmer_2, n = 1000))
plotResiduals(simulateResiduals(gun_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)

plot(simulateResiduals(rob_glmer_2, n = 1000))
plotResiduals(simulateResiduals(rob_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)

plot(simulateResiduals(viol_glmer_2, n = 1000))
plotResiduals(simulateResiduals(viol_glmer_2, n = 1000), form = ccahs_block_analytical$DISADV_2000)
plot(simulateResiduals(prop_glmer_2, n = 1000))


models_ccahs_glmer_sq <- list("Gun Assault"  = gun_glmer_2, 
                             "Homicide"     = hom_glmer_2, 
                             "Robbery"      = rob_glmer_2, 
                             "Any Violent"  = viol_glmer_2, 
                             "Any Property" = prop_glmer_2)

save(models_ccahs_glmer_sq, file = "./output/chicago/models/models_ccahs_glmer_sq.RData")
