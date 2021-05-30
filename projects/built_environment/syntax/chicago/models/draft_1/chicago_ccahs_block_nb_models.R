library(tidyverse)
library(lme4)
library(lavaan)
library(broom.mixed)
source("./syntax/project_functions.R")

load("./data/chicago/derived/ccahs_block_analytical.RData")

dv_list <- 
  c("Homicide"     = "CRIME_assault_battery_gun_2004_2006",
    "Gun Assault"  = "CRIME_homicide_2004_2006",
    "Robbery"      = "CRIME_robbery_2004_2006",
    "Any Violent"  = "CRIME_violent_2004_2006",
    "Any Property" = "CRIME_property_2004_2006")
             
iv_list <- 
  c("CE_2001", 
    "DISADV_2000", 
    "AFFLUENCE_2000", 
    "HISP_FOR_2000", 
    "DENSITY_2000", 
    "sq_BE_pr_mean_bars_liquor_onstreet_block_2001", 
    "sq_BE_pr_vacant_onstreet_block_2001",
    "sq_BE_pr_abandoned_bld_onstreet_block_2001",
    "sq_MIXED_LAND_USE_2001",
    "poly(density,2)")

block_model <- function(dv, iv, type = "glmer.nb", tidy = TRUE, its = 2e5, 
                        df = ccahs_block_analytical, outcome = NULL){
  if(is.null(outcome)){
    outcome <- dv
  }
  message("Fitting model on ", dv)
  if(type == "glmer.nb"){
    form <- formula(paste0(dv, " ~ ", 
                           paste0(iv, collapse= " + "), 
                           "+ (1|ccahs_nc)"))
    out  <- lme4::glmer.nb(form, data = df, control = 
                             glmerControl(optimizer="bobyqa", 
                                          optCtrl=list(maxfun=its)))
    if (tidy == TRUE){
      out <- filter(select(tidy(out), -effect, -group), 
                    term != "sd__(Intercept)")
      out$outcome <- outcome
    }
  } else if (type == "glm.nb"){
    form <- formula(paste0(dv, " ~ ", paste0(iv, collapse= " + ")))
    out  <- MASS::glm.nb(formula = form, data = df)
    if (tidy == TRUE){
      out <- tidy(out)
      out$outcome <- outcome
    }
  }
  return(out)
}

run_block_models <- function(dv_list, iv, type = "glmer.nb", tidy = TRUE, 
                             its = 2e5, df = ccahs_block_analytical){
  lookup <- setNames(names(dv_list), dv_list)
  if (tidy==TRUE){
    map_dfr(dv_list, ~ block_model(., iv=iv, type = type, tidy = tidy, 
                                   its = its, df = df, outcome = lookup[.]))
  } else if (tidy==FALSE){
    set_names(map(dv_list, ~ block_model(., iv=iv, type = type, tidy = tidy, 
                                         its = its, df = df)),
              names(dv_list))
  }
}

# Not the most efficient way; could do untidy -> tidy to halve computation time

glm_models_tidy         <- run_block_models(dv_list, iv = iv_list, type = "glm.nb",   tidy = TRUE)
glm_models              <- run_block_models(dv_list, iv = iv_list, type = "glm.nb",   tidy = FALSE)
glmer_models_tidy       <- run_block_models(dv_list, iv = iv_list, type = "glmer.nb", tidy = TRUE)
glmer_models            <- run_block_models(dv_list, iv = iv_list, type = "glmer.nb", tidy = FALSE)
glmer_models_polydisadv <- run_block_models(dv_list, iv = str_replace(iv_list, "DISADV_2000", "poly(DISADV_2000,2)"), type = "glmer.nb", tidy = FALSE)

save(glm_models_tidy, 
     glmer_models_tidy,
     glm_models, 
     glmer_models, 
     glmer_models_polydisadv,
     file = "./output/chicago/models/all_block_models.RData")
