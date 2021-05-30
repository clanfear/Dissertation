

nc_block_model <- function(outcome){
  form_nc <- paste0(outcome, " ~ CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + (1|ccahs_nc)")
  form_block <- paste0(form_nc, " + BE_eb_bars_liquor_block_2001 + BE_pr_vacant_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001")
  model_nc <- glmer.nb(formula(form_nc),
                          data = ccahs_block_analytical,
                       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

  model_block <- glmer.nb(formula(form_block),
                          data = ccahs_block_analytical,
                          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  rbind(
    tidy(model_nc, effects = "fixed") %>% select(term, estimate, std.error) %>% mutate(model = paste0(outcome, "_nc")),
    tidy(model_block, effects = "fixed") %>% select(term, estimate, std.error) %>% mutate(model = paste0(outcome, "_block"))
  )
}

hom_1_glmer <- glmer.nb(homicide_2004_2006 ~ 
                          CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + (1|ccahs_nc),
                        data = ccahs_block_analytical, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

hom_2_glmer <- glmer.nb(homicide_2004_2006 ~ 
                          CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000 + (1|ccahs_nc) +
                          BE_eb_bars_liquor_block_2001 + BE_pr_vacant_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001,
                        data = ccahs_block_analytical, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


lapply(list(hom_1_glmer, hom_2_glmer), broom::tidy)


sem_base <- "~ c*CE_2001 + b1*BE_eb_bars_liquor_block_2001 + b2*BE_pr_vacant_onstreet_block_2001 + b3*BE_pr_abandoned_bld_onstreet_block_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000
           
           # mediators
             BE_eb_bars_liquor_block_2001 ~ a1*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000
             BE_pr_vacant_onstreet_block_2001 ~ a2*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000
             BE_pr_abandoned_bld_onstreet_block_2001 ~ a3*CE_2001 + DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000

           # indirect effect (a*b)
             ab1 := a1*b1
             ab2 := a2*b2
             ab3 := a3*b3
             
           # total effect
             total := c + (a1*b1) + (a2*b2) + (a3*b3)
             perc := (total-c) / total
             
             BE_eb_bars_liquor_block_2001 ~~ BE_pr_vacant_onstreet_block_2001 + BE_pr_abandoned_bld_onstreet_block_2001
             BE_pr_vacant_onstreet_block_2001 ~~ BE_pr_abandoned_bld_onstreet_block_2001
             CE_2001 ~~ DISADV_2000 + HISP_FOR_2000 + RESSTAB_2000 + POP_2000
             DISADV_2000 ~~ HISP_FOR_2000 + RESSTAB_2000 + POP_2000
             HISP_FOR_2000 ~~ RESSTAB_2000 + POP_2000
             RESSTAB_2000 ~~ POP_2000
"

hom_ml_sem <- paste0("l_homicide_2004_2006", sem_base)
summary(sem(hom_ml_sem, data = ccahs_block_analytical))