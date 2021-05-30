extract_colnames <- function(x){
  str_remove_all(x, "(\\+|\\||~)*") %>%
    str_trim(.) %>%
    str_split(., "( |~|\\+|\\*)+") %>%
    unlist(.) %>%
    unique(.) %>%
    return(.)
}

form_filter <- function(df, formula){
  df %>%
    select(extract_colnames(formula), "ccahs_nc", "population_ltdb_nc") %>%
    group_by(ccahs_nc) %>%
    filter(if_all(everything(), ~ all(!is.na(.)))) %>%
    ungroup() %>%
    mutate(ccahs_nc = factor(ccahs_nc))
}

prep_gam_neighbors <- function(df){
  gam_boundaries <- semi_join(nc_boundaries, df, by = "ccahs_nc")
  neigh_list     <- spdep::poly2nb(gam_boundaries)
  names(neigh_list) <- gam_boundaries$ccahs_nc
  return(neigh_list)
}

run_model <- function(form, spec){
  current_formula       <- form
  current_specification <- spec
  if (current_specification == "Lag RE"){
    current_formula <- paste0(current_formula, " + LAG_LOG_HOM_RATE")
  } else if (str_detect(current_specification, "IV") & str_detect(current_formula, "CNT_MURDER")) {
    current_formula <- paste0(str_remove(str_replace(current_formula, "CNT_MURDER", "LOG_HOM_RATE"), "CE_hlm"), " | CE_hlm ~ TE_hlm + KT_hlm")
  } else if (str_detect(current_specification, "IV") & !str_detect(current_formula, "CNT_MURDER")) {
    current_formula <- paste0(str_remove(current_formula, "CE_hlm"), " | CE_hlm ~ TE_hlm + KT_hlm")
  }
  model_df <- form_filter(nc_analytical_long, current_formula)
  if (current_specification %in% c("RE", "Lag RE") & str_detect(current_formula, "CNT_MURDER")){
    model_formula <- formula(paste0(current_formula, "+ (1|ccahs_nc) + offset(log(population_ltdb_nc))"))
    current_model <- glmer(model_formula,
                           data = model_df, family = poisson,
                           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
  } else if (current_specification == "Lag RE" & str_detect(current_formula, "VIOLENT_CRIME") ){
    model_formula <- formula(current_formula)
    current_model <- lm(model_formula, data = model_df)
  } else if (current_specification %in% c("RE", "Lag RE")){
    model_formula <- formula(paste0(current_formula, "+ (1|ccahs_nc)"))
    current_model <- lmer(model_formula, data = model_df)
  } else if (current_specification == "FE") {
    model_formula <- formula(paste0(current_formula, "| ccahs_nc"))
    current_model <- feols(model_formula, data = model_df)
  } else if (str_detect(current_specification, "MRF") & str_detect(current_formula, "CNT_MURDER")) {
    if (current_specification == "MRF Polydis"){
      current_formula <- str_replace(current_formula, "FAC_disadv", "s(FAC_disadv)")
    } 
    model_formula <- formula(paste0(current_formula, "+ offset(log(population_ltdb_nc)) + s(ccahs_nc, bs = 'mrf', xt = list(nb = model_neighbs))"))
    model_neighbs <- prep_gam_neighbors(model_df)
    current_model <- gam(model_formula, data = model_df, family = nb, control = gam.control(nthreads = 4))
  } else if (str_detect(current_specification, "MRF") & !str_detect(current_formula, "CNT_MURDER")) {
    if (current_specification == "MRF Polydis"){
      current_formula <- str_replace(current_formula, "FAC_disadv", "s(FAC_disadv)")
    }
    model_formula <- formula(paste0(current_formula, "+ s(ccahs_nc, bs = 'mrf', xt = list(nb = model_neighbs))"))
    model_neighbs <- prep_gam_neighbors(model_df)
    current_model <- gam(model_formula, data = model_df, control = gam.control(nthreads = 4))
  } else if (str_detect(current_specification, "IV") & str_detect(current_formula, "LOG_HOM_RATE")) {
    model_formula <- formula(current_formula)
    current_model <- feols(model_formula, data = model_df, cluster = "ccahs_nc")
  } else if (str_detect(current_specification, "IV") & !str_detect(current_formula, "LOG_HOM_RATE")) {
    model_formula <- formula(current_formula)
    current_model <- feols(model_formula, data = model_df, fixef = "ccahs_nc")
  }
  return(current_model)
}

get_pred_mat <- function(iv_spec_name, model, ...){
  if (is(model, "lmerMod") | is(model,"glmerMod")){
    mod_coefs <- fixef(model)
  } else {
    mod_coefs <- coef(model)
  }
  param_names <- names(mod_coefs)
  if(is(model,"glmerMod") | is(model, "lmerMod") | is(model, "gam")){
    mod_vcov <- vcov(model)
  } else {
    mod_vcov <- sandwich::vcovHAC(model)
  }
  sim_params <- MASS::mvrnorm(10000, mod_coefs, mod_vcov)
  if (iv_spec_name == "CE x PE x Year") {
    scenarios <- c("Low CE Low PE 1995", "High CE Low PE 1995", "Low CE High PE 1995", "High CE High PE 1995",
                   "Low CE Low PE 2003", "High CE Low PE 2003", "Low CE High PE 2003", "High CE High PE 2003")
  } else if (iv_spec_name == "CE x PE") {
    scenarios <- c("Low CE Low PE", "High CE Low PE", "Low CE High PE", "High CE High PE")
  } else if (iv_spec_name == "Year Int") {
    scenarios <- c("Low CE 1995", "High CE 1995", "Low CE 2003", "High CE 2003",
                   "Low PE 1995", "High PE 1995", "Low PE 2003", "High PE 2003")
  } else {
    scenarios <- c("Low CE", "High CE", "Low PE", "High PE")
  }
  model_df <- tibble::as_tibble(model.matrix(model)[seq_along(scenarios),])
  model_df[,-1] <- 0 # Zero everything out since everything is mean 0
  model_df <- cbind(model_df, scenarios) %>%
    mutate(CE_hlm = case_when(
      str_detect(scenarios, "Low CE") ~ -.5, # Units are 2 SDs already
      str_detect(scenarios, "High CE") ~ .5, # So this is a -1/+1 SD
      TRUE ~ 0
    ),
    PE_hlm = case_when(
      str_detect(scenarios, "Low PE") ~ -.5,
      str_detect(scenarios, "High PE") ~ .5,
      TRUE ~ 0
    ))
  if(any(str_detect(param_names, "year"))){
    model_df <- mutate(model_df, year = ifelse(str_detect(scenarios, "2003"), 1, 0))
  }
  model_df   <- select(model_df, -scenarios)
  coef_names <- names(mod_coefs)
  for(var in coef_names[str_detect(coef_names, ":")]){ # Supreme fuckery!
    components <- unlist(str_split(var, ":"))
    model_df[ ,var] <- apply(model_df[, components], 1, prod)
  }
  model_mat <- t(model_df)
  colnames(model_mat) <- scenarios
  sim_mat <- tibble::as_tibble(sim_params %*% model_mat)
  if (iv_spec_name == "CE x PE x Year"){
    out_scen_mat <- cbind((sim_mat[,4] - sim_mat[,3]) - (sim_mat[,2] - sim_mat[,1]),
                          (sim_mat[,8] - sim_mat[,7]) - (sim_mat[,6] - sim_mat[,5]))
    colnames(out_scen_mat) <- c("CE x PE 1995", 
                                "CE x PE 2003")
  } else if (iv_spec_name == "CE x PE"){
    out_scen_mat <- cbind((sim_mat[,4] - sim_mat[,3]) - (sim_mat[,2] - sim_mat[,1]))
    colnames(out_scen_mat) <- c("CE x PE")
  } else if (iv_spec_name == "Year Int"){
    out_scen_mat <- cbind(sim_mat[, c(2,4,6,8)] - sim_mat[, c(1,3,5,7)])
    colnames(out_scen_mat) <- paste0(scenarios[c(2,4,6,8)], " - ", scenarios[c(1,3,5,7)])
  } else if (iv_spec_name == "Base"){
    out_scen_mat <- sim_mat[, c(2,4)] - sim_mat[, c(1,3)]
    colnames(out_scen_mat) <- paste0(scenarios[c(2,4)], " - ", scenarios[c(1,3)])
  }
  map_dfr(out_scen_mat, ~ quantile(., c(0.025, .5, .975)), .id="scen")
}