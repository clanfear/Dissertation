cf_data <- data.frame(year = 2016, 
                      month = 1 , 
                      contingency = rep(c("Contingency","Normal"), each = 3), 
                      temp = mean(nb_gam_pol_model_departures5$model$temp),
                      rain = "No Rain",
                      holiday = FALSE,
                      weekend = FALSE,
                      O3 = mean(nb_gam_pol_model_departures5$model$O3),
                      PM2.5 = mean(nb_gam_pol_model_departures5$model$PM2.5),
                      hour = c(8,12,19))

# We draw 10,000 sets of coefficients from a multivariate normal distribution
# with means at our estimates and distributed by the parameter covariance matrix.
cf_beta <- MASS::mvrnorm(n=100000,
                         mu = coef(nb_gam_pol_model_departures5),
                         Sigma = vcov(nb_gam_pol_model_departures5))

# We create counterfactual predictions by taking the linear predictor matrix of
# the GAM model and multiplying it by the model estimates and transposing it.
# This yields a matrix where each column is a vector of predictions where
# coefficients
cf_predictions <- 
  predict(nb_gam_pol_model_departures5, newdata = cf_data, type = "lpmatrix") %*% 
  t(cf_beta) %>% t()
colnames(cf_predictions) <- c("cont_8", "cont_12", "cont_19","norm_8", "norm_12", "norm_19")

# We then subtract the normal days from contingency days to get a difference.
cf_ratios <- exp(cf_predictions[,1:3]) / exp(cf_predictions[,4:6])
cf_diffs <- exp(cf_predictions[,1:3]) - exp(cf_predictions[,4:6])
# USe a function to extract means and 95% intervals.
extract_pe_ci <- function(x){
  vals <- c(mean(x), quantile(x, probs=c(.025, .975)))
  names(vals) <- c("PE", "LB", "UB")
  return(vals)
}
# Spit out our values.
differences <- as.data.frame(apply(cf_diffs, 2, extract_pe_ci))
ratios <- as.data.frame(apply(cf_ratios, 2, extract_pe_ci))
differences
ratios