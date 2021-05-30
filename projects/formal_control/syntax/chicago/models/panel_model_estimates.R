library(tidyverse)
library(fixest)
library(lme4)
library(piecewiseSEM)
library(lavaan)
library(broom.mixed)
library(patchwork)
library(ragg)
library(fishualize)
source("../shared/syntax/project_functions.R")
source("./syntax/chicago/models/panel_model_estimates_function.R")
load("./output/all_models.RData")
ragg_png <- function(...) ragg::agg_png(..., res = 300, units = "in")


# Function to take a model estimate, concoct counterfactual scenarios, then
# extract first difference estimates with confidence intervals. This will yield
# the model point estimates for CE / PE but also show where a param is significant
# in presence of interaction.

pred_ml_models <- all_models %>% mutate(sim_estimates = pmap(list(iv_spec_name, models), get_pred_mat))

plot_data <- pred_ml_models %>% 
  unnest(sim_estimates) %>%
  mutate(sig = ifelse(sign(`2.5%`)==sign(`97.5%`), "significant", "not significant"),
         scenario = fct_rev(fct_relevel(case_when(
           scen == "High PE - Low PE" ~ "PE\nOverall",
           scen == "High CE - Low CE" ~ "CE\nOverall",
           scen == "High CE 1995 - Low CE 1995" ~ "CE\nin 1995",
           scen == "High CE 2003 - Low CE 2003" ~ "CE\nin 2003",
           scen == "High PE 1995 - Low PE 1995" ~ "PE\nin 1995",
           scen == "High PE 2003 - Low PE 2003" ~ "PE\nin 2003",
           scen == "CE x PE" ~ "CE x PE",
         ), 
         "CE\nOverall", 
         "PE\nOverall", 
         "CE\nin 1995", 
         "CE\nin 2003",
         "PE\nin 1995", 
         "PE\nin 2003", 
         "CE x PE"
         )),
         outcome = str_to_title(str_replace(outcome, "_", "\n"))) %>%
  mutate(model = case_when(
    fit_spec == "MRF Polydis" ~ "Markov Random Field\nDisadvantage Spline",
    fit_spec == "RE" ~ "Random Effects",
    fit_spec == "Lag RE" ~ "Lag Homicide",
    fit_spec == "FE" ~ "Fixed Effects",
    fit_spec == "MRF" ~ "Markov Random Field",
    fit_spec == "IV" ~ "Instrumental\nVariables"
  )) %>% 
  mutate(model = fct_rev(fct_relevel(model,
                             "Random Effects", "Lag Homicide",  
                             "Markov Random Field",  "Markov Random Field\nDisadvantage Spline", "Fixed Effects", "Instrumental\nVariables")))

cf_plot <- function(x){
  line_defs <- tribble(~x, ~y, ~xend, ~yend ,
                       -0.7, 5.5, 0.2, 5.5,  
                       # -0.7, 5.75, 0.2, 5.75, 
                       -0.7, 1.5, 0.2, 1.5)
  # -0.7,  .75, 0.2,  .75)
  text_defs <- tribble(~x, ~y, ~label, ~outcome,
                       -0.93, 6.5, "Base\nSpecification", "Homicide",
                       -0.93, 3.5, "Year\nInteraction", "Homicide",
                       -0.93, 1, "CE x PE\nInteraction", "Homicide")
  ggplot(data = x, aes(x = `50%`, y = scenario, group = model, color = model, shape = model, alpha = sig)) + 
    facet_grid(. ~ outcome) + 
    scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.5), guide = "none") +
    geom_point(position = ggstance::position_dodgev(height = 0.4)) +
    # scale_color_manual(values = c("significant" = "black", "not significant"="gray50"), guide = "none") +
    scale_shape_discrete("") +
    scale_color_fish_d("", option = "Centropyge_loricula") +
    scale_x_continuous(breaks = seq(-0.4, 0.2, by = 0.2)) +
    geom_vline(xintercept=0, linetype = "dashed") + xlab(" \nPoint Estimate and 95% CI") + ylab("") +
    geom_segment(data = line_defs, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes = FALSE) +
    geom_text(data = text_defs, aes(x = x, y=y, label=label), angle = 90, hjust=0.5, inherit.aes = FALSE) +
    geom_errorbarh(aes(xmin = `2.5%`, xmax = `97.5%`),
                   height = 0.20,
                   position = ggstance::position_dodgev(height = 0.4)) + 
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.y = element_text(hjust=0.5), 
          panel.spacing.x = unit(.25, "in"),
          panel.grid.major.y = element_blank(),
          plot.margin = margin(l = 0.4, r=0.1, unit = "in")) +
    coord_cartesian(clip = "off", xlim = c(-0.4, 0.2)) +
    guides(color = guide_legend(reverse=TRUE),
           shape = guide_legend(reverse=TRUE))
}
cf_plot(plot_data)
# Rewrite plot to convert interactions to first diffs; put CE and PE on same plot?
ggsave("./output/img/ce_pe_plot_vertical.png",  cf_plot(plot_data), width = 6.5, height = 9, units = "in", device = ragg_png)

