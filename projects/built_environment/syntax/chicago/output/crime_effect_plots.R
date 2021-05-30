library(tidyverse)
library(piecewiseSEM)

# Second stage

load("./data/chicago/derived/base_second_stage_list.RData")
load("./data/chicago/derived/interaction_second_stage_list.RData")


# SEM models
load("./data/chicago/derived/psem_hlm_list_summary.RData")
load("./data/chicago/derived/psem_hlm_int_list_summary.RData")

fix_names <- function(x){
  . <- str_remove_all(x, "BE_pr_|_block_2001|FAC_|_2000|hlm|_2001|CRIME_|_2004_2006|_onstreet|_bld")
  . <- str_replace_all(., "_", " ")
  . <- str_to_title(.)
  . <- str_replace_all(. , c("Ce" = "Collective Efficacy",
                             "Disadv" = "Disadvantage",
                             "2" = "^2",
                             "Hispimm" = "Hispanic / Immigrant",
                             "Nc" = "NC",
                             "Dest" = "Destination"))
  return(.)
}

psem_hlm_list_summary$coefficients %>% 
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "~~") & str_detect(Response, "CRIME")) %>%
  filter(!str_detect(Predictor, "FAC|density")) %>%
  mutate(level = factor(ifelse(str_detect(Predictor, "(^CE|^FAC|ltdb)"), "NC", "Block"), levels = c("NC","Block")),
         lb = Estimate - (as.numeric(Std.Error)*1.96),
         ub = Estimate + (as.numeric(Std.Error)*1.96),
         Predictor = fix_names(Predictor),
         Response = fix_names(Response),
         sig = ifelse(lb > 0 | ub < 0, "Sig", "Not Sig")) %>%
  mutate(across(c(Estimate, lb, ub), ~exp(.))) %>%
  ggplot(aes(x = Estimate, y = Predictor, alpha = sig)) + 
  facet_grid(level ~ Response, scales = "free_y", space = "free_y") + 
  geom_vline(xintercept = 1, lty = "dashed") +
  scale_alpha_manual(values = c("Sig"=1, "Not Sig"=0.3)) +
  geom_point() + 
  xlab("Rate Ratio (1 SD) Difference - 95% CI") +
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) + 
  theme_minimal() + 
  theme(panel.grid = element_blank())

psem_hlm_list_summary$coefficients %>% 
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "~~") & !str_detect(Response, "CRIME")) %>%
  filter(!str_detect(Response, "CE_hlm")) %>%
  mutate(level = factor(ifelse(str_detect(Predictor, "(^CE|^FAC|ltdb)"), "NC", "Block"), levels = c("NC","Block")),
         lb = Estimate - (as.numeric(Std.Error)*1.96),
         ub = Estimate + (as.numeric(Std.Error)*1.96),
         Predictor = fix_names(Predictor),
         Response = fix_names(Response),
         sig = ifelse(lb > 0 | ub < 0, "Sig", "Not Sig")) %>%
  ggplot(aes(x = Estimate, y = Predictor, alpha = sig)) + 
  facet_grid(level ~ Response, scales = "free_y", space = "free_y") + 
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_alpha_manual(values = c("Sig"=1, "Not Sig"=0.3)) +
  geom_point() + 
  xlab("Standardized Association (1 SD) (95% CI)") +
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())

psem_hlm_list_summary$coefficients %>% 
  select(Response, Predictor, Estimate, Std.Error) %>%
  filter(!str_detect(Response, "~~") & str_detect(Response, "CE_hlm")) %>%
  mutate(level = factor(ifelse(str_detect(Predictor, "(^CE|^FAC|ltdb)"), "NC", "Block"), levels = c("NC","Block")),
         lb = Estimate - (as.numeric(Std.Error)*1.96),
         ub = Estimate + (as.numeric(Std.Error)*1.96),
         Predictor = fix_names(Predictor),
         Response = fix_names(Response),
         sig = ifelse(lb > 0 | ub < 0, "Sig", "Not Sig")) %>%
  ggplot(aes(x = Estimate, y = Predictor, alpha = sig)) + 
  facet_grid(level ~ Response, scales = "free_y", space = "free_y") + 
  geom_vline(xintercept = 0, lty = "dashed") +
  scale_alpha_manual(values = c("Sig"=1, "Not Sig"=0.3)) +
  geom_point() + 
  xlab("Standardized Association (1 SD) (95% CI)") +
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) + 
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank())
# Need the interaction plots too. Maybe do them CF style like other chapter? Real easy since all nb models
