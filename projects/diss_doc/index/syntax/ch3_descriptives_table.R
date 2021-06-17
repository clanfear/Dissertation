library(tidyverse)
library(flextable)
library(ftExtra)
source("../../shared/syntax/project_functions.R")
load("../../formal_control/data/chicago/derived/nc_analytical_wide.RData")

ch3_descriptives <- nc_analytical_wide %>%
  select(ccahs_nc,
         CE_hlm_1995, CE_hlm_2001, 
         PE_hlm_1995, PE_hlm_2001, 
         TE_hlm_1995, AT_hlm_1995, TE_hlm_2001,
         CNT_MURDER_1995, CNT_MURDER_2003, CNT_MURDER_1989_1991,
         PV_hlm_1995, PV_hlm_2001,
         VICT_hlm_1995, VICT_hlm_2001,
         VIOLENT_CRIME_1995, VIOLENT_CRIME_2003 = VIOLENT_RATE_2003,
         FAC_disadv_1990,  FAC_disadv_2000,
         FAC_stability_1990, FAC_stability_2000,
         FAC_hispimm_1990, FAC_hispimm_2000,
  ) %>%
  mutate(CNT_MURDER_1989_1991 = CNT_MURDER_1989_1991/3) %>%
  pivot_longer(-ccahs_nc) %>%
  mutate(name = str_remove_all(name, "_(1990|1995|2001|2000|2003|1989_1991)")) %>%
  group_by(name) %>%
  filter(!is.na(value)) %>%
  mutate(value = ifelse(str_detect(name, "^(FAC|PV|VICT|CE|PE|TE|AT|VIOLENT)"), standardize(value, scale = 1), value)) %>%
  summarize(across(value, list(Mean = ~mean(., na.rm=TRUE),
                               SD   = ~sd(., na.rm=TRUE),
                               Min  = ~min(., na.rm=TRUE),
                               Max  = ~max(., na.rm=TRUE)), .names = "{.fn}"),
            df = list(value)) %>%
  ungroup() %>%
  mutate(across(-c(name, df), ~round(., 2))) %>%
  mutate(name           =
           fct_relevel(
             fct_recode(name,
                        "Homicide"              = "CNT_MURDER", 
                        "Perceived Violence"    = "PV_hlm",
                        "Victimization"         = "VICT_hlm",
                        "Violent Crime"         = "VIOLENT_CRIME",
                        "Collective Efficacy"   = "CE_hlm",
                        "Police Efficacy"       = "PE_hlm", 
                        "Reciprocated Exchange" = "TE_hlm",
                        "Attachment"            = "AT_hlm",
                        "Disadvantage"          = "FAC_disadv",
                        "Stability"             = "FAC_stability",
                        "Hispanic/Immigrant"    = "FAC_hispimm"),
             "Homicide",         
             "Perceived Violence",        
             "Victimization",      
             "Violent Crime",         
             "Collective Efficacy",
             "Police Efficacy",
             "Reciprocated Exchange",           
             "Attachment",       
             "Disadvantage",    
             "Stability", 
             "Hispanic/Immigrant")) %>%
  arrange(name) %>%
  rename(Measure = name) %>%
  relocate(Min, df, Max, .after = last_col()) %>%
  flextable() %>%
  mk_par(j = "df", i = ~ !is.na(Max), value = as_paragraph(
    plot_chunk(value = df, type = "density", col = "black", 
               width = 0.8, height = .2, free_scale = TRUE)
  )) %>%
  set_header_labels(df = "Density", level = "") %>%
  font(fontname = "Latin Modern Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  set_table_properties(width = 0.8, layout = "autofit") %>%
  border_remove() %>%
  align(j = 2:6, align = "center", part = "all") %>%
  hline_bottom(border = officer::fp_border(width = 1)) %>%
  hline_top(border = officer::fp_border(width = 1), part = "header") %>%
  hline_top(border = officer::fp_border(width = 0.5))
