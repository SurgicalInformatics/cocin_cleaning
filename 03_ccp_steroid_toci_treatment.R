# further steroid and IL6 cleaning and labelling applied to objects from
# https://github.com/SurgicalInformatics/cocin_ccp

library(tidyverse)
library(finalfit)
source("functions.R")
datadir = "/home/common/covid/cleaned/full/"
timestamp = "2022-05-06_1157"

oneline    = read_rds(paste0(datadir, "oneline_", timestamp, "_full.RDS"))
ccp_data   = read_rds(paste0(datadir, "ccp_data_", timestamp, "_full.rds"))

## Treatment Steroid Analysis
# Form steroid variables
ccp_treatment_vars = ccp_data %>% 
  mutate(
    any_dexa = as.factor(case_when(
      dexamethasone == "Yes" ~ "Yes",
      dexamethasone == "Yes to Dexamethasone but other dose" ~ "Yes",
      dexamethasone == "Yes to Dexamethasone but other frequency" ~ "Yes",
      dexamethasone == "Yes to Dexamethasone but other dose AND other frequency" ~ "Yes",
      dexamethasone2 == 1 ~ "Yes",
      dexamethasone3 == 1 ~ "Yes",
      dexamethasone4 == 1 ~ "Yes",
      dexamethasone5 == 1 ~ "Yes",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Yes")) %>% 
      ff_label("Dexamethasone")),
    
    any_cortico = as.factor(case_when(
      corticost_cmyn == "Yes" ~ "Yes",
      corticost2_cmyn == "Yes" ~ "Yes",
      corticost3_cmyn == "Yes" ~ "Yes",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Yes")) %>% 
      ff_label("Corticosteroids")),
    
    any_steroid = as.factor(case_when(
      any_dexa == "Yes" | any_cortico == "Yes" ~ "Yes",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Yes")) %>% 
      ff_label("Steroids")) 
  ) %>% 
  select(subjid, any_dexa, any_cortico, any_steroid) %>% 
  filter(any_dexa == "Yes" | any_cortico == "Yes" | any_steroid == "Yes")

# Join back to oneline
oneline = oneline %>% 
  left_join(ccp_treatment_vars, by="subjid") %>% 
  mutate(any_dexa = fct_explicit_na(any_dexa, "No") %>% fct_relevel("No"),
         any_cortico = fct_explicit_na(any_cortico, "No") %>% fct_relevel("No"),
         any_steroid = fct_explicit_na(any_steroid, "No") %>% fct_relevel("No")
  )
