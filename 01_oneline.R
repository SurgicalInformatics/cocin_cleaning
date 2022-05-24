# further cleaning and labelling applied to objects from
# https://github.com/SurgicalInformatics/cocin_ccp

library(tidyverse)
library(finalfit)
source("functions.R")
datadir = "/home/common/covid/cleaned/full/"
timestamp = "2022-05-06_1157"

oneline    = read_rds(paste0(datadir, "oneline_", timestamp, "_full.RDS"))

# dexamethasone collapse into a two-level version ----
# suffixed with .yn
# check count before and after
count(oneline, dexamethasone)
oneline = oneline %>% 
  mutate(across(dexamethasone, ~ fct_collapse_yn(.) %>% 
                  fct_recode(NULL = "N/K") %>% 
                  fct_relevel("No"),
                .names = "{.col}.yn"))
count(oneline, dexamethasone.yn, dexamethasone)

# ethnicity collapse ----
count(oneline, ethnicity)
oneline = oneline %>% 
  mutate(
    ethnicity.4levels = fct_collapse(ethnicity,
                                  "Other Ethnic Minority" = 
                                    c("Other", "Arab", "Latin American", "Aboriginal/First Nations", "West Asian")) %>% 
      fct_relevel("White", "South Asian", "East Asian", "Black", "Other Ethnic Minority") %>% 
      ff_label("Ethnicity"))
count(oneline, ethnicity.4levels, ethnicity)


# symptoms ----
# Form symptom variable from existing symptom CRF
count(oneline, fever_ceoccur_v2)
oneline = oneline %>% 
  mutate(across(fever_ceoccur_v2:anosmia_ceoccur_v2,
                ~ fct_recode(.,
                             "Yes" = "YES",
                             "No" = "NO",
                             "Unknown" = "Unknown"))
  )
# add in .names = "{.col}2" inside accross() to test that worked before over-writing
# count(oneline, fever_ceoccur_v2, fever_ceoccur_v22)

oneline = oneline %>% 
  mutate(
    number_symptoms = select(., fever_ceoccur_v2:anosmia_ceoccur_v2) %>% 
      {. == "Yes"} %>% 
      rowSums(na.rm = TRUE) %>% 
      ff_label("Number of symptoms"),
    
    number_symptoms.factor = case_when(
      number_symptoms < 1    ~ "0",
      number_symptoms < 2    ~ "1", 
      is.na(number_symptoms) ~ NA_character_,
      TRUE                   ~ "2+") %>% 
      factor() %>% 
      ff_label("Number of symptoms"),
    
    any_symptoms = case_when(
      number_symptoms == 0              ~ "No",
      number_symptoms  > 0              ~ "Yes",
      asymptomatic    == "Asymptomatic" ~ "No",
      no_symptoms     == "YES"          ~ "No",
      no_symptoms_v3  == "YES"          ~ "No",
      adm_no_symp     == 1              ~ "No",
      no_symptoms     == "No"           ~ "Yes",
      no_symptoms_v3  == "No"           ~ "Yes"
    ) %>% 
      ff_label("Any symptoms")
    )
count(oneline, number_symptoms.factor, number_symptoms)
count(oneline, any_symptoms, number_symptoms.factor)


# labelling ----
oneline = oneline %>% 
  mutate(
    any_icu         = ff_label(any_icu,         "ICU/HDU admission"),
    any_invasive    = ff_label(any_invasive,    "Invasive ventilation"),
    any_noninvasive = ff_label(any_noninvasive, "Noninvasive ventillation")
  )