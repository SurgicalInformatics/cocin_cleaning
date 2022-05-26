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


# Any incidence of symptom (index or readmission)
oneline = oneline %>% 
  mutate(
    any_fever = case_when(
      fever_ceoccur_v2 == 'Yes' | fever == 'Yes' | fever_ceoccur_v3 == 'Yes' | temp_vsorres >= 38 | daily_temp_vsorres >= 38 ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Fever"),
    any_cough = case_when(
      cough == 'Yes' | cough_ceoccur_v2 == 'Yes' | coughsput_ceoccur_v2 == 'Yes' | coughhb_ceoccur_v2 == 'Yes' | 
        cough_ceoccur_v3 == 'Yes' | coughsput_ceoccur_v3 == 'Yes' | coughhb_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Cough"),
    any_dyspnoe = case_when(
      dyspnoe == 'Yes' | shortbreath_ceoccur_v2 == 'Yes' | shortbreath_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Dyspnoea"),
    any_fatigue = case_when(
      fatigue_ceoccur_v2 == 'Yes' | fatigue_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Fatigue"),
    any_confusion = case_when(
      confusion_ceoccur_v2 == 'Yes' | confusion_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Confusion"),
    any_diarrhoea = case_when(
      diarrhoea_ceoccur_v2 == 'Yes' | diarrhoea_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Diarrhoea"),
    any_vomit = case_when(
      vomit_ceoccur_v2 == 'Yes' | vomit_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Vomiting"),
    any_myalgia = case_when(
      myalgia_ceoccur_v2 == 'Yes' | myalgia_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Myalgia"),
    any_chestpain = case_when(
      chestpain_ceoccur_v2 == 'Yes' | chestpain_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Chest Pain"),
    any_headache = case_when(
      headache_ceoccur_v2 == 'Yes' | headache_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Headache"),
    any_wheeze = case_when(
      wheeze_ceoccur_v2 == 'Yes' | wheeze_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Wheezing"),
    any_abdopain = case_when(
      abdopain_ceoccur_v2 == 'Yes' | abdopain_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Abdominal Pain"),
    any_sorethroat = case_when(
      sorethroat_ceoccur_v2 == 'Yes' | sorethroat_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Sore Throat"),
    any_jointpain = case_when(
      jointpain_ceoccur_v2 == 'Yes' | jointpain_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Joint Pain"),
    any_runnynose = case_when(
      runnynose_ceoccur_v2 == 'Yes' | runnynose_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Runny Nose"),
    any_skinulcers = case_when(
      skinulcers_ceoccur_v2 == 'Yes' | skinulcers_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Skin Ulcers"),
    any_seizures = case_when(
      seizures_cecoccur_v2 == 'Yes' | seizures_cecoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Seizures"),
    any_skinrash = case_when(
      rash_ceoccur_v2 == 'Yes' | rash_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Skin Rash"),
    any_lowerchest = case_when(
      lowerchest_ceoccur_v2 == 'Yes' | lowerchest_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Lower Chest Indrawing"),
    any_bleed = case_when(
      bleed_ceoccur_v2 == 'Yes' | bleed_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Bleeding"),
    any_lymp = case_when(
      lymp_ceoccur_v2 == 'Yes' | lymp_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Lymphadenopathy"),
    any_earpain = case_when(
      earpain_ceoccur_v2 == 'Yes' | earpain_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Ear Pain"),
    any_conjunct = case_when(
      conjunct_ceoccur_v2 == 'Yes' | conjunct_ceoccur_v3 == 'Yes' ~ 'Yes',
      TRUE ~ "No") %>% ff_label("Conjunctivitis")
  )


# labelling ----
oneline = oneline %>% 
  mutate(
    any_icu         = ff_label(any_icu,         "ICU/HDU admission"),
    any_invasive    = ff_label(any_invasive,    "Invasive ventilation"),
    any_noninvasive = ff_label(any_noninvasive, "Noninvasive ventillation")
  )