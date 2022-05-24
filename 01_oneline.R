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
    ethnicity.4lev = fct_collapse(ethnicity,
                                  "Other Ethnic Minority" = 
                                    c("Other", "Arab", "Latin American", "Aboriginal/First Nations", "West Asian")) %>% 
      fct_relevel("White", "South Asian", "East Asian", "Black", "Other Ethnic Minority"))
count(oneline, ethnicity.4lev, ethnicity)

# labeling ----
oneline = oneline %>% 
  mutate(
    any_icu         = ff_label(any_icu, "ICU/HDU admission"),
    any_invasive    = ff_label(any_invasive, "Invasive ventilation"),
    any_noninvasive = ff_label(any_noninvasive, "Noninvasive ventillation")
  )


