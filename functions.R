library(tidyverse)
library(finalfit)

fct_collapse_yn = function(.f){
  .f %>% 
    forcats::fct_relabel(~ str_replace(.x, "^No.*", "No")) %>% 
    forcats::fct_relabel(~ str_replace(.x, "^Yes.*", "Yes"))
}

