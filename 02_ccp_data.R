# note that there may vbe variables that are cleaned in oneline, but then not cleaned in ccp_data
# recommendation: if variable exists in oneline, get it from there, not from ccp_data

library(tidyverse)
library(finalfit)
source("functions.R")
datadir = "/home/common/covid/cleaned/full/"
timestamp = "2022-05-06_1157"

ccp_data   = read_rds(paste0(datadir, "ccp_data_", timestamp, "_full.rds"))
