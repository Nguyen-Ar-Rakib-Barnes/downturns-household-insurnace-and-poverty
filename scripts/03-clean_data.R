#### Preamble ####
# Purpose: Cleans the raw data - removed rows where K6SUM==98,96 
# Author: Marcus Barnes | Christina Nguyen | Nibras Ar Rakib
# Date: 11 Oct 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download raw data from https://meps.ipums.org/meps/

#### Workspace setup ####
library(tidyverse)

#### Clean data ####
raw_data <- read_csv("data/01-raw_data/meps_00003.csv")

cleaned_data <-
  raw_data |>
  janitor::clean_names()

cleaned_data <- cleaned_data |> select(year,pid,age,sex,marstat,racea,educ,inctot,k6sum)

cleaned_data <- cleaned_data |> filter(!k6sum %in% c(96,98)) |> filter(inctot>0)
cleaned_data$outcome <- ifelse(cleaned_data$k6sum<13,0,1)
#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/analysis_data.csv")
