#### Preamble ####
# Purpose: Simulates a dataset of IPUMS MEPS.
# Author: Christina Nguyen - Marcus Barnes - Nibras Ar Rakib
# Date: 23 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed


#### Workspace setup ####
library(tidyverse)
set.seed(853)

number_of_rows <- 500
# Simulating the variables 
# year,age,sex,marstat,racea,educ,inctot,k6sum
simulated_meps_dataset <- tibble(
  year = sample(x=c(2018,2021), size = number_of_rows, replace = TRUE),
  age = sample(x = (19:80), size = number_of_rows, replace = TRUE),
  sex = sample(x = c("Male","Female"), size = number_of_rows, replace = TRUE),
  marstat = sample(x = c("Married","Widowed","Divorced","Separated","Never married"), size = number_of_rows, replace = TRUE),
  racea = sample(x = c("White","Black/African-American","Alaskan Native or American Indian"
                       ,"Multiple Race, including Asian, excluding Black and White"
                       ,"Multiple Race, including Asian and Black, excluding White"
                       ,"Multiple Race, including Asian and White, excluding Black"
                       ,"Multiple Race, including Black, excluding Asian and White","Multiple Race, including Black and White, excluding Asian"
                       ,"Multiple Race, including White, excluding Asian and Black","	Multiple Race, including Asian, White, and Black"), size = number_of_rows, replace = TRUE),
  educ = sample(x = c("Grade 12 or less","Never attended/kindergarten only","GED or equivalent"
                      ,"AA degree", "Bachelor's", "Master's degree","Doctoral degree"), size = number_of_rows, replace = TRUE),
  inctot = sample(x = (1900:5000), size = number_of_rows, replace = TRUE),
  k6sum = sample(x = (0:24), size = number_of_rows, replace = TRUE)
)

#### Save data ####
write_csv(simulated_meps_dataset, "data/00-simulated_data/simulated_data.csv")