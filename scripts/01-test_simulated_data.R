#### Preamble ####
# Purpose: Tests the structure and validity of the simulated IPUMS 
  #MEPS dataset.
# Author: Christina Nguyen - Marcus Barnes - Nibras Ar Rakib
# Date: 24 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run

#### Workspace setup ####
library(tidyverse)

analysis_data <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####
# Check if the dataset 2018,2021 year
valid_years <- c(2018, 2021)
if (all(analysis_data$year %in% valid_years)) {
  message("Test Passed: The 'Year' column contains only 2018 and 2021.")
} else {
  stop("Test Failed: The 'Year' column contains invalid year.")
}

# Check if the dataset has 3 columns
if (ncol(analysis_data) == 8) {
  message("Test Passed: The dataset has 8 columns.")
} else {
  stop("Test Failed: The dataset does not have 8 columns.")
}

# Check if the sex column has 2 unique values
valid_sex <- c("Male","Female")
dataset_sex <- unique(analysis_data$sex)
if(all(valid_sex %in% dataset_sex)){
  message("Test Passed: variable sex is valid.")
} else{
  message("Test Failed: variable sex is invalid")
}

# Check if the K6SUM column has value from 0-24
valid_k6sum<-c(0:24)
if(all(unique(analysis_data$k6sum) %in% valid_k6sum)){
  message("Test Passed: variable K6SUM is valid.")
} else{
  message("Test Failed: variable K6SUM is invalid")
}
