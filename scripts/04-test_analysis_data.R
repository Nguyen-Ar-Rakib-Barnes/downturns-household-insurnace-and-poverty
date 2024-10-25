#### Preamble ####
# Purpose: Testing parquet files
# Author: Marcus Barnes | Christina Nguyen | Nibras Ar Rakib
# Date: 25 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: package - arrow


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

data_2018 <- read_parquet("data/02-analysis_data/model_data_sample_2018_5k.parquet")
data_2021 <- read_parquet("data/02-analysis_data/model_data_sample_2021_5k.parquet")


#### Test data ####
# Test that the dataset has 5000 rows for year 2018 and 2021 - for our model, we are using sample 5000 row
testthat::test_that("dataset has 5000 rows", {
  expect_equal(nrow(data_2018), 5000)
  expect_equal(nrow(data_2018), 5000)
})
# 
# Test that the dataset has 13 columns
test_that("dataset has 13 columns", {
  expect_equal(ncol(data_2018), 13)
  expect_equal(ncol(data_2021), 13)
})
# 
# # Test that the 'k6sum' column is double
test_that("'k6sum' is double", {
  expect_type(data_2018$k6sum, "double")
  expect_type(data_2021$k6sum, "double")
})

# Test that 'sex' contains only valid Male and Female
valid_values <- c("Male", "Female")
test_that("'sex' contains valid values", {
  expect_true(all(data_2018$sex %in% valid_values))
  expect_true(all(data_2021$sex %in% valid_values))
})

# Test that education labels are correct 
valid_education <- c("Grade 12 or less","Never attended/kindergarten only","High school","GED or equivalent","Some college","AA degree","Bachelor's","Master's","Professional","Doctoral")
test_that("'education' contains valid values", {
  expect_true(all(unique(data_2018$education) %in% valid_education))
  expect_true(all(unique(data_2021$education) %in% valid_education))
})

# Test that income is double
test_that("'income' is double", {
  expect_type(data_2018$inctot, "double")
  expect_type(data_2021$inctot, "double")
})

# 
# # Test that there are no empty strings in 'sex' or 'education' columns
test_that("no empty strings in 'sex' or 'education' columns", {
  expect_false(any(data_2018$sex == "" | data_2018$education == ""))
  expect_false(any(data_2021$sex == "" | data_2018$education == ""))
})