#### Preamble ####
# Purpose: Linear Regression
# Author: Nibras Ar Rakib
# Date: 11 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: 

#### Workspace setup ####
library(boot)
library(broom.mixed)
library(collapse)
library(dataverse)
library(gutenbergr)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)

#### Read data ####

df <- read.csv("data/02-analysis_data/analysis_data.csv")

filtered_df <- df |> filter(year==2021) |> filter(age!=996) |> filter(marstat!=99) |> filter(!educ %in% c(996,997,998,999))

labeled_df <- filtered_df |> mutate(
  label_outcome = if_else(outcome==1, "Yes", "No"),
  label_outcome = as_factor(label_outcome),
  label_sex = if_else(sex==1,"Male", "Female"),
  label_education = case_when(
    educ==100~"Grade 12 or less, no high school diploma or equivalent",
    educ==101~"Grade 8 or less (no further detail)",
    educ==102~"Never attended/kindergarten only",
    educ==103~"Grades 1-11 (no further detail)",
    educ==104~"Grade 1",
    educ==105~"Grade 2",
    educ==106~"Grade 3",
    educ==107~"Grade 4",
    educ==108~"Grade 5",
    educ==109~"Grade 6",
    educ==110~"Grade 7",
    educ==111~"Grade 8",
    educ==112~"Grade 9-12, no diploma (no further detail)",
    educ==113~"Grade 9",
    educ==114~"Grade 10",
    educ==115~"Grade 11",
    educ==116~"12th grade, no diploma",
    educ==200~"High school diploma or GED",
    educ==201~"High school graduate",
    educ==202~"GED or equivalent",
    educ==300~"Some college, no 4yr degree",
    educ==301~"Some college, no degree",
    educ==302~"AA degree: technical/vocational/occupational",
    educ==303~"AA degree: academic program",
    educ==400~"Bachelor's degree (BA,AB,BS,BBA)",
    educ==500~"Master's, Professional, or Doctoral Degree",
    educ==501~"Master's degree (MA,MS,Med,MBA)",
    educ==502~"Professional (MD,DDS,DVM,JD)",
    educ==503~"Doctoral degree (PhD, EdD)",
    educ==504~"Other degree",
    educ==505~"Professional School or Doctoral degree, topcoded (MD, DDS, DVM, JD, PhD, EdD)"
  ),
  label_education = factor(
    label_education,
    levels=c(
      "Grade 12 or less, no high school diploma or equivalent",
      "Grade 8 or less (no further detail)",
      "Never attended/kindergarten only",
      "Grades 1-11 (no further detail)",
      "Grade 1",
      "Grade 2",
      "Grade 3",
      "Grade 4",
      "Grade 5",
      "Grade 6",
      "Grade 7",
      "Grade 8",
      "Grade 9-12, no diploma (no further detail)",
      "Grade 9",
      "Grade 10",
      "Grade 11",
      "12th grade, no diploma",
      "High school diploma or GED",
      "High school graduate",
      "GED or equivalent",
      "Some college, no 4yr degree",
      "Some college, no degree",
      "AA degree: technical/vocational/occupational",
      "AA degree: academic program",
      "Bachelor's degree (BA,AB,BS,BBA)",
      "Master's, Professional, or Doctoral Degree",
      "Master's degree (MA,MS,Med,MBA)",
      "Professional (MD,DDS,DVM,JD)",
      "Doctoral degree (PhD, EdD)",
      "Other degree",
      "Professional School or Doctoral degree, topcoded (MD, DDS, DVM, JD, PhD, EdD)"
    )
  )
)

write_csv(labeled_df, "data/02-analysis_data/model_data.csv")

labeled_df_reduced <- labeled_df |> slice_sample(n = 1000)

mental_illness <-
  stan_glm(
    label_outcome ~ label_sex + label_education,
    data = labeled_df_reduced,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )
saveRDS(
  mental_illness,
  file = here::here("models/mental_illness.rds")
)

