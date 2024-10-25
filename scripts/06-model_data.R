#### Preamble ####
# Purpose: Regression
# Author: Nibras Ar Rakib
# Date: 11 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: 

#### Workspace setup ####
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)


labeled_df_reduced_2018 <- read_parquet("data/02-analysis_data/model_data_sample_2018_5k.parquet")
labeled_df_reduced_2021 <- read_parquet("data/02-analysis_data/model_data_sample_2021_5k.parquet")

mental_illness_2018 <-
  stan_glm(
    outcome ~ sex + education,
    data = labeled_df_reduced_2018,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept =
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

mental_illness_2021 <-
  stan_glm(
    outcome ~ sex + education,
    data = labeled_df_reduced_2021,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept =
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )


# 
# 

saveRDS(
  mental_illness_2018,
  file = here::here("models/mental_illness_2018.rds")
)
# ### Model data ###
saveRDS(
  mental_illness_2021,
  file = here::here("models/mental_illness_2021.rds")
)


mental_illness_2018 <- readRDS(file = here::here("models/mental_illness_2018.rds"))
mental_illness_2021 <- readRDS(file = here::here("models/mental_illness_2021.rds"))


modelsummary(
  list(
    "Severe Mental Illness" = mental_illness_2018
  ),
  statistic = "mad"
)
modelplot(mental_illness_2018, conf_level = 0.9) +
  labs(x = "90 per cent credibility interval")

modelsummary(
  list(
    "Severe Mental Illness" = mental_illness_2021
  ),
  statistic = "mad"
)
modelplot(mental_illness_2021, conf_level = 0.9) +
  labs(x = "90 per cent credibility interval")

# Calculate log-odds (Intercept + Coefficient)
intercept <- coef(mental_illness_2021)["(Intercept)"]
coef_male <- coef(mental_illness_2021)["sexMale"]

# Calculate probabilities for both groups
prob_female <- exp(intercept) / (1 + exp(intercept))
prob_male <- exp(intercept + coef_male) / (1 + exp(intercept + coef_male))

# Why the probabilities are low? is okay?
cat("Probability for Females:", round(prob_female, 4), "\n")
cat("Probability for Males:", round(prob_male, 4), "\n")


