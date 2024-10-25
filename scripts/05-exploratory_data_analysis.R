#### Preamble ####
# Purpose: exploratory data analysis
# Author: Nibras Ar Rakib
# Date: 15 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(arrow)
library(tidyverse)


labeled_df_reduced_2018 <- read_parquet("data/02-analysis_data/model_data_2018.parquet")
labeled_df_reduced_2021 <- read_parquet("data/02-analysis_data/model_data_2021.parquet")
# Identify age group
labeled_df_reduced_2021 |> 
  ggplot(aes(x = age)) +
  geom_histogram(stat = "count", fill = "#013065") +
  labs(title = "Age Group", 
                                     x = "Age", 
                                     y = "Frequency") +
  theme_light()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14)                   # Axis text size (ticks)
  )

# Identify sex
labeled_df_reduced_2021 |> 
  ggplot(aes(x = sex)) +
  geom_histogram(stat = "count", fill = "#013065", color = "white") +
  labs(title = "Sex", 
       x = "Sex", 
       y = "Frequency") +
  theme_light()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14)                   # Axis text size (ticks)
  )
# Identify Marital stat
labeled_df_reduced_2021 |> 
  ggplot(aes(x = marital_status)) +
  geom_histogram(stat = "count", binwidth = 5, fill = "#013065", color = "white") +
  labs(title = "Marital Status", 
       x = "Marital Status", 
       y = "Frequency") +
  theme_light()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14)                   # Axis text size (ticks)
  )
# Identify race
labeled_df_reduced_2021 |> 
  ggplot(aes(x = race)) +
  geom_histogram(stat = "count", binwidth = 5, fill = "#013065", color = "white") +
  labs(title = "Racial Status", 
       x = "Race", 
       y = "Frequency") +
  theme_light()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14),                   # Axis text size (ticks)
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
# Education
labeled_df_reduced_2021 |> 
  ggplot(aes(x = education)) +
  geom_histogram(stat = "count", binwidth = 5, fill = "#013065", color = "white") +
  labs(title = "Education", 
       x = "Education", 
       y = "Frequency") +
  theme_light()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14),              # Axis text size (ticks)
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
income_mean_2021 <- mean(labeled_df_reduced_2021$inctot)
print(paste("Average Income:", income_mean_2021))

std_def_2021 <- sd(labeled_df_reduced_2021$inctot)
print(paste("Standard Deviation:", std_def_2021))

# Calculate mean and standard deviation of income by sex
summary_stats_2021 <- labeled_df_reduced_2021 |>
  group_by(sex) |>
  summarise(
    mean_income = mean(inctot),
    sd_income = sd(inctot)
  )

ggplot(summary_stats_2021, aes(x = sex, y = mean_income, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", color = "#013065", width = 0.6) +
  labs(
    title = "Mean Income by Sex",
    x = "Sex",
    y = "Mean Income"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Title font size
    axis.title.x = element_text(size = 16),               # X-axis label size
    axis.title.y = element_text(size = 16),               # Y-axis label size
    axis.text = element_text(size = 14),              # Axis text size (ticks)
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

