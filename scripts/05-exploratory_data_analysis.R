#### Preamble ####
# Purpose: Regression
# Author: Nibras Ar Rakib
# Date: 15 October 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT


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
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Read data ####

df <- read.csv("data/02-analysis_data/analysis_data.csv")

### Model data ###
filtered_df_2018 <- df |> filter(year==2018) |> filter(age!=996) |>
  filter(age>18) |> filter(age<80) |>
  filter(sex %in% c(1,2)) |> filter(marstat %in% c(10,20,30,40,50,99)) |>
  filter(racea %in% c(100,200,300,310,320,330,340,350,400,410,411,412,413,414,415,416,420,421,422,423,430,431,432,433,434,500,510,520,530,540,550,560,570,580,600,610,611,612,613,614,615,616,617)) |> 
  filter(educ %in% c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,200,201,202,300,301,302,303,400,500,501,502,503,504,505))

### Model data ###
filtered_df_2021 <- df |> filter(year==2021) |> filter(age!=996) |>
  filter(age>18) |> filter(age<80) |>
  filter(sex %in% c(1,2)) |> filter(marstat %in% c(10,20,30,40,50,99)) |>
  filter(racea %in% c(100,200,300,310,320,330,340,350,400,410,411,412,413,414,415,416,420,421,422,423,430,431,432,433,434,500,510,520,530,540,550,560,570,580,600,610,611,612,613,614,615,616,617)) |> 
   filter(educ %in% c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,200,201,202,300,301,302,303,400,500,501,502,503,504,505))

write_csv(filtered_df_2018, "data/02-analysis_data/model_data_2018.csv")
write_csv(filtered_df_2021, "data/02-analysis_data/model_data_2021.csv")

set.seed(555)

labeled_df_reduced_2018 <- filtered_df_2018 |> slice_sample(n = 5000)
labeled_df_reduced_2021 <- filtered_df_2021 |> slice_sample(n = 5000)

write_csv(labeled_df_reduced_2018, "data/02-analysis_data/model_data_sample_2018_5k.csv")
write_csv(labeled_df_reduced_2021, "data/02-analysis_data/model_data_sample_2021_5k.csv")


labeled_df_reduced_2018 <- read.csv("data/02-analysis_data/model_data_sample_2018_5k.csv")
labeled_df_reduced_2021 <- read.csv("data/02-analysis_data/model_data_sample_2021_5k.csv")


labeled_df_reduced_2018 <- labeled_df_reduced_2018 |> mutate(
  outcome = if_else(outcome==1, "Yes", "No"),
  outcome = as_factor(outcome),
  sex = if_else(sex==1,"Male", "Female"),
  sex = as_factor(sex),
  marital_status = case_when(
    marstat==10~"Married",
    marstat==20~"Widowed",
    marstat==30~"Divorced",
    marstat==40~"Separated",
    marstat==50~"Never married"
  ),
  marital_status = factor(marital_status, levels=c("Married","Widowed","Divorced","Separated","Never married")),
  race= case_when(
    racea==100~"White",
    racea==200~"Black/African-American",
    racea==300~"Aleut, Alaskan Native, or American Indian",
    racea==310~"Alaskan Native or American Indian",
    racea==320~"Alaskan Native/Eskimo",
    racea==330~"Aleut",
    racea==340~"American Indian",
    racea==350~"American Indian or Alaskan Native and any other group",
    racea==400~"Asian or Pacific Islander",
    racea==410~"Asian",
    racea==411~"Chinese",
    racea==412~"Filipino",
    racea==413~"Korean",
    racea==414~"Vietnamese",
    racea==415~"Japanese",
    racea==416~"Asian Indian",
    racea==420~"Pacific Islander",
    racea==421~"Hawaiian",
    racea==422~"Samoan",
    racea==423~"Guamanian",
    racea==430~"Other Asian or Pacific Islander",
    racea==431~"Other Asian or Pacific Islander (1992-1995)",
    racea==432~"Other Asian or Pacific Islander (1996)",
    racea==433~"Other Asian or Pacific Islander (1997-1998)",
    racea==434~"Other Asian (1999 forward)",
    racea==500~"Other Race",
    racea==510~"Other Race (1963-1977)",
    racea==520~"Other Race (1978)",
    racea==530~"Other Race (1979-1991)",
    racea==540~"Other Race (1992-1995)",
    racea==550~"Other Race (1996)",
    racea==560~"Other Race (1997-1998)",
    racea==570~"Other Race (1999-2002)",
    racea==580~"Primary Race not releasable",
    racea==600~"Multiple Race, No Primary Race Selected",
    racea==610~"Multiple Race, including Asian, excluding Black and White",
    racea==611~"Multiple Race, including Asian and Black, excluding White",
    racea==612~"Multiple Race, including Asian and White, excluding Black",
    racea==613~"Multiple Race, including Black, excluding Asian and White",
    racea==614~"Multiple Race, including Black and White, excluding Asian",
    racea==615~"Multiple Race, including White, excluding Asian and Black",
    racea==616~"Multiple Race, including Asian, White, and Black",
    racea==617~"Multiple Race, excluding Asian, White, and Black"
  ),
  race = factor(race, 
                levels=c("White", "Black/African-American", "Aleut, Alaskan Native, or American Indian", "Alaskan Native or American Indian", "Alaskan Native/Eskimo", "Aleut", "American Indian", "American Indian or Alaskan Native and any other group", "Asian or Pacific Islander", "Asian", "Chinese", "Filipino", "Korean", "Vietnamese", "Japanese", "Asian Indian", "Pacific Islander", "Hawaiian", "Samoan", "Guamanian", "Other Asian or Pacific Islander", "Other Asian or Pacific Islander (1992-1995)", "Other Asian or Pacific Islander (1996)", "Other Asian or Pacific Islander (1997-1998)", "Other Asian (1999 forward)", "Other Race", "Other Race (1963-1977)", "Other Race (1978)", "Other Race (1979-1991)", "Other Race (1992-1995)", "Other Race (1996)", "Other Race (1997-1998)", "Other Race (1999-2002)", "Primary Race not releasable", "Multiple Race, No Primary Race Selected", "Multiple Race, including Asian, excluding Black and White", "Multiple Race, including Asian and Black, excluding White", "Multiple Race, including Asian and White, excluding Black", "Multiple Race, including Black, excluding Asian and White", "Multiple Race, including Black and White, excluding Asian", "Multiple Race, including White, excluding Asian and Black", "Multiple Race, including Asian, White, and Black", "Multiple Race, excluding Asian, White, and Black"
                )),
  education = case_when(
    educ==100~"Grade 12 or less, no high school diploma or equivalent",
    educ==101~"Grade 8 or less (no further detail)",
    educ==102~"Never attended/kindergarten only",
    educ==103~"Grade 12 or less, no high school diploma or equivalent",
    educ==104~"Grade 12 or less, no high school diploma or equivalent",
    educ==105~"Grade 12 or less, no high school diploma or equivalent",
    educ==106~"Grade 12 or less, no high school diploma or equivalent",
    educ==107~"Grade 12 or less, no high school diploma or equivalent",
    educ==108~"Grade 12 or less, no high school diploma or equivalent",
    educ==109~"Grade 12 or less, no high school diploma or equivalent",
    educ==110~"Grade 12 or less, no high school diploma or equivalent",
    educ==111~"Grade 12 or less, no high school diploma or equivalent",
    educ==112~"Grade 12 or less, no high school diploma or equivalent",
    educ==113~"Grade 12 or less, no high school diploma or equivalent",
    educ==114~"Grade 12 or less, no high school diploma or equivalent",
    educ==115~"Grade 12 or less, no high school diploma or equivalent",
    educ==116~"Grade 12 or less, no high school diploma or equivalent",
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
  education = factor(
    education,
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

labeled_df_reduced_2021 <- labeled_df_reduced_2021 |> mutate(
  outcome = if_else(outcome==1, "Yes", "No"),
  outcome = as_factor(outcome),
  sex = if_else(sex==1,"Male", "Female"),
  sex = as_factor(sex),
  marital_status = case_when(
    marstat==10~"Married",
    marstat==20~"Widowed",
    marstat==30~"Divorced",
    marstat==40~"Separated",
    marstat==50~"Never married"
  ),
  marital_status = factor(marital_status, levels=c("Married","Widowed","Divorced","Separated","Never married")),
  race= case_when(
    racea==100~"White",
    racea==200~"Black/African-American",
    racea==300~"Aleut, Alaskan Native, or American Indian",
    racea==310~"Alaskan Native or American Indian",
    racea==320~"Alaskan Native/Eskimo",
    racea==330~"Aleut",
    racea==340~"American Indian",
    racea==350~"American Indian or Alaskan Native and any other group",
    racea==400~"Asian or Pacific Islander",
    racea==410~"Asian",
    racea==411~"Chinese",
    racea==412~"Filipino",
    racea==413~"Korean",
    racea==414~"Vietnamese",
    racea==415~"Japanese",
    racea==416~"Asian Indian",
    racea==420~"Pacific Islander",
    racea==421~"Hawaiian",
    racea==422~"Samoan",
    racea==423~"Guamanian",
    racea==430~"Other Asian or Pacific Islander",
    racea==431~"Other Asian or Pacific Islander (1992-1995)",
    racea==432~"Other Asian or Pacific Islander (1996)",
    racea==433~"Other Asian or Pacific Islander (1997-1998)",
    racea==434~"Other Asian (1999 forward)",
    racea==500~"Other Race",
    racea==510~"Other Race (1963-1977)",
    racea==520~"Other Race (1978)",
    racea==530~"Other Race (1979-1991)",
    racea==540~"Other Race (1992-1995)",
    racea==550~"Other Race (1996)",
    racea==560~"Other Race (1997-1998)",
    racea==570~"Other Race (1999-2002)",
    racea==580~"Primary Race not releasable",
    racea==600~"Multiple Race, No Primary Race Selected",
    racea==610~"Multiple Race, including Asian, excluding Black and White",
    racea==611~"Multiple Race, including Asian and Black, excluding White",
    racea==612~"Multiple Race, including Asian and White, excluding Black",
    racea==613~"Multiple Race, including Black, excluding Asian and White",
    racea==614~"Multiple Race, including Black and White, excluding Asian",
    racea==615~"Multiple Race, including White, excluding Asian and Black",
    racea==616~"Multiple Race, including Asian, White, and Black",
    racea==617~"Multiple Race, excluding Asian, White, and Black"
  ),
  race = factor(race, 
                levels=c("White", "Black/African-American", "Aleut, Alaskan Native, or American Indian", "Alaskan Native or American Indian", "Alaskan Native/Eskimo", "Aleut", "American Indian", "American Indian or Alaskan Native and any other group", "Asian or Pacific Islander", "Asian", "Chinese", "Filipino", "Korean", "Vietnamese", "Japanese", "Asian Indian", "Pacific Islander", "Hawaiian", "Samoan", "Guamanian", "Other Asian or Pacific Islander", "Other Asian or Pacific Islander (1992-1995)", "Other Asian or Pacific Islander (1996)", "Other Asian or Pacific Islander (1997-1998)", "Other Asian (1999 forward)", "Other Race", "Other Race (1963-1977)", "Other Race (1978)", "Other Race (1979-1991)", "Other Race (1992-1995)", "Other Race (1996)", "Other Race (1997-1998)", "Other Race (1999-2002)", "Primary Race not releasable", "Multiple Race, No Primary Race Selected", "Multiple Race, including Asian, excluding Black and White", "Multiple Race, including Asian and Black, excluding White", "Multiple Race, including Asian and White, excluding Black", "Multiple Race, including Black, excluding Asian and White", "Multiple Race, including Black and White, excluding Asian", "Multiple Race, including White, excluding Asian and Black", "Multiple Race, including Asian, White, and Black", "Multiple Race, excluding Asian, White, and Black"
                )),
  education = case_when(
    educ==100~"Grade 12 or less, no high school diploma or equivalent",
    educ==101~"Grade 8 or less (no further detail)",
    educ==102~"Never attended/kindergarten only",
    educ==103~"Grade 12 or less, no high school diploma or equivalent",
    educ==104~"Grade 12 or less, no high school diploma or equivalent",
    educ==105~"Grade 12 or less, no high school diploma or equivalent",
    educ==106~"Grade 12 or less, no high school diploma or equivalent",
    educ==107~"Grade 12 or less, no high school diploma or equivalent",
    educ==108~"Grade 12 or less, no high school diploma or equivalent",
    educ==109~"Grade 12 or less, no high school diploma or equivalent",
    educ==110~"Grade 12 or less, no high school diploma or equivalent",
    educ==111~"Grade 12 or less, no high school diploma or equivalent",
    educ==112~"Grade 12 or less, no high school diploma or equivalent",
    educ==113~"Grade 12 or less, no high school diploma or equivalent",
    educ==114~"Grade 12 or less, no high school diploma or equivalent",
    educ==115~"Grade 12 or less, no high school diploma or equivalent",
    educ==116~"Grade 12 or less, no high school diploma or equivalent",
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
  education = factor(
    education,
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


mental_illness_2018 <-
  stan_glm(
    outcome ~ sex + marital_status + race + education,
    data = labeled_df_reduced_2018,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept =
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )

mental_illness_2021 <-
  stan_glm(
    outcome ~ sex + marital_status + race + education,
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


modelsummary(
  list(
    "Severe Mental Illness" = mental_illness_2021
  ),
  statistic = "mad"
)






# Calculate log-odds (Intercept + Coefficient)
intercept <- coef(mental_illness_2021)["(Intercept)"]
coef_male <- coef(mental_illness_2021)["sexMale"]

# Calculate probabilities for both groups
prob_female <- exp(intercept) / (1 + exp(intercept))
prob_male <- exp(intercept + coef_male) / (1 + exp(intercept + coef_male))

# Why the probabilities are low? is okay?
cat("Probability for Females:", round(prob_female, 4), "\n")
cat("Probability for Males:", round(prob_male, 4), "\n")
