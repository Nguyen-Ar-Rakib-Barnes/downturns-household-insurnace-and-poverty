#### Preamble ####
# Purpose: Cleans the raw data - removed rows where K6SUM==98,96 
# Author: Marcus Barnes | Christina Nguyen | Nibras Ar Rakib
# Date: 11 Oct 2024
# Contact: nibras.rakib@mail.utoronto.ca
# License: MIT
# Pre-requisites: Download raw data from https://meps.ipums.org/meps/
#   package - arrow

#### Workspace setup ####
library(tidyverse)
library(arrow)
#### Clean data ####
raw_data <- read_csv("data/01-raw_data/meps_00003.csv")

cleaned_data <-
  raw_data |>
  janitor::clean_names()

cleaned_data <- cleaned_data |> select(year,pid,age,sex,marstat,racea,educ,inctot,k6sum)

# 96,98 - represent NIU, Unknown-not ascertained - check data dictionary for detail 

cleaned_data <- cleaned_data |> filter(!k6sum %in% c(96,98)) |> filter(inctot>0) 
cleaned_data$outcome <- ifelse(cleaned_data$k6sum<13,0,1)
#### Save data in parquet format ####
write_parquet(cleaned_data, "data/02-analysis_data/analysis_data.parquet")

df <- read_parquet("data/02-analysis_data/analysis_data.parquet")

# 996 - represent missing - check data dictionary for detail
# for all other code, go to data dictionary
### Model data ###
filtered_df_2018 <- df |> filter(year==2018) |> filter(age!=996) |>
  filter(age>18) |> filter(age<80) |>
  filter(sex %in% c(1,2)) |> filter(marstat %in% c(10,20,30,40,50,99)) |>
  filter(racea %in% c(100,200,300,310,320,330,340,350,400,410,411,412,413,414,415,416,420,421,422,423,430,431,432,433,434,500,510,520,530,540,550,560,570,580,600,610,611,612,613,614,615,616,617)) |> 
  filter(educ %in% c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,200,201,202,300,301,302,303,400,501,502,503)) # skipping 500 and 505 and 504

### Model data ###
filtered_df_2021 <- df |> filter(year==2021) |> filter(age!=996) |>
  filter(age>18) |> filter(age<80) |>
  filter(sex %in% c(1,2)) |> filter(marstat %in% c(10,20,30,40,50,99)) |>
  filter(racea %in% c(100,200,300,310,320,330,340,350,400,410,411,412,413,414,415,416,420,421,422,423,430,431,432,433,434,500,510,520,530,540,550,560,570,580,600,610,611,612,613,614,615,616,617)) |> 
  filter(educ %in% c(100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,200,201,202,300,301,302,303,400,501,502,503))  # skipping 500 and 505 and 504


# Reduce the categories for race, and education
labeled_df_reduced_2018 <- filtered_df_2018 |> mutate(
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
    educ==100~"Grade 12 or less",
    educ==101~"Grade 12 or less",
    educ==102~"Never attended/kindergarten only",
    educ==103~"Grade 12 or less",
    educ==104~"Grade 12 or less",
    educ==105~"Grade 12 or less",
    educ==106~"Grade 12 or less",
    educ==107~"Grade 12 or less",
    educ==108~"Grade 12 or less",
    educ==109~"Grade 12 or less",
    educ==110~"Grade 12 or less",
    educ==111~"Grade 12 or less",
    educ==112~"Grade 12 or less",
    educ==113~"Grade 12 or less",
    educ==114~"Grade 12 or less",
    educ==115~"Grade 12 or less",
    educ==116~"Grade 12 or less",
    educ==200~"High school",
    educ==201~"High school",
    educ==202~"GED or equivalent",
    educ==300~"Some college",
    educ==301~"Some college",
    educ==302~"AA degree",
    educ==303~"AA degree",
    educ==400~"Bachelor's",
    educ==501~"Master's",
    educ==502~"Professional",
    educ==503~"Doctoral"
  ),
  education = factor(
    education,
    levels=c(
      "Grade 12 or less","Never attended/kindergarten only","High school","GED or equivalent","Some college","AA degree","Bachelor's","Master's","Professional","Doctoral"
    )
  )
)

labeled_df_reduced_2021 <- filtered_df_2021 |> mutate(
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
    educ==100~"Grade 12 or less",
    educ==101~"Grade 12 or less",
    educ==102~"Never attended/kindergarten only",
    educ==103~"Grade 12 or less",
    educ==104~"Grade 12 or less",
    educ==105~"Grade 12 or less",
    educ==106~"Grade 12 or less",
    educ==107~"Grade 12 or less",
    educ==108~"Grade 12 or less",
    educ==109~"Grade 12 or less",
    educ==110~"Grade 12 or less",
    educ==111~"Grade 12 or less",
    educ==112~"Grade 12 or less",
    educ==113~"Grade 12 or less",
    educ==114~"Grade 12 or less",
    educ==115~"Grade 12 or less",
    educ==116~"Grade 12 or less",
    educ==200~"High school",
    educ==201~"High school",
    educ==202~"GED or equivalent",
    educ==300~"Some college",
    educ==301~"Some college",
    educ==302~"AA degree",
    educ==303~"AA degree",
    educ==400~"Bachelor's",
    educ==501~"Master's",
    educ==502~"Professional",
    educ==503~"Doctoral"
  ),
  education = factor(
    education,
    levels=c(
      "Grade 12 or less","Never attended/kindergarten only","High school","GED or equivalent","Some college","AA degree","Bachelor's","Master's","Professional","Doctoral"
    )
  )
)


write_parquet(labeled_df_reduced_2018, "data/02-analysis_data/model_data_2018.parquet")
write_parquet(labeled_df_reduced_2021, "data/02-analysis_data/model_data_2021.parquet")


set.seed(555)

labeled_df_reduced_2018 <- labeled_df_reduced_2018 |> slice_sample(n = 5000)
labeled_df_reduced_2021 <- labeled_df_reduced_2021 |> slice_sample(n = 5000)

write_parquet(labeled_df_reduced_2018, "data/02-analysis_data/model_data_sample_2018_5k.parquet")
write_parquet(labeled_df_reduced_2021, "data/02-analysis_data/model_data_sample_2021_5k.parquet")

