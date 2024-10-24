# Sex, Education, and Mental Depression: Analyzing Census Data Through Statistical Methods (U.S.A., 2018 and 2021) 

## Overview

Depression is a prevalent mental health issue in the United States, affecting individuals across many demographic groups. Previous research suggests that factors such as gender, education, and socioeconomic status contribute to differences in the experience and prevalence of psychological distress. This repository's project explores the intersection of some of these variables, using data from a Medical Expenditure Panel Survey (MEPS) dataset, taken from the Integrated Public Use Microdata Series (IPUMS) website, to analyze trends in severe psychological distress, as measured by the Kessler 6 Scale (K6SUM). The Kessler 6 Scale is a short screening tool which assesses nonspecific distress based on six key symptoms. Scores range from 0 to 24. Higher scores indicate greater distress; specifically, a score of 13 or higher indicates a high risk of severe psychological distress. In focus, this study examines the possible relationship between sex, education levels, and mental health outcomes before and after the COVID-19 pandemic, with two years in focus (2018 and 2021), using logistic regression models to examine the predictors of severe mental illness. The findings reveal gender disparities, with women consistently reporting higher rates of distress, and highlight the limited protective effect of education on mental health, raising important questions about societal and structural factors that impact emotional well-being. Through this analysis, the project's data and findings can help inform future interventions and policies that address mental health inequalities by targeting at-risk groups based on education and gender.

## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from IPUMS.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.


## Statement on LLM usage

None used
