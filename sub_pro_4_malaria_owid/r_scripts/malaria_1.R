# Malaria Analysis using OWID Data
# Various datasets are assessed:
# 1) Children and Insecticide-Treated Nets
# 2) Malaria Child Deaths
# 3) Malaria Death Rates
# 4) Malaria Incidence
# 5) Malaria Total Deaths
# 6) Share of the Population with Malaria
# 7) Prevalence of malaria and gdp per capita

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)

# B) Load data

# 1) Children and Insecticide-Treated Nets

child_itn <- read_csv("sub_pro_4_malaria_owid/datasets/children_itn_bednet.csv")

# Clean the column headings

child_itn_clean <- child_itn %>%
  clean_names()

# Change the column title names

child_itn_clean <- child_itn_clean %>%
  rename("country" = "entity",
         "itn_use" = "use_of_insecticide_treated_bed_nets_percent_of_under_5_population") 

# 2) Malaria Child Deaths

malaria_child_deaths <- read_csv("sub_pro_4_malaria_owid/datasets/malaria_child_deaths.csv")

# Clean the column headings

malaria_child_deaths_clean <- malaria_child_deaths %>%
  clean_names()

# Change the column title names

malaria_child_deaths_clean <- malaria_child_deaths_clean %>%
  rename("country" = "entity",
         "child_death" = "deaths_malaria_sex_both_age_under_5_rate")

# 3) Malaria Death Rates

malaria_death <- read_csv("sub_pro_4_malaria_owid/datasets/malaria_death_rates.csv")

# Clean the column headings

malaria_death_clean <- malaria_death %>%
  clean_names()

# Change the column title names

malaria_death_clean <- malaria_death_clean %>%
  rename("country" = "entity",
         "death_rate" = "deaths_malaria_sex_both_age_age_standardized_rate")

# 4) Malaria Incidence

malaria_incidence <- read_csv("sub_pro_4_malaria_owid/datasets/malaria_incidence.csv")

# Clean the column headings

malaria_incidence_clean <- malaria_incidence %>%
  clean_names()

# Change the column title names

malaria_incidence_clean <- malaria_incidence_clean %>%
  rename("country" = "entity",
         "incidence" = "incidence_of_malaria_per_1_000_population_at_risk")

# 5) Malaria Total Deaths

malaria_total_deaths <- read_csv("sub_pro_4_malaria_owid/datasets/malaria_total_deaths.csv")

# Clean the column headings

malaria_total_deaths_clean <- malaria_total_deaths %>%
  clean_names()

# Change the column title names

malaria_total_deaths_clean <- malaria_total_deaths_clean %>%
  rename("country" = "entity",
         "total_death" = "deaths_malaria_sex_both_age_all_ages_number")

# 6) Share of the Population with Malaria

population_share_malaria <- read_csv("sub_pro_4_malaria_owid/datasets/population_share_malaria.csv")

# Clean the column headings

population_share_malaria_clean <- population_share_malaria %>%
  clean_names()

# Change the column title names

population_share_malaria_clean <- population_share_malaria_clean %>%
  rename("country" = "entity",
         "pop_share_malaria" = "prevalence_malaria_sex_both_age_age_standardized_percent")

# 7) Prevalence of malaria and gdp per capita

prevalence_gdp_capita <- read_csv("sub_pro_4_malaria_owid/datasets/prevalence_gdp_capita.csv")

# Clean the column headings

prevalence_gdp_capita_clean <- prevalence_gdp_capita %>%
  clean_names()

# Change the column title names

prevalence_gdp_capita_clean <- prevalence_gdp_capita_clean %>%
  rename("country" = "entity",
         "prevalence_malaria_percent" = "prevalence_malaria_sex_both_age_age_standardized_percent",
         "gdp_capita" = "gdp_per_capita_ppp_constant_2017_international",
         "population" = "population_historical_estimates")

# EDA 

