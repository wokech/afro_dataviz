# GDP per Capita for 4 regions

# Load all the libraries

library(tidyverse)
library(janitor)
library(ggauto)

# Load the datasets

total_gdp_per_capita <- read_csv("sub_pro_12_tiger_tiger_cubs_lions_pumas/datasets/combined_datasets/total_gdp_per_capita.csv")

total_gdp_per_capita_tidy <- total_gdp_per_capita %>%
  pivot_longer(
    cols = -observation_date,
    names_to = "Country",
    values_to = "GDP per Capita"     
  ) |>
  clean_names()

total_gdp_per_capita_tidy <- total_gdp_per_capita_tidy |>
  mutate(region = case_when(
    country == "Vietnam" ~ "Asian Tiger Cubs",
    country == "Indonesia" ~ "Asian Tiger Cubs",
    country == "Malaysia" ~ "Asian Tiger Cubs",
    country == "Thailand" ~ "Asian Tiger Cubs",
    country == "The Philippines" ~ "Asian Tiger Cubs",
    country == "Peru" ~ "Pacific Pumas",
    country == "Mexico" ~ "Pacific Pumas",
    country == "Chile" ~ "Pacific Pumas",
    country == "Colombia" ~ "Pacific Pumas",
    country == "Republic of Korea" ~ "Asian Tigers",
    country == "Hong Kong SAR" ~ "Asian Tigers",
    country == "Singapore" ~ "Asian Tigers",
    country == "South Africa" ~ "African Lions",
    country == "Ethiopia" ~ "African Lions",
    country == "Ghana" ~ "African Lions",
    country == "Nigeria" ~ "African Lions",
    country == "Mozambique" ~ "African Lions",
    country == "Kenya" ~ "African Lions",
    TRUE ~ "Other"
  ))


total_gdp_per_capita_tidy |>
  dplyr::mutate(observation_date = ymd(observation_date)) |>
  ggauto(observation_date, gdp_per_capita, country)

