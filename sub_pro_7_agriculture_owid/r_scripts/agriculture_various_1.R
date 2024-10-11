# Various agricultural datasets from OWID

# Here we look at multiple datasets:
# 1) Vegetable consumption per capita;
# 2) Fruit consumption per capita;
# 3) Share of land area used for arable agriculture
# 4) Nitrogen fertilizer application per hectare of cropland
# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(janitor)
library(scales)
library(devtools)
library(treemapify)
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)

# Load the datasets

# Vegetable Consumption
veg_consumption <- read_csv("sub_pro_7_agriculture_owid/datasets/vegetable-consumption-per-capita.csv")

# Fruit Consumption
fruit_consumption <- read_csv("sub_pro_7_agriculture_owid/datasets/fruit-consumption-per-capita.csv")

# Share of land area used for arable agriculture
share_land_area_ag <- read_csv("sub_pro_7_agriculture_owid/datasets/share-of-land-area-used-for-arable-agriculture.csv")

# Nitogen Fertilizer App
nitrogen_fertilizer_app <- read_csv("sub_pro_7_agriculture_owid/datasets/nitrogen-fertilizer-application-per-hectare-of-cropland.csv")

# Crop yields
crop_yields <- read_csv("sub_pro_7_agriculture_owid/datasets/global-food.csv")

# Clean the datasets

veg_consumption_clean <- veg_consumption %>%
  clean_names() |>
  rename(veg_consumption_per_capita = vegetables_00002918_food_available_for_consumption_0645pc_kilograms_per_year_per_capita)

fruit_consumption_clean <- fruit_consumption %>%
  clean_names() |>
  rename(fruit_consumption_per_capita = fruit_00002919_food_available_for_consumption_0645pc_kilograms_per_year_per_capita)

share_land_area_ag_clean <- share_land_area_ag %>%
  clean_names()

nitrogen_fertilizer_app_clean <- nitrogen_fertilizer_app %>%
  clean_names() |>
  rename(nitrogen_per_hectare = nutrient_nitrogen_n_total_00003102_use_per_area_of_cropland_005159_kilograms_per_hectare)

crop_yields_clean <- crop_yields %>%
  clean_names()

# Create datasets for plotting

veg_consumption_clean_countries <- veg_consumption_clean |>
  filter(!is.na(code))

veg_consumption_clean_regions <- veg_consumption_clean |>
  filter(is.na(code))

fruit_consumption_clean_countries <- fruit_consumption_clean |>
  filter(!is.na(code))

fruit_consumption_clean_regions <- fruit_consumption_clean |>
  filter(is.na(code))

share_land_area_ag_clean_countries <- share_land_area_ag_clean |>
  filter(!is.na(code))

share_land_area_ag_clean_regions <- share_land_area_ag_clean |>
  filter(is.na(code))

nitrogen_fertilizer_app_clean_countries <- nitrogen_fertilizer_app_clean |>
  filter(!is.na(code))

nitrogen_fertilizer_app_clean_regions <- nitrogen_fertilizer_app_clean |>
  filter(is.na(code))

# Only include African Countries

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
                       "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                       "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                       "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
                       "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
                       "Uganda", "Zambia", "Zimbabwe")

# Vegetable Consumption in Africa

veg_consumption_clean_countries_africa <- veg_consumption_clean_countries |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

#############
# Check if the values in the african_countries dataset are present in new dataframes

unique(veg_consumption_clean_countries_africa$country)

african_countries[!(african_countries %in% unique(veg_consumption_clean_countries_africa$country))]

#############

# Fruit Consumption in Africa

fruit_consumption_clean_countries_africa <- fruit_consumption_clean_countries |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

#############
# Check if the values in the african_countries dataset are present in new dataframes

unique(fruit_consumption_clean_countries_africa$country)

african_countries[!(african_countries %in% unique(fruit_consumption_clean_countries_africa$country))]

#############

# Share of land area in Africa

share_land_area_ag_clean_countries_africa <- share_land_area_ag_clean_countries |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

#############
# Check if the values in the african_countries dataset are present in new dataframes

unique(share_land_area_ag_clean_countries_africa$country)

african_countries[!(african_countries %in% unique(share_land_area_ag_clean_countries_africa$country))]

#############

# Nitrogen Fertilizer Application in Africa

nitrogen_fertilizer_app_clean_countries_africa <- nitrogen_fertilizer_app_clean_countries |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

#############
# Check if the values in the african_countries dataset are present in new dataframes

unique(nitrogen_fertilizer_app_clean_countries_africa$country)

african_countries[!(african_countries %in% unique(nitrogen_fertilizer_app_clean_countries_africa$country))]

#############

