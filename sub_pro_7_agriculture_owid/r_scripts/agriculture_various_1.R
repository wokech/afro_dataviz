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

# Select the correct columns
crop_yields_clean <- crop_yields %>%
  clean_names()
