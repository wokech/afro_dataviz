# Fish and Seafood Production

# Sum of seafood from wild catch and fish farming (aquaculture)

# 1) Load the required libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)

# 2) Load and clean the required data sets

fish_seafood_1 <- read_csv("sub_pro_13_agriculture_owid/processed_tables/fish-seafood-production.csv")

# Clean the column headings

fish_seafood_1_clean <- fish_seafood_1 %>%
  clean_names()

# Change the column title names

fish_seafood_1_clean <- fish_seafood_1_clean %>%
  rename("country" = "entity",
         "fish_and_seafood_production_tonnes" = "fish_and_seafood_00002960_production_005511_tonnes") 

fish_seafood_1_clean_region <- fish_seafood_1_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

fish_seafood_1_clean_region_fao <- fish_seafood_1_clean_region %>%
  filter(grepl('(FAO)', country))

fish_seafood_1_clean_region_non_fao <- fish_seafood_1_clean_region %>%
  filter(!grepl('(FAO)', country))

fish_seafood_1_clean_country <- fish_seafood_1_clean %>%
  filter(!is.na(code)) %>%
  select(c(1,3,4)) 

fish_seafood_1_clean_world <- fish_seafood_1_clean_country %>%
  filter(country %in% c("World"))

# Pivot the data

fish_seafood_1_clean_world_wide <- fish_seafood_1_clean_world %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

write_csv(fish_seafood_1_clean_world_wide, "sub_pro_13_agriculture_owid/processed_tables/fish_seafood_1_clean_world_wide.csv")
