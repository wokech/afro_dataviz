# Northern White Rhino

# 1) Load the required libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)

# 2) Load and clean the required datasets

n_white_1 <- read_csv("sub_pro_12_conservation_owid/processed_tables/northern-white-rhinos.csv")

# Clean the column headings

n_white_1_clean <- n_white_1 %>%
  clean_names()

# Change the column title names

n_white_1_clean <- n_white_1_clean %>%
  rename("country" = "entity",
         "number" = "northern_white_rhino_population_af_rsg_other_sources_2019") %>%
  select(c(1,3,4)) 

# Get the World rhino population

n_white_1_clean_world <- n_white_1_clean %>%
  filter(country %in% c("World"))

# Pivot the data

n_white_1_clean_world_wide <- n_white_1_clean_world %>%
  pivot_wider(names_from = year, values_from = number)
