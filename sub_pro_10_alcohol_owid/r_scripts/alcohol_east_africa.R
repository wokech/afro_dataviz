# Alcohol consumption in East Africa

# (A) Load the relevant libraries

library(readr)
library(tidyverse)
library(janitor)

# (B) Load the required data

alcohol <- read_csv('sub_pro_10_alcohol/datasets/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv')
head(alcohol)

# (C) Clean the data and filter for Africa

alcohol_eac <- alcohol %>%
  clean_names() %>%
  rename("alcohol_consumption_per_capita" = "total_alcohol_consumption_per_capita_liters_of_pure_alcohol_projected_estimates_15_years_of_age",
         "country" = "entity") %>%
  filter(country %in% c("Kenya", "Tanzania", "Uganda", "Burundi", "Rwanda", "Democratic Republic of Congo"))

write.csv(alcohol_eac, "sub_pro_10_alcohol/datasets/alcohol_eac.csv")

