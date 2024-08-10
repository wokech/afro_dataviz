# African Elephants (2019)

# Load the required libraries

library(tidyverse)
library(janitor)

# Load the dataset

afri_elephant <- read_csv("sub_pro_6_conservation_owid/processed_tables/african-elephants.csv")

# Clean the names

afri_elephant_clean <- afri_elephant %>%
  clean_names()

# Create new tables

afri_elephant_region <- afri_elephant_clean %>%
  filter(entity %in% c("Eastern Africa", "Western Africa", "Southern Africa")) %>%
  rename(population = african_elephant_population_af_esg_2019)


# EDA

ggplot(afri_elephant_region, aes(year, population, color = entity)) +
  geom_line()
