# Wildlife Populations in Africa from OWID Datasets

# Datasets used

# 1) African Elephant
# 2) Rhino (White and Black)

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)

# Load data

# African Elephants
african_elephants <- read_csv("sub_pro_6_conservation_owid/datasets/african-elephants.csv")

# Black Rhinos
black_rhinos <- read_csv("sub_pro_6_conservation_owid/datasets/black-rhinos.csv")

# Northern White Rhinos
northern_white_rhinos <- read_csv("sub_pro_6_conservation_owid/datasets/northern-white-rhinos.csv")

# Southern White Rhinos
southern_white_rhinos <- read_csv("sub_pro_6_conservation_owid/datasets/southern-white-rhinos.csv")

# Clean datasets

african_elephants <- african_elephants %>%
  clean_names() |>
  rename(af_elephant_pop = african_elephant_population_af_esg_2019)

black_rhinos <- black_rhinos %>%
  clean_names() |>
  rename(black_rhino_pop = black_rhino_population_af_rsg_other_sources_2019)

northern_white_rhinos <- northern_white_rhinos %>%
  clean_names() |>
  rename(northern_white_rhino_pop = northern_white_rhino_population_af_rsg_other_sources_2019)

southern_white_rhinos <- southern_white_rhinos %>%
  clean_names() |>
  rename(southern_white_rhino_pop = southern_white_rhino_population_af_rsg_other_sources_2019)

# Set up datasets

# 1) Elephants

# Total African Elephants

african_elephants_only <- african_elephants |>
  filter(entity == "Africa")

# African Elephants by Region

african_elephants_regions <- african_elephants %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Central Africa",
                       "Southern Africa", "Western Africa"))

# African Elephants by Country (Greater than 10,000)

african_elephants_country <- african_elephants |>
  filter(!is.na(code)) |>
  filter(af_elephant_pop > 10000)

# 2) Black Rhino

# Black Rhinos in Africa

black_rhinos_world <- black_rhinos |>
  filter(code == "OWID_WRL")

black_rhinos_country <- black_rhinos |>
  filter(code != "OWID_WRL")

# Plot top countries for black rhinos

##########

# 3) Northern White Rhino

northern_white_rhinos_world <- northern_white_rhinos |>
  filter(code == "OWID_WRL")

northern_white_rhinos_country <- northern_white_rhinos |>
  filter(code != "OWID_WRL")

# Plot top countries for n w rhinos

##########

# 4) Southern White Rhino

southern_white_rhinos_world <- southern_white_rhinos |>
  filter(code == "OWID_WRL")

southern_white_rhinos_country <- southern_white_rhinos |>
  filter(code != "OWID_WRL")

# Plot top countries for s w rhinos

##########

# B) EDA and Basic Plots

