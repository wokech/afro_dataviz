# Population share with malaria
# Racing bar chart

## Example
## https://www.infoworld.com/article/3633448/easy-racing-bar-charts-in-r-with-ddplot.html

# 1) Load the packages required for the maps

# Solve package loading issues with options(timeout = 600) 
# increase download length time

#install.packages("sf")
library(sf) # simple features
library(tidyverse)
library(ggplot2)
library(ggrepel)
#install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(rKenyaCensus)
library(patchwork)
library(janitor)
#install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(ggthemes)
library(scales)
library(readxl)
library(readr)
#install.packages("ddplot")
library(ddplot)

# 2) Load the data

population_share_malaria <- read_csv("sub_pro_7_malaria_owid/processed_tables/population_share_malaria.csv")

# Clean the column headings

population_share_malaria_clean <- population_share_malaria %>%
  clean_names()

# Change the column title names

population_share_malaria_clean <- population_share_malaria_clean %>%
  rename("country" = "entity",
         "pop_share_malaria" = "prevalence_malaria_sex_both_age_age_standardized_percent") %>%
  select(c(1,3,4)) 

# Get the EAC malaria death rates

population_share_malaria_clean_eac <- population_share_malaria_clean %>%
  filter(country %in% c("Kenya", "Uganda", "Tanzania", 
                        "Burundi", "Rwanda", "South Sudan", 
                        "Democratic Republic of Congo"))

## For Flourish pivot the data

population_share_malaria_clean_eac_wide <- population_share_malaria_clean_eac %>%
  pivot_wider(names_from = year, values_from = pop_share_malaria)

flags <- c("https://public.flourish.studio/country-flags/svg/bi.svg",
           "https://public.flourish.studio/country-flags/svg/cd.svg",
           "https://public.flourish.studio/country-flags/svg/ke.svg",
           "https://public.flourish.studio/country-flags/svg/rw.svg",
           "https://public.flourish.studio/country-flags/svg/ss.svg",
           "https://public.flourish.studio/country-flags/svg/tz.svg",
           "https://public.flourish.studio/country-flags/svg/ug.svg")

population_share_malaria_clean_eac_wide$flags <- flags

write.csv(population_share_malaria_clean_eac_wide, file = "sub_pro_7_malaria_owid/processed_tables/population_share_malaria_wide_fluorish.csv")
