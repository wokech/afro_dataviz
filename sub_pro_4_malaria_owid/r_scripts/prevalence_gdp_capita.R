# Prevalence of malaria and gdp per capita
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

prevalence_gdp_capita <- read_csv("sub_pro_7_malaria_owid/processed_tables/prevalence_gdp_capita.csv")

# Clean the column headings

prevalence_gdp_capita_clean <- prevalence_gdp_capita %>%
  clean_names()

# Change the column title names

prevalence_gdp_capita_clean <- prevalence_gdp_capita_clean %>%
  rename("country" = "entity",
         "prevalence_malaria_percent" = "prevalence_malaria_sex_both_age_age_standardized_percent",
         "gdp_capita" = "gdp_per_capita_ppp_constant_2017_international",
         "population" = "population_historical_estimates")
         ) %>%
  select(c(1,3,4,5,6)) 

# Get the EAC malaria death rates

prevalence_gdp_capita_clean_eac <- prevalence_gdp_capita_clean %>%
  filter(country %in% c("Kenya", "Uganda", "Tanzania", 
                        "Burundi", "Rwanda", "South Sudan", 
                        "Democratic Republic of Congo"))

## For Flourish pivot the data - May not need!!


flags <- c("https://public.flourish.studio/country-flags/svg/bi.svg",
           "https://public.flourish.studio/country-flags/svg/cd.svg",
           "https://public.flourish.studio/country-flags/svg/ke.svg",
           "https://public.flourish.studio/country-flags/svg/rw.svg",
           "https://public.flourish.studio/country-flags/svg/ss.svg",
           "https://public.flourish.studio/country-flags/svg/tz.svg",
           "https://public.flourish.studio/country-flags/svg/ug.svg")

## population_share_malaria_clean_eac_wide$flags <- flags

## write.csv(population_share_malaria_clean_eac_wide, file = "sub_pro_7_malaria_owid/processed_tables/population_share_malaria_wide_fluorish.csv")
