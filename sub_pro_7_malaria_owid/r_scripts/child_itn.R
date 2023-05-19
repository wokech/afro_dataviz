# Children and ITNs
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

child_itn <- read_csv("sub_pro_7_malaria_owid/processed_tables/children_itn_bednet.csv")

# Clean the column headings

child_itn_clean <- child_itn %>%
  clean_names()

# Change the column title names

child_itn_clean <- child_itn_clean %>%
  rename("country" = "entity",
         "itn_use" = "use_of_insecticide_treated_bed_nets_percent_of_under_5_population") %>%
  select(c(1,3,4)) 

# Get the EAC malaria death rates

child_itn_clean_eac <- child_itn_clean %>%
  filter(country %in% c("Kenya", "Uganda", "Tanzania", 
                        "Burundi", "Rwanda", "South Sudan", 
                        "Democratic Republic of Congo"))

## For Flourish pivot the data

child_itn_clean_eac_wide <- child_itn_clean_eac %>%
  pivot_wider(names_from = year, values_from = itn_use)

flags <- c("https://public.flourish.studio/country-flags/svg/bi.svg",
           "https://public.flourish.studio/country-flags/svg/cd.svg",
           "https://public.flourish.studio/country-flags/svg/ke.svg",
           "https://public.flourish.studio/country-flags/svg/rw.svg",
           "https://public.flourish.studio/country-flags/svg/ss.svg",
           "https://public.flourish.studio/country-flags/svg/tz.svg",
           "https://public.flourish.studio/country-flags/svg/ug.svg")

child_itn_clean_eac_wide$flags <- flags

write.csv(child_itn_clean_eac_wide, file = "sub_pro_7_malaria_owid/processed_tables/child_itn_wide_fluorish.csv")
