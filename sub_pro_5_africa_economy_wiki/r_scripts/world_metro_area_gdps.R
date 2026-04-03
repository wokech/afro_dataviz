# Wikipedia - World Metro Area GDPs

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(treemapify)
library(scales)
library(ggrepel)

# (C) Get the data from Wikipedia

#link <- "https://en.wikipedia.org/wiki/List_of_metropolitan_areas_by_GDP"
#metro_areas_gdp <- link %>%
#  read_html("[class='wikitable sortable']") %>%
#  html_table(fill = TRUE)

#metro_areas_gdp_table <- metro_areas_gdp[[1]]

#write_csv(metro_areas_gdp_table, "sub_pro_5_africa_economy_wiki/processed_datasets/metro_areas_gdp_table.csv")

metro_areas_gdp_table <- read_csv("sub_pro_5_africa_economy_wiki/processed_datasets/metro_areas_gdp_table.csv")
