# Wikipedia - EAC regions via HDI
# https://en.wikipedia.org/wiki/List_of_East_African_Community_sub_regions_by_Human_Development_Index

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

# link <- "https://en.wikipedia.org/wiki/List_of_East_African_Community_sub_regions_by_Human_Development_Index"
# eac_regions_hdi <- link %>%
#   read_html("[class='wikitable sortable']") %>%
#   html_table(fill = TRUE)

# eac_regions_hdi_table <- eac_regions_hdi[[1]]

#NEED TO ADD COUNTRIES TO THE DATASET#

# write_csv(eac_regions_hdi_table, "sub_pro_5_africa_economy_wiki/processed_datasets/eac_regions_hdi_table.csv")

eac_regions_hdi_table <- read_csv("sub_pro_5_africa_economy_wiki/processed_datasets/eac_regions_hdi_table.csv")

