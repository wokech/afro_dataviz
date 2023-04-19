# An analysis of Kenyan counties by GDP

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)

# (B) Get the data from Wikipedia

link <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP"
kenya_county <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

kenya_county_GDP <- kenya_county[[1]]
kenya_county_GDP_capita <- kenya_county[[2]]

# (C) Clean the data, fix columns and county labels

