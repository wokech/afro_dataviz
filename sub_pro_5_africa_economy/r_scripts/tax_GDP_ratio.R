# An analysis of tax revenue to GDP ratio

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Get the data from Wikipedia

link <- "https://en.wikipedia.org/wiki/List_of_sovereign_states_by_tax_revenue_to_GDP_ratio"
tax_GDP_world <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

tax_GDP_world_1 <- tax_GDP_world[[6]]
