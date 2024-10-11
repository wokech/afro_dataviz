# Olympics and Paralympics (2024) in Africa

# Olympics: https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table

# Paralympics: https://www.paralympic.org/en/paris-2024-paralympics/medals

# (A) Load the required libraries

library(readxl)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Download the data

# link <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"

# olympics_2024 <- link %>%
#   read_html("[class='wikitable sortable']") %>%
#   html_table(fill = TRUE)

# olympics_2024_1 <- olympics_2024[[4]]

# olympics_2024_1 <- olympics_2024_1 |>
#   clean_names() |>
#   rename(country = noc) |>
#   mutate(total = as.numeric(total))

# Save datasets

# write_csv(olympics_2024_1, "sub_pro_9_olympics_wpr_ioc/datasets/olympics_2024")

# (C) Load the required datasets

olympics_2024 <- read_csv("sub_pro_9_olympics_wpr_ioc/datasets/olympics_2024") |>
  clean_names()
paralympics_2024 <- read_excel("sub_pro_9_olympics_wpr_ioc/datasets/paralympics_2024.xlsx") |>
  clean_names()

# Get the Africa datasets

olympics_africa_2024 <- olympics_2024 |>
  filter(country %in% c("Kenya", "Algeria", "South Africa", "Ethiopia", 
                        "Egypt", "Tunisia", "Botswana", "Uganda", 
                        "Morocco", "Cape Verde", "Ivory Coast", "Zambia"))

paralympics_africa_2024 <- paralympics_2024 |>
  filter(country %in% c("Morocco", "Algeria", "Tunisia", "Nigeria",
                        "Egypt", "South Africa", "Ethiopia", "Namibia",
                        "Kenya", "Mauritius"))
  
