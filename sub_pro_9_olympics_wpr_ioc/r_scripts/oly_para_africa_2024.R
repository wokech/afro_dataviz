# Paralympics and African Countries

# https://www.paralympic.org/en/paris-2024-paralympics/medals

# (A) Load the required libraries

library(readxl)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Load the webpage

link <- "https://en.wikipedia.org/wiki/2024_Summer_Olympics_medal_table"

olympics_2024 <- link %>%
  read_html("[class='wikitable sortable']") %>%
  html_table(fill = TRUE)

olympics_2024_1 <- olympics_2024[[4]]

olympics_2024_1 <- olympics_2024_1 |>
  clean_names() |>
  rename(country = noc) |>
  mutate(total = as.numeric(total))

# Save datasets

write.csv(olympics_2024_1, "sub_pro_9_olympics_wpr_ioc/datasets/olympics_2024")
