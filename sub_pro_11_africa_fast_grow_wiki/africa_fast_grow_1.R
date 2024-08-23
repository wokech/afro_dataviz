# Africa’s Fastest Growing Companies 2023 and 2024
# Website: https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477
# Website: https://www.ft.com/africas-fastest-growing-companies-2023

# A) Load the required packages and libraries

#install.packages('rvest')
library(rvest)

# B) Scrape the data

# 2023

# Specify the URL of the webpage
url_2023 <- "https://www.ft.com/africas-fastest-growing-companies-2023"

# Read the HTML content
webpage_2023 <- read_html(url_2023)

# Extract the table (adjust CSS selector as needed)
table_data_2023 <- html_table(webpage_2023, fill = TRUE)[[1]]

# View the scraped table
View(table_data_2023)

# 2024

# Specify the URL of the webpage
url_2024 <- "https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477"

# Read the HTML content
webpage_2024 <- read_html(url_2024)

# Extract the table (adjust CSS selector as needed)
table_data_2024 <- html_table(webpage_2024, fill = TRUE)[[1]]

# Print the scraped table
View(table_data_2024)

# C) Data cleaning 

table_data_2023_clean <- table_data_2023 %>%
  clean_names()
table_data_2024_clean <- table_data_2024 %>%
  clean_names()

# Save the data

write.csv(table_data_2023_clean, "sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2023")
write.csv(table_data_2024_clean, "sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2024")

# D) EDA

# Which companies are in both tables? 

merge_1 <- merge(table_data_2023_clean, table_data_2024_clean, by = "company_name")
merge_2 <- merge(table_data_2023_clean, table_data_2024_clean, by = "brand")

# Where do most companies come from?

table_data_2023_clean_country <- table_data_2023_clean %>%
  group_by(country) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_country <- table_data_2024_clean %>%
  group_by(country) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# What sector do the companies work in?

table_data_2023_clean_sector <- table_data_2023_clean %>%
  group_by(sector) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_sector <- table_data_2024_clean %>%
  group_by(sector) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# When were the companies founded?

table_data_2023_clean_year <- table_data_2023_clean %>%
  group_by(founding_year) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_year <- table_data_2024_clean %>%
  group_by(founding_year) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Grouped data by year

table_data_2023_clean_year_group <- table_data_2023_clean %>%
  mutate(founding_year_group = case_when(
    founding_year >= 1850 & founding_year <= 1950 ~ "1850-1950",
    founding_year >= 1951 & founding_year <= 1975 ~ "1951-1975",
    founding_year >= 1976 & founding_year <= 2000 ~ "1976-2000",
    founding_year >= 2001 & founding_year <= 2012 ~ "2001-2012",
    founding_year >= 2013 & founding_year <= 2024 ~ "2013-2024", 
    )) %>%
  group_by(founding_year_group) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_year_group <- table_data_2024_clean %>%
  mutate(founding_year_group = case_when(
    founding_year >= 1850 & founding_year <= 1950 ~ "1850-1950",
    founding_year >= 1951 & founding_year <= 1975 ~ "1951-1975",
    founding_year >= 1976 & founding_year <= 2000 ~ "1976-2000",
    founding_year >= 2001 & founding_year <= 2012 ~ "2001-2012",
    founding_year >= 2013 & founding_year <= 2024 ~ "2013-2024", 
  )) %>%
  group_by(founding_year_group) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

