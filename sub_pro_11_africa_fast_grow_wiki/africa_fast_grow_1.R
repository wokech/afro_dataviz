# Africa’s Fastest Growing Companies 2023 and 2024
# Website: https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477
# Website: https://www.ft.com/africas-fastest-growing-companies-2023

# A) Load the required packages and libraries

install.packages('rvest')
library(rvest)

# B) Scrape the data

# 2023

# Specify the URL of the webpage
url_2023 <- "https://www.ft.com/africas-fastest-growing-companies-2023"

# Read the HTML content
webpage_2023 <- read_html(url_2023)

# Extract the table (adjust CSS selector as needed)
table_data_2023 <- html_table(webpage_2023, fill = TRUE)[[1]]

# Print the scraped table
print(table_data_2023)

# 2024

# Specify the URL of the webpage
url_2024 <- "https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477"

# Read the HTML content
webpage_2024 <- read_html(url_2024)

# Extract the table (adjust CSS selector as needed)
table_data_2024 <- html_table(webpage_2024, fill = TRUE)[[1]]

# Print the scraped table
print(table_data_2024)

# Which companies are in both tables

merge_1 <- merge(table_data_2023, table_data_2024, by = "Company name")
merge_2 <- merge(table_data_2023, table_data_2024, by = "Brand")

# C) Data cleaning 



# D) EDA