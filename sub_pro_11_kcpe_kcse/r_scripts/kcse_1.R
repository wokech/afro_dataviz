# KCSE candidates and grades (2006 to 2022)

# (A) Load the relevant libraries

library(readr)
library(readxl)
library(tidyverse)
library(janitor)

# (B) Load the required data

kcse_1 <- read_excel('sub_pro_11_kcpe_kcse/processed_tables/kcse_1.xlsx')
head(kcse_1)
str(kcse_1)

kcse_1_long_wide <- kcse_1 %>%
  pivot_longer(!1, names_to = "Years", values_to = "Count") %>%
  pivot_wider(names_from = Year, values_from = Count) 

kcse_1_long_wide_clean <- kcse_1_long_wide %>%
  clean_names() %>%
  select(-c("total_a_c", "total_students", "percentage_qualified"))

kcse_1_long_wide_clean <- kcse_1_long_wide_clean %>% 
  mutate(years = gsub("\\.{3}[0-9]*$","", years)) %>%
  rename(year = years, a_minus = a_2, b_plus = b, b = b_2, b_minus = b_3, c_plus = c) %>%
  mutate(year = as.numeric(year),
         a = as.numeric(a),
         a_minus = as.numeric(a_minus), 
         b_plus = as.numeric(b_plus),
         b = as.numeric(b),
         b_minus = as.numeric(b_minus),
         c_plus = as.numeric(c_plus))

kcse_1_long_wide_clean_male <- kcse_1_long_wide_clean %>%
  filter(gender == "Male")

kcse_1_long_wide_clean_female <- kcse_1_long_wide_clean %>%
  filter(gender == "Female")

kcse_1_long_wide_clean_total <- kcse_1_long_wide_clean %>%
  filter(gender == "Total")


