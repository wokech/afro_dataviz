# Africa’s Fastest Growing Companies 2023 and 2024
# Website: https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477
# Website: https://www.ft.com/africas-fastest-growing-companies-2023

# A) Load the required packages and libraries

#install.packages('rvest')
library(rvest)
library(tidyverse)
library(janitor)

# B) Scrape the data

# # 2022
# 
# # Specify the URL of the webpage
# url_2022 <- "https://www.ft.com/africas-fastest-growing-companies"
# 
# # Read the HTML content
# webpage_2022 <- read_html(url_2022)
# 
# # Extract the table (adjust CSS selector as needed)
# table_data_2022 <- html_table(webpage_2022, fill = TRUE)[[1]]
# 
# # View the scraped table
# View(table_data_2022)

# 2023

# # Specify the URL of the webpage
# url_2023 <- "https://www.ft.com/africas-fastest-growing-companies-2023"
# 
# # Read the HTML content
# webpage_2023 <- read_html(url_2023)
# 
# # Extract the table (adjust CSS selector as needed)
# table_data_2023 <- html_table(webpage_2023, fill = TRUE)[[1]]
# 
# # View the scraped table
# View(table_data_2023)
# 
# # 2024
# 
# # Specify the URL of the webpage
# url_2024 <- "https://www.ft.com/content/a1bc5d2e-046e-499b-b27e-4d057f9d8477"
# 
# # Read the HTML content
# webpage_2024 <- read_html(url_2024)
# 
# # Extract the table (adjust CSS selector as needed)
# table_data_2024 <- html_table(webpage_2024, fill = TRUE)[[1]]
# 
# # Print the scraped table
# View(table_data_2024)

# C) Data cleaning 

# table_data_2022_clean <- table_data_2022 %>%
#   clean_names() |>
#   select(c(rank:founding_year)) |>
#   filter(row_number() <= n()-1)
# table_data_2023_clean <- table_data_2023 %>%
#   clean_names()
# table_data_2024_clean <- table_data_2024 %>%
#   clean_names()

# Save the data

# write.csv(table_data_2022_clean, "sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2022")
# write.csv(table_data_2023_clean, "sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2023")
# write.csv(table_data_2024_clean, "sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2024")

# Get the data

table_data_2022_clean <- read_csv("sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2022")
table_data_2023_clean <- read_csv("sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2023")
table_data_2024_clean <- read_csv("sub_pro_11_africa_fast_grow_wiki/datasets/fast_grow_2024")

# D) EDA

# Which companies are in all tables? 

# Review this section - NOT CLEAR HOW TO WORK ON THIS...

# Use intersect / semi_join / merge

companies_2022_2024_1 <- intersect(intersect(table_data_2022_clean$company_name, 
                                           table_data_2023_clean$company_name), 
                                 table_data_2024_clean$company_name)

companies_2022_2024_2 <- table_data_2022_clean %>%
  semi_join(table_data_2023_clean, by = "company_name") %>%
  semi_join(table_data_2024_clean, by = "company_name") %>%
  pull(company_name) %>%
  unique()

companies_2022_2024_3 <- intersect(intersect(table_data_2022_clean$brand, 
                                             table_data_2023_clean$brand), 
                                   table_data_2024_clean$brand
                                   )

companies_2022_2024_4 <- table_data_2022_clean %>%
  semi_join(table_data_2023_clean, by = "brand") %>%
  semi_join(table_data_2024_clean, by = "brand") %>%
  pull(brand) %>%
  unique()

merge_1 <- merge(table_data_2023_clean, table_data_2024_clean, by = "company_name")
merge_2 <- merge(table_data_2023_clean, table_data_2024_clean, by = "brand")

# Where do most companies come from?

# 2022

table_data_2022_clean_country <- table_data_2022_clean %>%
  group_by(country) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2022_clean_country |>
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = country, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 10) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2022)",
       subtitle = "Country of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2022/origin_2022.png", width = 12, height = 12, dpi = 300)

# 2023

table_data_2023_clean_country <- table_data_2023_clean %>%
  group_by(country) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2023_clean_country |>
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = country, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 10) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2023)",
       subtitle = "Country of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2023/origin_2023.png", width = 12, height = 12, dpi = 300)

# 2024

table_data_2024_clean_country <- table_data_2024_clean %>%
  group_by(country) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_country |>
  ggplot(aes(x = reorder(country, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = country, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 10) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2024)",
       subtitle = "Country of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2024/origin_2024.png", width = 12, height = 12, dpi = 300)

# What sector do the companies work in?

# 2022

table_data_2022_clean_sector <- table_data_2022_clean %>%
  group_by(sector) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2022_clean_sector |>
  ggplot(aes(x = reorder(sector, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = sector, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 8) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2022)",
       subtitle = "Sector",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.4))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2022/sector_2022.png", width = 12, height = 12, dpi = 300)

# 2023

table_data_2023_clean_sector <- table_data_2023_clean %>%
  group_by(sector) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2023_clean_sector |>
  ggplot(aes(x = reorder(sector, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = sector, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 8) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2023)",
       subtitle = "Sector",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.95))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2023/sector_2023.png", width = 12, height = 12, dpi = 300)

# 2024

table_data_2024_clean_sector <- table_data_2024_clean %>%
  group_by(sector) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_sector |>
  ggplot(aes(x = reorder(sector, total), y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = sector, y = total+0.25),  
            hjust = 0,                           
            color = "black",                        
            size = 8) +  
  geom_text(aes(label = total, y = total-0.1),  
            hjust = 1,                           
            color = "white",                        
            size = 10) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2024)",
       subtitle = "Sector",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.95))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2024/sector_2024.png", width = 12, height = 12, dpi = 300)

# When were the companies founded?

table_data_2022_clean_year <- table_data_2022_clean %>%
  group_by(founding_year) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2023_clean_year <- table_data_2023_clean %>%
  group_by(founding_year) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

table_data_2024_clean_year <- table_data_2024_clean %>%
  group_by(founding_year) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

# Grouped data by year

# 2022

table_data_2022_clean_year_group <- table_data_2022_clean %>%
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

# Order the bars by year

table_data_2022_clean_year_group$founding_year_group <- factor(table_data_2022_clean_year_group$founding_year_group, 
                                                               levels = c("1850-1950", "1951-1975", "1976-2000", "2001-2012", "2013-2024"))


table_data_2022_clean_year_group |>
  ggplot(aes(x = founding_year_group, y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = total, y = total-0.2),  
            hjust = 1,                           
            color = "white",                        
            size = 15) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2022)",
       subtitle = "Year of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.75, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2022/year_2022.png", width = 12, height = 12, dpi = 300)

# 2023

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

table_data_2023_clean_year_group$founding_year_group <- factor(table_data_2023_clean_year_group$founding_year_group, 
                                                               levels = c("1850-1950", "1951-1975", "1976-2000", "2001-2012", "2013-2024"))


table_data_2023_clean_year_group |>
  ggplot(aes(x = founding_year_group, y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = total, y = total-0.2),  
            hjust = 1,                           
            color = "white",                        
            size = 15) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2023)",
       subtitle = "Year of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.75, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2023/year_2023.png", width = 12, height = 12, dpi = 300)

# 2024

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

table_data_2024_clean_year_group$founding_year_group <- factor(table_data_2024_clean_year_group$founding_year_group, 
                                                               levels = c("1850-1950", "1951-1975", "1976-2000", "2001-2012", "2013-2024"))


table_data_2024_clean_year_group |>
  ggplot(aes(x = founding_year_group, y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = total, y = total-0.2),  
            hjust = 1,                           
            color = "white",                        
            size = 15) +  
  coord_flip() +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Africa’s Fastest Growing Companies (2024)",
       subtitle = "Year of Origin",
       caption = "Data Source: Financial Times") +
  scale_y_continuous(breaks = seq(0, 40, by = 10),
                     minor_breaks = seq(0, 40, by = 5),
                     expand = expansion(mult = c(0, 0.25))) +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.75, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_11_africa_fast_grow_wiki/images/2024/year_2024.png", width = 12, height = 12, dpi = 300)
