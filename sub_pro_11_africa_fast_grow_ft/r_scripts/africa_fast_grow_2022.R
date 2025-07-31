# Africa’s Fastest Growing Companies 2022

# A) Load the required packages and libraries

#install.packages('rvest')
library(rvest)
library(tidyverse)
library(janitor)
library(treemapify)
library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(patchwork)
library(ggrepel)
library(scales)

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

# C) Data cleaning 

# table_data_2022_clean <- table_data_2022 |>
#   clean_names() |>
#   select(c(rank:founding_year)) |>
#   filter(row_number() <= n()-1)

# Save the data

# write.csv(table_data_2022_clean, "sub_pro_11_africa_fast_grow_ft/datasets/fast_grow_2022")

# Get the data

table_data_2022_clean <- read_csv("sub_pro_11_africa_fast_grow_ft/datasets/fast_grow_2022")

# D) EDA

# Where do most companies come from?

# 2022

table_data_2022_clean_country <- table_data_2022_clean |>
  group_by(country) |>
  summarise(total = n()) |>
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
                     expand = expansion(mult = c(0, 0.3))) +
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

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/origin_2022.png", width = 12, height = 12, dpi = 300)

# What sector do the companies work in?

# 2022

table_data_2022_clean_sector <- table_data_2022_clean |>
  group_by(sector) |>
  mutate(sector = recode(sector, "Precious metals" = "Precious Metals")) |>
  summarise(total = n()) |>
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

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/sector_2022.png", width = 12, height = 12, dpi = 300)

# When were the companies founded?

table_data_2022_clean_year <- table_data_2022_clean |>
  group_by(founding_year) |>
  summarise(total = n()) |>
  arrange(desc(total))

# Grouped data by year

# 2022

table_data_2022_clean_year_group <- table_data_2022_clean |>
  mutate(founding_year_group = case_when(
    founding_year >= 1850 & founding_year <= 1950 ~ "1850-1950",
    founding_year >= 1951 & founding_year <= 1975 ~ "1951-1975",
    founding_year >= 1976 & founding_year <= 2000 ~ "1976-2000",
    founding_year >= 2001 & founding_year <= 2012 ~ "2001-2012",
    founding_year >= 2013 & founding_year <= 2022 ~ "2013-2022", 
  )) |>
  group_by(founding_year_group) |>
  summarise(total = n()) |>
  arrange(desc(total))

# Order the bars by year

table_data_2022_clean_year_group$founding_year_group <- factor(table_data_2022_clean_year_group$founding_year_group, 
                                                               levels = c("1850-1950", "1951-1975", "1976-2000", "2001-2012", "2013-2022"))


table_data_2022_clean_year_group |>
  ggplot(aes(x = founding_year_group, y = total)) +
  geom_col(fill = "purple") + 
  geom_text(aes(label = total, y = total-0.2),  
            hjust = 1,                           
            color = "white",                        
            size = 13) +  
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
        axis.text.x = element_text(size = 35, color = "black"),
        axis.text.y = element_text(size = 35, color = "black"),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0, vjust=0.5),
        plot.subtitle = element_text(family="Helvetica", size = 30),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = -0.35, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

# ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/year_2022.png", width = 12, height = 12, dpi = 300)


# Treemap of top 5 countries, year, and sectors

# Countries

table_data_2022_clean_country_treemap <- table_data_2022_clean |>
  group_by(country) |>
  summarise(total = n()) |>
  arrange(desc(total)) |>
  mutate(group = if_else(row_number() <= 5,
                         country, "Other Countries")) |>
  group_by(group) |>
  summarise(total = sum(total)) |>
  mutate(percent_total = round((total/sum(total))*100, 1)) |>
  arrange(desc(percent_total))

# Visualize the data

ggplot(table_data_2022_clean_country_treemap, 
       aes(area = percent_total, fill = percent_total, 
           label = paste0(group, "\n",
                          percent_total, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="bisque1"),
        plot.background = element_rect(fill="bisque1"),
        legend.background = element_rect(fill="bisque1")) +
  scale_fill_gradient(low = "#ECD2FC", high = "#C16FF5")


ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/origin_2022_treemap.png", width = 12, height = 8, dpi = 300)


# Sectors

table_data_2022_clean_sector_treemap <- table_data_2022_clean |>
  mutate(sector = recode(sector, "Real estate" = "Real Estate")) |>
  mutate(sector = recode(sector, "Precious metals" = "Precious Metals")) |>
  mutate(sector = recode(sector, "Agricultural Commodities" = "Agricultural\nCommodities")) |>
  group_by(sector) |>
  summarise(total = n()) |>
  arrange(desc(total)) |>
  mutate(group = if_else(row_number() <= 5,
                         sector, "Other Sectors")) |>
  group_by(group) |>
  summarise(total = sum(total)) |>
  mutate(percent_total = round((total/sum(total))*100, 1)) |>
  arrange(desc(percent_total))

# Visualize the data

ggplot(table_data_2022_clean_sector_treemap, 
       aes(area = percent_total, fill = percent_total, 
           label = paste0(group, "\n",
                          percent_total, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="bisque1"),
        plot.background = element_rect(fill="bisque1"),
        legend.background = element_rect(fill="bisque1")) +
  scale_fill_gradient(low = "#ECD2FC", high = "#C16FF5")

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/sector_2022_treemap.png", width = 12, height = 8, dpi = 300)

# Founding Year

table_data_2022_clean_year_group <- table_data_2022_clean |>
  mutate(founding_year_group = case_when(
    founding_year >= 1850 & founding_year <= 1950 ~ "1850-1950",
    founding_year >= 1951 & founding_year <= 1975 ~ "1951-1975",
    founding_year >= 1976 & founding_year <= 2000 ~ "1976-2000",
    founding_year >= 2001 & founding_year <= 2012 ~ "2001-2012",
    founding_year >= 2013 & founding_year <= 2022 ~ "2013-2022", 
  )) |>
  group_by(founding_year_group) |>
  summarise(total = n()) |>
  arrange(desc(total))

table_data_2022_clean_year_group$founding_year_group <- factor(table_data_2022_clean_year_group$founding_year_group, 
                                                               levels = c("1850-1950", "1951-1975", "1976-2000", "2001-2012", "2013-2022"))

table_data_2022_clean_year_group_treemap <- table_data_2022_clean_year_group |>
  group_by(founding_year_group) |>
  summarise(total = sum(total)) |>
  mutate(percent_total = round((total/sum(total))*100, 1)) |>
  arrange(desc(percent_total))

# Visualize the data

ggplot(table_data_2022_clean_year_group_treemap, 
       aes(area = percent_total, fill = percent_total, 
           label = paste0(founding_year_group, "\n",
                          "(", percent_total, "%", ")"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "off",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="bisque1"),
        plot.background = element_rect(fill="bisque1"),
        legend.background = element_rect(fill="bisque1")) +
  scale_fill_gradient(low = "#ECD2FC", high = "#C16FF5")

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/year_2022_treemap.png", width = 12, height = 8, dpi = 300)

# Matching treemap and map

# Map

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world |>
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# List top 5 counties
top_5 <- africa |>
  filter(admin %in% c("Nigeria", "South Africa", "Egypt", "Ghana", "Kenya")) |>
  pull(admin)

# Add group column to full tidy dataset
africa_top_5 <- africa |>
  mutate(group = if_else(admin %in% top_5, admin, "Other Countries"))

# Color Map (2022)

color_map_2022 <- c(
  "Nigeria" = "#FFB5A7",
  "South Africa" = "#B5EAD7",
  "Egypt" = "#9EC1CF",
  "Ghana" = "#BDA167",
  "Kenya" = "#CC79A7",
  "Other Countries" = "#BEBEBE"  # For grouped others
)

# Plot of Africa
ggplot(data = africa_top_5) +
  geom_sf(aes(fill = group), linewidth = 0.5) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"),
        legend.position = "none") +
  scale_fill_manual(values = color_map_2022)

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/origin_2022_map_match.png", width = 12, height = 8, dpi = 300)

# Treemap

# Countries

table_data_2022_clean_country_treemap <- table_data_2022_clean |>
  group_by(country) |>
  summarise(total = n()) |>
  arrange(desc(total)) |>
  mutate(group = if_else(row_number() <= 5,
                         country, "Other Countries")) |>
  group_by(group) |>
  summarise(total = sum(total)) |>
  mutate(percent_total = round((total/sum(total))*100, 1)) |>
  arrange(desc(percent_total))

# Visualize the data

color_map_2022 <- c(
  "Nigeria" = "#FFB5A7",
  "South Africa" = "#B5EAD7",
  "Egypt" = "#9EC1CF",
  "Ghana" = "#BDA167",
  "Kenya" = "#CC79A7",
  "Other Countries" = "#BEBEBE"  # For grouped others
)

ggplot(table_data_2022_clean_country_treemap, 
       aes(area = percent_total, fill = group, 
           label = paste0(group, "\n",
                          percent_total, "%"))) +
  geom_treemap() +
  labs(title = "",
       subtitle = "",
       fill = "",
       caption = "") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 40) + 
  theme(legend.position = "none",
        plot.title = element_text(size=24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 10),
        plot.caption = element_text(size =12),
        panel.background = element_rect(fill="bisque1"),
        plot.background = element_rect(fill="bisque1"),
        legend.background = element_rect(fill="bisque1")) +
  scale_fill_manual(values = color_map_2022)

ggsave("sub_pro_11_africa_fast_grow_ft/images/2022/origin_2022_treemap_match.png", width = 12, height = 8, dpi = 300)