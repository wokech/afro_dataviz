# Fish and Seafood Production

# Sum of seafood from wild catch and fish farming (aquaculture)

# 1) Load the required libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(tidyverse)
library(tidyr)
library(tidyr)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)
# install.packages("ggrepel")
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)

# 2) Load and clean the required data sets

fish_seafood_1 <- read_csv("sub_pro_13_agriculture_owid/processed_tables/fish-seafood-production.csv")

# Clean the column headings

fish_seafood_1_clean <- fish_seafood_1 %>%
  clean_names()

# Change the column title names

fish_seafood_1_clean <- fish_seafood_1_clean %>%
  rename("country" = "entity",
         "fish_and_seafood_production_tonnes" = "fish_and_seafood_00002960_production_005511_tonnes") 

fish_seafood_1_clean_region <- fish_seafood_1_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

fish_seafood_1_clean_region_fao <- fish_seafood_1_clean_region %>%
  filter(grepl('(FAO)', country))

fish_seafood_1_clean_region_non_fao <- fish_seafood_1_clean_region %>%
  filter(!grepl('(FAO)', country))

fish_seafood_1_clean_country <- fish_seafood_1_clean %>%
  filter(!is.na(code)) %>%
  select(c(1,3,4)) 

fish_seafood_1_clean_world <- fish_seafood_1_clean_country %>%
  filter(country %in% c("World"))

# Pivot the data for Flourish

fish_seafood_1_clean_world_wide <- fish_seafood_1_clean_world %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

write_csv(fish_seafood_1_clean_world_wide, "sub_pro_13_agriculture_owid/processed_tables/fish_seafood_1_clean_world_wide.csv")

# Draw a line plot for the worldwide fish production

fish_seafood_1_clean_world %>%
  ggplot(aes(year, fish_and_seafood_production_tonnes)) + 
  geom_line(color = "#5C4033", size = 1.5) +
  geom_point(color = "#5C4033", size = 3) +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Global Fish Production Has\nIncreased By Nearly 5x\nBetween 1960 And 2020",
       subtitle = "",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                  label_number(scale = 1e-6)) +
  theme(axis.title.x =element_text(size = 28, vjust = -2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"),
        plot.title = element_text(family="Helvetica", face="bold", size = 48, hjust = 0.3, colour = "#5C4033"),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "right") 

ggsave("sub_pro_13_agriculture_owid/images/global_fish.png", width = 12, height = 12, dpi = 72)

# Focus on WorldWide by Continent

fish_seafood_1_clean_region_non_fao_continent <- fish_seafood_1_clean_region_non_fao %>%
  filter(country %in% c("Africa", "Asia", "Europe", 
                        "North America", "South America", 
                        "Oceania"))

fish_seafood_1_clean_region_non_fao_continent %>% 
  ggplot(aes(year, fish_and_seafood_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Africa's Share of Global Fish Production\nHas Significantly Decreased",
       subtitle = "",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Pastel2") +
  scale_color_brewer(palette = "Pastel2") +
  theme(axis.title.x =element_text(size = 28, vjust = 2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 0, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0.5, colour = "#5C4033"),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 18, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("bisque1"),
        legend.position = "top")

ggsave("sub_pro_13_agriculture_owid/images/continent_fish.png", width = 12, height = 12, dpi = 72)

# Focus on Worldwide by Income

fish_seafood_1_clean_region_non_fao_income <- fish_seafood_1_clean_region_non_fao %>%
  filter(str_detect(country, "income"))

fish_seafood_1_clean_region_non_fao_income %>% 
  ggplot(aes(year, fish_and_seafood_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Majority of the Global Fish Production\nis from Upper-Middle-Income countries",
       subtitle = "",
       caption = "Data Source: Our World in Data\nBy @afro_dataviz") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title.x =element_text(size = 28, vjust = 2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 0, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0.5, colour = "#5C4033"),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 18, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 24),
        legend.background = element_rect("bisque1"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_13_agriculture_owid/images/income_fish.png", width = 12, height = 12, dpi = 72)
