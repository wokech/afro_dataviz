# Meat Production

# 1) Load the Required Libraries

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
#library(bbplot)
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

# 2) Data Cleaning and Organization

# Load and clean the required data sets

meat <- read_csv("sub_pro_13_agriculture_owid/processed_tables/global-meat-production.csv")

# Clean the column headings

meat_clean <- meat %>%
  clean_names()

# Change the column title names

meat_clean <- meat_clean %>%
  rename("country" = "entity",
         "meat_production_tonnes" = "meat_total_00001765_production_005510_tonnes") 

# Filter by region

meat_clean_region <- meat_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by FAO region

meat_clean_region_fao <- meat_clean_region %>%
  filter(grepl('(FAO)', country))

# Filter by non-FAO region

meat_clean_region_non_fao <- meat_clean_region %>%
  filter(!grepl('(FAO)', country))

# 3) Continental (Non-FAO) Fish Production

# a) Stacked area chart

meat_clean_region_non_fao_continent <- meat_clean_region_non_fao %>%
  filter(country %in% c("Africa", "Asia", "Europe", 
                        "North America", "South America", 
                        "Oceania"))

# A) 1080 by 1080 

meat_clean_region_non_fao_continent %>% 
  ggplot(aes(year, meat_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Meat Production\n(Millions of Tonnes)",
       title = "Africa's Share of Global Meat Production\nHas Significantly Decreased",
       subtitle = "",
       caption = "Data Source: Our World in Data    |     By @afro_dataviz") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 360000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust  = 0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 24, , hjust = 0.5, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("bisque1"),
        legend.position = c(.35, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_13_agriculture_owid/images/continent_meat_1.png", width = 12, height = 12, dpi = 72)

# B) 1080 by 1920 

meat_clean_region_non_fao_continent %>% 
  ggplot(aes(year, meat_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Meat Production\n(Millions of Tonnes)",
       title = "Africa's Share of Global Meat\nProduction Has Significantly\nDecreased",
       subtitle = "",
       caption = "Data Source: Our World in Data | By @afro_dataviz") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 360000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_brewer(palette = "Set3") +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 32, colour = "#000000", hjust  = 0.5),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 24, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("bisque1"),
        legend.position = c(.5, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_13_agriculture_owid/images/continent_meat_2.png", width = 9, height = 16, dpi = 72)

