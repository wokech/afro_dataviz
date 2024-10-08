# Olympics and African Countries

# https://worldpopulationreview.com/country-rankings/olympic-medals-by-country

# (A) Load the required libraries

library(readxl)
library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Load the required data and include the required sheet

olympic_data <- read_excel("sub_pro_9_olympics/datasets/olympic_medal_country.xlsx", sheet = "African countries")

# (C) Clean the data, fix columns and county labels

olympic_data_clean <- olympic_data %>%
  clean_names() 

str(olympic_data_clean)

# Plot 1 - Total medals earned

# Plot size = 1080 by 1080

ggplot(olympic_data_clean, aes(reorder(country, +total_medals), total_medals, fill = country)) +
  geom_bar(stat = "identity") + coord_flip(ylim = c(0, 120)) +
  geom_text(aes(label = paste(total_medals), hjust = -0.1), size = 8) +
  gghighlight(max(total_medals) > 50) + 
  scale_fill_brewer(palette="Set2") +
  labs(x = "Country",
       y = "Total number of medals",
       title = "Three African countries have won\n60% of the continent's Olympic medals",
       subtitle = "",
       caption = "Data Source: World Population Review/IOC(2022)") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 130, 20)) +
  theme(axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", color = "black"),
        axis.text.y = element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 20, hjust = 0, vjust = 1),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_olympics/images/africa_olympic_medals_square.png", width = 12, height = 12, dpi = 72)

# Plot 2 - Total medals earned per million people

# Plot 1080 by 1080

ggplot(olympic_data_clean, aes(reorder(country, +medals_per_1m), medals_per_1m, fill = country)) +
  geom_bar(stat = "identity") + coord_flip(ylim = c(0, 2.2)) +
  geom_text(aes(label = paste(round(medals_per_1m, 2)), hjust = -0.1), size = 8) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  gghighlight(max(medals_per_1m) > 1) + 
  scale_fill_brewer(palette="Set2") +
  labs(x = "Country",
       y = "Medals per 1 million people (2023)",
       title = "Only four African countries have at least\n1 olympic medal per 1 million people",
       subtitle = "",
       caption = "Data Source: World Population Review/IOC(2022)") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 2.5, 0.5)) +
  theme(axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", color = "black"),
        axis.text.y = element_text(size = 24, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 20, hjust = 0, vjust = 1),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_9_olympics/images/africa_olympic_medals_per_person_square.png", width = 12, height = 12, dpi = 72)
