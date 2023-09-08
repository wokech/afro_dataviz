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

# To export the images
# camcorder::gg_record()

library(camcorder)

gg_record(
  dir = 'sub_pro_9_olympics/images',
  width = 12,
  height = 12 * 9 / 16,
  dpi = 300,
  bg = 'white'
)

# (B) Load the required data and include the required sheet

olympic_data <- read_excel("sub_pro_9_olympics/processed_tables/olympic_medal_country.xlsx", sheet = "African countries")

# (C) Clean the data, fix columns and county labels

olympic_data_clean <- olympic_data %>%
  clean_names() 

str(olympic_data_clean)

# Plot 1 - Total medals earned

ggplot(olympic_data_clean, aes(reorder(country, +total_medals), total_medals, fill = country)) +
  geom_bar(stat = "identity") + coord_flip() +
  geom_text(aes(label = paste(total_medals), hjust = -0.1), size = 5) +
  gghighlight(max(total_medals) > 50) + 
  scale_fill_brewer(palette="OrRd") +
  labs(x = "Country",
       y = "Total number of medals",
       title = "Three African countries have won 60% of the continent's Olympic medals",
       subtitle = "Kenya, South Africa, and Ethiopia are the most dominant African countries at the Olympics",
       caption = "Data Source: World Population Review\nBy @willyokech") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 130, 10)) +
  theme(axis.title.x =element_text(size = 16),
        axis.title.y =element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 


# Plot 2 - Total medals earned per million people

ggplot(olympic_data_clean, aes(reorder(country, +medals_per_1m), medals_per_1m, fill = country)) +
  geom_bar(stat = "identity") + coord_flip() +
  geom_text(aes(label = paste(round(medals_per_1m, 2)), hjust = -0.1), size = 5) +
  gghighlight(max(medals_per_1m) > 1) + 
  scale_fill_brewer(palette="OrRd") +
  labs(x = "Country",
       y = "Total number of medals per 1 million people",
       title = "Only four African countries have at least 1 medal per million people",
       subtitle = "Kenya, Namibia, South Africa, and Tunisia have the most medals per 1 million people",
       caption = "Data Source: World Population Review\nBy @willyokech") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 2.5, 0.2)) +
  theme(axis.title.x =element_text(size = 16),
        axis.title.y =element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 
