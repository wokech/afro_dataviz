# Fish and Seafood Production (Wildfish Catch)

# 1) Load the Required Libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(janitor)
library(scales)
library(devtools)
library(treemapify)
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)

# 2) Data Cleaning and Organization

# Fetch the data

# wild_fish <- read.csv("https://ourworldindata.org/grapher/capture-fishery-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                        na.strings = "")
# 
# # Save the data
# 
# write.csv(wild_fish, "sub_pro_7_agriculture_owid/datasets/capture-fishery-production.csv",
#           row.names = FALSE)

# Read in the data

wild_fish <- read.csv("sub_pro_7_agriculture_owid/datasets/capture-fishery-production.csv")

# Clean the column headings

wild_fish_clean <- wild_fish %>%
  clean_names()

# Change the column title names

wild_fish_clean <- wild_fish_clean %>%
  rename("country" = "entity",
         "wild_fish_tonnes" = "er_fsh_capt_mt") 

# Filter by region

wild_fish_clean_region <- wild_fish_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by WB region

wild_fish_clean_region_wb <- wild_fish_clean_region %>%
  filter(grepl('(WB)', country))

# Remove "(WB)" from region

wild_fish_clean_region_wb <- wild_fish_clean_region_wb %>%
  mutate(country = str_remove_all(country, "[(WB)]"))

# Filter by not WB region

wild_fish_clean_region_non_wb <- wild_fish_clean_region %>%
  filter(!grepl('(WB)', country))

# 3) Continental (WB) wild capture fish production

# a) Stacked area chart

wild_fish_clean_region_wb %>% 
  ggplot(aes(year, wild_fish_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Wild Fish Catch Production\n(Millions of Tonnes)",
       title = "",
       subtitle = "",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 125000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 0, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold"),
        axis.text.y = element_text(size = 28, face = "bold"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust  = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.subtitle.position = 'plot',
        plot.caption.position = 'plot',
        legend.title = element_blank(),
        legend.text = element_text(size = 24),
        legend.background = element_rect("bisque1"),
        legend.position = c(.5, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ggsave("sub_pro_7_agriculture_owid/images/continent_wild_fish_1.png", width = 12, height = 12, dpi = 72)

