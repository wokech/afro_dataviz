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
library(jsonlite)

# 2) Data Cleaning and Organization

# Fetch the data

# wild_fish <- read.csv("https://ourworldindata.org/grapher/capture-fishery-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                        na.strings = "")
# 
# # Save the data
# write.csv(wild_fish, "sub_pro_7_agriculture_owid/datasets/capture-fishery-production.csv",
#           row.names = FALSE)

# Read in the data

wild_fish <- read.csv("sub_pro_7_agriculture_owid/datasets/capture-fishery-production.csv")

# Clean the column headings

wild_fish_clean <- wild_fish %>%
  clean_names()

# Change the column title names

wild_fish_clean <- wild_fish_clean %>%
  rename("region" = "entity",
         "wild_fish_tonnes" = "er_fsh_capt_mt") 

# Filter by region

wild_fish_clean_region <- wild_fish_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by WB region

wild_fish_clean_region_wb <- wild_fish_clean_region %>%
  filter(grepl('(WB)', region))

# Remove "(WB)" from region

wild_fish_clean_region_wb <- wild_fish_clean_region_wb %>%
  mutate(region = str_remove_all(region, " \\(WB\\)"))

# Filter by not WB region

wild_fish_clean_region_non_wb <- wild_fish_clean_region %>%
  filter(!grepl('(WB)', region))

# 3) Continental (WB) wild capture fish production

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#6A5ACD", "#56B4E9"
)

# reorder the stacks

desired_order <- c("North America", "Sub-Saharan Africa", "Middle East and North Africa", "Europe and Central Asia", "Latin America and Caribbean", "South Asia", "East Asia and Pacific")

wild_fish_clean_region_wb <- wild_fish_clean_region_wb %>%
  mutate(region = factor(region, levels = desired_order)) %>%
  arrange(desc(region))

#  calculate cumulative positions for label placement

label_df_wf <- wild_fish_clean_region_wb %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(x_label = max(year),
         y_top = cumsum(wild_fish_tonnes),
         y_bottom = y_top - wild_fish_tonnes,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(region, year, x_label, y_top, y_mid) 

# a) Stacked area chart

wild_fish_clean_region_wb %>% 
  ggplot(aes(year, wild_fish_tonnes, fill = region, label = region, color = region), fill = "bisque1") +
  geom_area() +
  geom_text_repel(
    data = label_df_wf,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0,
    fontface = "bold",
    size = 7.5,
    inherit.aes = FALSE,
    direction = "y",
    hjust = 0,
    nudge_x = 40,
    segment.curvature = 0.01,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Year",
       y = "Wild Fish Catch Production\n(Millions of Tonnes)",
       title = "In 2020, Sub-Saharan Africa contributed less\nthan 10% of global wild fish catch production",
       subtitle = "",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 110000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
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
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none"
        )

ggsave("sub_pro_7_agriculture_owid/images/continental/continent_wild_fish_1.png", width = 12, height = 12, dpi = 72)

