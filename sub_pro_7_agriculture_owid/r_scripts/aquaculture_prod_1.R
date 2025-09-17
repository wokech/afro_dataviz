# Fish and Seafood Production (Aquaculture)

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

# aquaculture <- read.csv("https://ourworldindata.org/grapher/aquaculture-farmed-fish-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                            na.strings = "")
# 
# # Save the data
# write.csv(aquaculture, "sub_pro_7_agriculture_owid/datasets/aquaculture-farmed-fish-production.csv",
#           row.names = FALSE)

# Read in the data

aquaculture <- read.csv("sub_pro_7_agriculture_owid/datasets/aquaculture-farmed-fish-production.csv")

# Clean the column headings

aquaculture_clean <- aquaculture %>%
  clean_names()

# Change the column title names

aquaculture_clean <- aquaculture_clean %>%
  rename("region" = "entity",
         "aquaculture_tonnes" = "er_fsh_aqua_mt") 

# Filter by region

aquaculture_clean_region <- aquaculture_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by WB region

aquaculture_clean_wb <- aquaculture_clean_region %>%
  filter(grepl('(WB)', region))

# Remove "(WB)" from region

aquaculture_clean_region_wb <- aquaculture_clean_wb %>%
  mutate(region = str_remove_all(region, " \\(WB\\)"))

# Filter by not WB region

aquaculture_clean_region_non_wb <- aquaculture_clean_region %>%
  filter(!grepl('(WB)', region))

# 3) Continental (WB) wild capture fish production

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#6A5ACD", "#56B4E9"
)

# reorder the stacks

desired_order <- c("North America", "Sub-Saharan Africa", "Middle East and North Africa", "Europe and Central Asia", "Latin America and Caribbean", "South Asia", "East Asia and Pacific")

aquaculture_clean_region_wb <- aquaculture_clean_region_wb %>%
  mutate(region = factor(region, levels = desired_order)) %>%
  arrange(desc(region))
  
#  calculate cumulative positions for label placement

label_df_aqua <- aquaculture_clean_region_wb %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(x_label = max(year),
         y_top = cumsum(aquaculture_tonnes),
         y_bottom = y_top - aquaculture_tonnes,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(region, year, x_label, y_top, y_mid) 

# a) Stacked area chart

aquaculture_clean_region_wb %>% 
  ggplot(aes(year, aquaculture_tonnes, fill = region, label = region, color = region), fill = "bisque1") +
  geom_area() +
  geom_text_repel(
    data = label_df_aqua,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0,
    fontface = "bold",
    size = 7.5,
    inherit.aes = FALSE,
    direction = "y",
    hjust = 0,
    nudge_x = -60,
    segment.curvature = 0.01,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  coord_cartesian(clip = "off") +
  labs(x = "Year",
       y = "Aquaculture Production\n(Millions of Tonnes)",
       title = "Sub-Saharan Africa has a minor role\nin global aquaculture production",
       subtitle = "",
       caption = "Data Source: Our World in Data  | FAO | World Bank") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 150000000), labels  = 
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

# ggsave("sub_pro_7_agriculture_owid/images/continental/continent_aquaculture_1.png", width = 12, height = 12, dpi = 72)

################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_aquaculture_percent <- aquaculture_clean_region_wb %>%
  group_by(year) %>%
  mutate(share = aquaculture_tonnes / sum(aquaculture_tonnes, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) %>%
  select(region, year, x_label, y_top, y_mid)


aquaculture_clean_region_wb %>% 
  ggplot(aes(year, aquaculture_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_aquaculture_percent,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0.2,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    nudge_x = 45,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Share of Bean Production (%)",
       title = "",
       caption = "") +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
    axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
    axis.text.x = element_text(size = 28, face = "bold", color = "black"),
    axis.text.y = element_text(size = 28, face = "bold", color = "black"),
    plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust = 0),
    plot.caption = element_text(family = "Helvetica", size = 24, hjust = 0, vjust = 1),
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    legend.position = "none"
  )

ggsave("sub_pro_7_agriculture_owid/images/continental_stack_perc/continent_aquaculture_1.png", width = 12, height = 12, dpi = 72)
