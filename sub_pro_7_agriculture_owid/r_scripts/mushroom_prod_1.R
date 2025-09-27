# Mushroom Production

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

# Fetch the data (downloaded from FAO Stat and read in to R)

############# NORTH AMERICA = AMERICA minus SOUTH AMERICA ##########################

# Read in the data
mushroom_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/mushroom-truffle-production-fao-stat.csv")

# Clean the column headings

mushroom_prod_clean <- mushroom_prod %>%
  clean_names() 

# Change the column title names

mushroom_prod_clean <- mushroom_prod_clean %>%
  select(area, year, value)

# Filter by region

mushroom_prod_clean_region <- mushroom_prod_clean %>%
  filter(area %in% c("Africa", "Americas", "Asia", "Europe", "Oceania"))

# Filter by Continent (No South America) - Rename Americas to North America

mushroom_prod_clean_region <- mushroom_prod_clean_region %>%
  mutate(area = if_else(area == "Americas", "North America", area))

# 3) Continental mushroom production

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Oceania", "Africa", "Europe", "North America", "Asia")

mushroom_prod_clean_region_complete <- mushroom_prod_clean_region %>%
  mutate(area = factor(area, levels = desired_order)) %>%
  arrange(desc(area))

#  calculate cumulative positions for label placement

label_df_mushroom <- mushroom_prod_clean_region_complete %>%
  filter(year == max(year)) %>%
  mutate(area = factor(area, levels = rev(desired_order))) %>%
  arrange(area) %>%
  mutate(x_label = max(year),
         y_top = cumsum(value),
         y_bottom = y_top - value,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(area, year, x_label, y_top, y_mid) 

# plot the stack area chart

mushroom_prod_clean_region_complete %>% 
  ggplot(aes(year, value, fill = area, label = area, color = area)) +
  geom_area() +
  geom_text_repel(
    data = label_df_mushroom,
    aes(x = x_label, y = y_mid, label = area, color = area),
    hjust = 0,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    hjust = 0,
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Mushroom Production\n(Millions of Tonnes)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 50000000), labels  = 
                       label_number(scale = 1e-6, big.mark = ",")) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust  = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust  = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, , hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.subtitle.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none"
  ) 

#ggsave("sub_pro_7_agriculture_owid/images/continental/continent_mushroom_1.png", width = 12, height = 12, dpi = 72)


mushroom_prod_clean_region_complete %>%
  filter(year == 2020) %>%
  mutate(percent = 100 * value/sum(value))




################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_mushroom_percent <- mushroom_prod_clean_region_complete %>%
  group_by(year) %>%
  mutate(share = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(area = factor(area, levels = rev(desired_order))) %>%
  arrange(area) %>%
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) %>%
  select(area, year, x_label, y_top, y_mid)


mushroom_prod_clean_region_complete %>% 
  ggplot(aes(year, value, fill = area, color = area)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_mushroom_percent,
    aes(x = x_label, y = y_mid, label = area, color = area),
    hjust = 0,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Share of Mushroom Production (%)",
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

#ggsave("sub_pro_7_agriculture_owid/images/continental_stack_perc/continent_mushroom_1.png", width = 12, height = 12, dpi = 72)



################################################################################
# AFRICA ONLY CHARTS
################################################################################

# 4) Mushroom production in Africa

# Organize data

# Africa total combined with regions data
mushroom_prod_clean_africa <- mushroom_prod_clean %>%
  filter(str_detect(area, "frica"))

# Africa regions alone
mushroom_prod_clean_region_fao_africa_segment <- mushroom_prod_clean_africa %>%
  filter(area %in% c("Eastern Africa", "Middle Africa", "Northern Africa", 
                       "Southern Africa", "Western Africa"))

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa")

# order of the colored regions

mushroom_prod_clean_region_fao_africa_segment <- mushroom_prod_clean_region_fao_africa_segment %>%
  mutate(area = factor(area, levels = desired_order)) %>%
  arrange(desc(area))

label_df_mushroom_africa <- mushroom_prod_clean_region_fao_africa_segment %>%
  filter(year == max(year)) %>%
  mutate(area = factor(area, levels = rev(desired_order))) %>%
  arrange(area) %>%
  mutate(x_label = max(year),
         y_top = cumsum(value),
         y_bottom = y_top - value,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(area, year, x_label, y_top, y_mid) 

# b) Stacked Area chart for Africa regions

mushroom_prod_clean_region_fao_africa_segment %>% 
  ggplot(aes(year, value, fill = area, label = area, color = area)) +
  geom_area() +
  geom_text_repel(
    data = label_df_mushroom_africa,
    aes(x = x_label, y = y_mid, label = area, color = area),
    hjust = 0.1,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    hjust = 0,
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Mushroom Production\n(Millions of Tonnes)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 50000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15, hjust = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.subtitle.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none")

ggsave("sub_pro_7_agriculture_owid/images/continental_africa_only/continent_mushroom_1.png", width = 12, height = 12, dpi = 300)

################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_mushroom_percent_africa <- mushroom_prod_clean_region_fao_africa_segment %>%
  group_by(year) %>%
  mutate(share = value / sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(area = factor(area, levels = rev(desired_order))) %>%
  arrange(area) %>%
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) %>%
  select(area, year, x_label, y_top, y_mid)


mushroom_prod_clean_region_fao_africa_segment %>% 
  ggplot(aes(year, value, fill = area, color = area)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_mushroom_percent_africa,
    aes(x = x_label, y = y_mid, label = area, color = area),
    hjust = 0.1,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Share of Mushroom Production (%)",
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

ggsave("sub_pro_7_agriculture_owid/images/continental_africa_only_stack_perc/continent_mushroom_1.png", width = 12, height = 12, dpi = 300)
