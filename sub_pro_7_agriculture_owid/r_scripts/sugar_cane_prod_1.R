# Sugar Cane Production

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

# # Fetch the data
# 
# sugar_cane_prod <- read.csv("https://ourworldindata.org/grapher/sugar-cane-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                       na.strings = "")
# 
# # Save the data
# write.csv(sugar_cane_prod, "sub_pro_7_agriculture_owid/datasets/sugar-cane-production-tonnes.csv",
#           row.names = FALSE)

# Read in the data
sugar_cane_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/sugar-cane-production-tonnes.csv")

# Clean the column headings

sugar_cane_prod_clean <- sugar_cane_prod %>%
  clean_names() 

# Change the column title names

sugar_cane_prod_clean <- sugar_cane_prod_clean %>%
  rename("region" = "entity",
         "sugar_cane_production_tonnes" = "sugar_cane_00000156_production_005510_tonnes") 

# Filter by region

sugar_cane_prod_clean_region <- sugar_cane_prod_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by FAO region

sugar_cane_prod_clean_region_fao <- sugar_cane_prod_clean_region %>%
  filter(grepl('(FAO)', region))

# Filter by non-FAO region

sugar_cane_prod_clean_region_non_fao <- sugar_cane_prod_clean_region %>%
  filter(!grepl('(FAO)', region))

# 3) Continental (Non-FAO) sugar_cane production

# a) Stacked area chart

sugar_cane_prod_clean_region_non_fao_continent <- sugar_cane_prod_clean_region_non_fao %>%
  filter(region %in% c("Africa", "Asia", "Europe", 
                       "North America", "South America", 
                       "Oceania"))

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Oceania", "Africa", "Europe", "North America", "South America", "Asia")

sugar_cane_prod_clean_region_non_fao_continent <- sugar_cane_prod_clean_region_non_fao_continent %>%
  mutate(region = factor(region, levels = desired_order)) %>%
  arrange(desc(region))

#  calculate cumulative positions for label placement

label_df_sugar_cane <- sugar_cane_prod_clean_region_non_fao_continent %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(x_label = max(year),
         y_top = cumsum(sugar_cane_production_tonnes),
         y_bottom = y_top - sugar_cane_production_tonnes,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(region, year, x_label, y_top, y_mid) 

# plot the stack area chart

sugar_cane_prod_clean_region_non_fao_continent %>% 
  ggplot(aes(year, sugar_cane_production_tonnes, fill = region, label = region, color = region)) +
  geom_area() +
  geom_text_repel(
    data = label_df_sugar_cane,
    aes(x = x_label, y = y_mid, label = region, color = region),
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
       y = "Sugar Cane Production\n(Millions of Tonnes)",
       title = "Only 5% of sugar cane production in 2020 was\nfrom Africa",
       subtitle = "",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 2250000000), labels  = 
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

# ggsave("sub_pro_7_agriculture_owid/images/continental/continent_sugar_cane_1.png", width = 12, height = 12, dpi = 72)


sugar_cane_prod_clean_region_non_fao_continent %>%
  filter(year == 2020) %>%
  mutate(percent = 100 * sugar_cane_production_tonnes/sum(sugar_cane_production_tonnes))


################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_sugar_cane_percent <- sugar_cane_prod_clean_region_non_fao_continent %>%
  group_by(year) %>%
  mutate(share = sugar_cane_production_tonnes / sum(sugar_cane_production_tonnes, na.rm = TRUE)) %>%
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


sugar_cane_prod_clean_region_non_fao_continent %>% 
  ggplot(aes(year, sugar_cane_production_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_sugar_cane_percent,
    aes(x = x_label, y = y_mid, label = region, color = region),
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
       y = "Share of Sugar Cane Production (%)",
       title = "Regional Share of Global Sugar Cane Production (1960–2020)",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
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
