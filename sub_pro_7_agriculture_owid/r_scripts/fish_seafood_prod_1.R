# Fish and Seafood Production

# Sum of seafood from wild catch and fish farming (aquaculture)

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

# Load and clean the required data sets

fish_seafood_1 <- read_csv("sub_pro_7_agriculture_owid/datasets/fish-seafood-production.csv")

# Clean the column headings

fish_seafood_1_clean <- fish_seafood_1 %>%
  clean_names()

# Change the column title names

fish_seafood_1_clean <- fish_seafood_1_clean %>%
  rename("country" = "entity",
         "fish_and_seafood_production_tonnes" = "fish_and_seafood_00002960_production_005511_tonnes") 

# Filter by region

fish_seafood_1_clean_region <- fish_seafood_1_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by FAO region

fish_seafood_1_clean_region_fao <- fish_seafood_1_clean_region %>%
  filter(grepl('(FAO)', country))

# Filter by non-FAO region

fish_seafood_1_clean_region_non_fao <- fish_seafood_1_clean_region %>%
  filter(!grepl('(FAO)', country))

# Filter by country

fish_seafood_1_clean_country <- fish_seafood_1_clean %>%
  filter(!is.na(code)) %>%
  select(c(1,3,4)) 

# Get total world production

fish_seafood_1_clean_world <- fish_seafood_1_clean_country %>%
  filter(country %in% c("World"))

# 3) Worldwide fish production

# a) Line Plot

fish_seafood_1_clean_world %>%
  ggplot(aes(year, fish_and_seafood_production_tonnes)) + 
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Global Fish Production Has Increased By\nApproximately 5x Between 1960 And 2020",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                  label_number(scale = 1e-6)) +
  theme(axis.title.x =element_text(size = 28, vjust = -2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
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
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "right") 

# ggsave("sub_pro_7_agriculture_owid/images/global_fish.png", width = 12, height = 12, dpi = 72)

# b) Pivot the worldwide data for Flourish

fish_seafood_1_clean_world_wide <- fish_seafood_1_clean_world %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

# write_csv(fish_seafood_1_clean_world_wide, "sub_pro_7_agriculture_owid/processed_tables/fish_seafood_1_clean_world_wide.csv")

# 4) Continental (Non-FAO) Fish Production

# a) Stacked area chart

fish_seafood_1_clean_region_non_fao_continent <- fish_seafood_1_clean_region_non_fao %>%
  filter(country %in% c("Africa", "Asia", "Europe", 
                        "North America", "South America", 
                        "Oceania"))

fish_seafood_1_clean_region_non_fao_continent %>% 
  ggplot(aes(year, fish_and_seafood_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Africa's Share of Global Fish & Seafood\nProduction Has Significantly Decreased",
       subtitle = "This is despite an increase in overall production (tonnes)",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
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
        legend.position = c(.35, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

# ggsave("sub_pro_7_agriculture_owid/images/continent_fish_2.png", width = 12, height = 12, dpi = 72)

# b) Circle Packing Chart 

# Obtain a sum of all the data from 1960 to 2020

fish_seafood_1_clean_region_non_fao_continent_sum <- fish_seafood_1_clean_region_non_fao_continent %>%
  group_by(country) %>%
  summarise(total = sum(fish_and_seafood_production_tonnes))

# Plot a packed circle chart and compare with Flourish

# Libraries
library(packcircles)
library(ggplot2)

# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(fish_seafood_1_clean_region_non_fao_continent_sum$total, sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(fish_seafood_1_clean_region_non_fao_continent_sum, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=50, label = country)) +
  scale_size_continuous(range = c(1,4)) +
  # General theme:
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

# ggsave("sub_pro_7_agriculture_owid/images/continent_fish_pack_circle.png", width = 12, height = 12, dpi = 72)

# c) Pivot the data for Flourish Line Chart

fish_seafood_1_clean_region_non_fao_continent_wide <- fish_seafood_1_clean_region_non_fao_continent %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

# write_csv(fish_seafood_1_clean_region_non_fao_continent_wide, "sub_pro_7_agriculture_owid/processed_tables/fish_seafood_1_clean_region_non_fao_continent_wide.csv")

# 5) Fish production by Income Level

fish_seafood_1_clean_region_non_fao_income <- fish_seafood_1_clean_region_non_fao %>%
  filter(str_detect(country, "income"))

# a) Stacked area chart

fish_seafood_1_clean_region_non_fao_income %>% 
  ggplot(aes(year, fish_and_seafood_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Majority of the Global Fish Production\nis from Upper-Middle-Income countries",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.title.x =element_text(size = 28, vjust = 2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 0, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 24, face = "bold"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0.5),
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

# ggsave("sub_pro_7_agriculture_owid/images/income_fish.png", width = 12, height = 12, dpi = 72)

# b) Pivot the data for Flourish Line Chart

fish_seafood_1_clean_region_non_fao_income_wide <- fish_seafood_1_clean_region_non_fao_income %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

# write_csv(fish_seafood_1_clean_region_non_fao_income_wide, "sub_pro_7_agriculture_owid/processed_tables/fish_seafood_1_clean_region_non_fao_income_wide.csv")

# c) Circle Packing Chart 

# Obtain a sum of all the data from 1960 to 2020

fish_seafood_1_clean_region_non_fao_income_sum <- fish_seafood_1_clean_region_non_fao_income %>%
  group_by(country) %>%
  summarise(total = sum(fish_and_seafood_production_tonnes))

# Plot a packed circle chart and compare with Flourish

# 6) Fish production in Africa

# Organize data

# Africa total combined with regions data
fish_seafood_1_clean_region_fao_africa <- fish_seafood_1_clean_region_fao %>%
  filter(str_detect(country, "frica"))

# Africa data alone
fish_seafood_1_clean_region_fao_africa_only <- fish_seafood_1_clean_region_fao_africa %>%
  filter(country %in% c("Africa (FAO)"))

# Africa regions alone
fish_seafood_1_clean_region_fao_africa_segment <- fish_seafood_1_clean_region_fao_africa %>%
  filter(country %in% c("Eastern Africa (FAO)", "Middle Africa (FAO)", 
                        "Northern Africa (FAO)", "Southern Africa (FAO)",
                        "Western Africa (FAO)")) |>
  mutate(country = str_remove(country, " \\(FAO\\)"))


# a) Line plot for Africa Only

fish_seafood_1_clean_region_fao_africa_only %>%
  ggplot(aes(year, fish_and_seafood_production_tonnes)) + 
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "Fish and Seafood Production in Africa increased\nfivefold between 1960 and 2020",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 15000000), labels  = 
                       label_number(scale = 1e-6)) +
  theme(axis.title.x =element_text(size = 28, vjust = -2, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, hjust = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15, hjust = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "right") 

# ggsave("sub_pro_7_agriculture_owid/images/africa_only_fish.png", width = 12, height = 12, dpi = 72)

# Pivot the data for Flourish

fish_seafood_1_clean_region_fao_africa_only_wide <- fish_seafood_1_clean_region_fao_africa_only %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

# write_csv(fish_seafood_1_clean_region_fao_africa_only_wide, "sub_pro_7_agriculture_owid/processed_tables/fish_seafood_1_clean_region_fao_africa_only_wide.csv")

# b) Stacked Area chart for Africa regions

fish_seafood_1_clean_region_fao_africa_segment %>% 
  ggplot(aes(year, fish_and_seafood_production_tonnes, fill = country, label = country, color = country)) +
  geom_area() +
  labs(x = "Year",
       y = "Fish and Seafood Production\n(Millions of Tonnes)",
       title = "North And West Africa Dominate Fish\nProduction On The Continent",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 15000000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
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
        legend.title = element_blank(),
        legend.text = element_text(size = 28),
        legend.background = element_rect("bisque1"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

# ggsave("sub_pro_7_agriculture_owid/images/africa_segment_fish.png", width = 12, height = 12, dpi = 72)

# Pivot the data for Flourish

fish_seafood_1_clean_region_fao_africa_segment_wide <- fish_seafood_1_clean_region_fao_africa_segment %>%
  pivot_wider(names_from = year, values_from = fish_and_seafood_production_tonnes)

# write_csv(fish_seafood_1_clean_region_fao_africa_segment_wide, "sub_pro_7_agriculture_owid/processed_tables/fish_seafood_1_clean_region_fao_africa_segment_wide.csv")

# c) Circle Packing Chart 

# Obtain a sum of all the data from 1960 to 2020

fish_seafood_1_clean_region_fao_africa_segment_sum <- fish_seafood_1_clean_region_fao_africa_segment %>%
  group_by(country) %>%
  summarise(total = sum(fish_and_seafood_production_tonnes))



################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_fish_seafood_percent <- fish_seafood_prod_clean_region_non_fao_continent %>%
  group_by(year) %>%
  mutate(share = fish_seafood_production_tonnes / sum(fish_seafood_production_tonnes, na.rm = TRUE)) %>%
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


fish_seafood_prod_clean_region_non_fao_continent %>% 
  ggplot(aes(year, fish_seafood_production_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_fish_seafood_percent,
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
       y = "Share of Fish/Seafood Production (%)",
       title = "Regional Share of Global Fish/Seafood Production (1960–2020)",
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
