# FAO Africa Agriculture
# Tea, Coffee, and Cocoa

# Notes:
# 1) Production Quantity is in tonnes
# 2) Area harvested is in hectares(ha)

# A) Load the packages and libraries

library(tidyverse)
library(gt)
library(maps)
library(sf)
library(showtext)
library(here)
library(janitor)
library(geomtextpath)


# B) Load fonts

# font_add_google and showtext_auto()

# C) Load the data

data_file_1 <- here("sub_pro_19_agriculture_fao/data", "africa_production_2022.csv")
data_file_2 <- here("sub_pro_19_agriculture_fao/data", "world_africa_area_harvested_1961_2022.csv")
data_file_3 <- here("sub_pro_19_agriculture_fao/data", "world_africa_production_1961_2022.csv")

africa_production_2022 <- read_csv(data_file_1)
world_africa_area_harvest_61to22 <- read_csv(data_file_2)
world_africa_production_61to22 <- read_csv(data_file_3)

# D) Clean the data, select the required columns and arrange data

africa_production_2022_clean <- africa_production_2022 |>
  clean_names() |>
  select(area, item, year, value)
world_africa_area_harvest_61to22_clean <- world_africa_area_harvest_61to22 |>
  clean_names() |>
  select(area, item, year, value)
world_africa_production_61to22_clean <- world_africa_production_61to22 |>
  clean_names() |>
  select(area, item, year, value)

# E) Perform calculations to get values for plotting

# 1) Top 5 countries for production

africa_production_2022_clean_cocoa <- africa_production_2022_clean |>
  filter(item == "Cocoa beans") |>
  arrange(desc(value)) |>
  top_n(5)

africa_production_2022_clean_coffee <- africa_production_2022_clean |>
  filter(item == "Coffee, green") |>
  arrange(desc(value)) |>
  top_n(5)

africa_production_2022_clean_tea <- africa_production_2022_clean |>
  filter(item == "Tea leaves") |>
  arrange(desc(value)) |>
  top_n(5)

# 2) Get percentage area harvested between 1961 and 2022

# Split table to separate World and Africa
# Split by crop
# Merge Africa and World by Year
# Get percentage area harvested 

africa_area_harvest_61to22_cocoa <- world_africa_area_harvest_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Cocoa beans")

africa_area_harvest_61to22_coffee <- world_africa_area_harvest_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Coffee, green")

africa_area_harvest_61to22_tea <- world_africa_area_harvest_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Tea leaves")

world_area_harvest_61to22_cocoa <- world_africa_area_harvest_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Cocoa beans")

world_area_harvest_61to22_coffee <- world_africa_area_harvest_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Coffee, green")

world_area_harvest_61to22_tea <- world_africa_area_harvest_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Tea leaves")

merge_africa_world_area_harvest_61to22_cocoa <- merge(africa_area_harvest_61to22_cocoa, world_area_harvest_61to22_cocoa, by = "year")

merge_africa_world_area_harvest_61to22_coffee <- merge(africa_area_harvest_61to22_coffee, world_area_harvest_61to22_coffee, by = "year")

merge_africa_world_area_harvest_61to22_tea <- merge(africa_area_harvest_61to22_tea, world_area_harvest_61to22_tea, by = "year")

percent_area_harvest_61to22_cocoa <- merge_africa_world_area_harvest_61to22_cocoa |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_area_harvest_61to22_coffee <- merge_africa_world_area_harvest_61to22_coffee |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_area_harvest_61to22_tea <- merge_africa_world_area_harvest_61to22_tea |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_area_harvest_61to22 <- rbind(percent_area_harvest_61to22_cocoa,
                                     percent_area_harvest_61to22_coffee,
                                     percent_area_harvest_61to22_tea)


# 3) Get percentage production between 1961 and 2022

# Split table to separate World and Africa
# Split by crop
# Merge Africa and World by Year
# Get percentage production

africa_production_61to22_cocoa <- world_africa_production_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Cocoa beans")

africa_production_61to22_coffee <- world_africa_production_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Coffee, green")

africa_production_61to22_tea <- world_africa_production_61to22_clean |>
  filter(area == "Africa") |>
  filter(item == "Tea leaves")

world_production_61to22_cocoa <- world_africa_production_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Cocoa beans")

world_production_61to22_coffee <- world_africa_production_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Coffee, green")

world_production_61to22_tea <- world_africa_production_61to22_clean |>
  filter(area == "World") |>
  filter(item == "Tea leaves")

merge_africa_world_production_61to22_cocoa <- merge(africa_production_61to22_cocoa, world_production_61to22_cocoa, by = "year")

merge_africa_world_production_61to22_coffee <- merge(africa_production_61to22_coffee, world_production_61to22_coffee, by = "year")

merge_africa_world_production_61to22_tea <- merge(africa_production_61to22_tea, world_production_61to22_tea, by = "year")

percent_production_61to22_cocoa <- merge_africa_world_production_61to22_cocoa |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_production_61to22_coffee <- merge_africa_world_production_61to22_coffee |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_production_61to22_tea <- merge_africa_world_production_61to22_tea |>
  mutate(percent = round((value.x/value.y), 3)*100) |>
  select(item.x, year, percent)

percent_production_61to22 <- rbind(percent_production_61to22_cocoa,
                                   percent_production_61to22_coffee,
                                   percent_production_61to22_tea)

# Plots

# 1) Percent Area Harvest

last_points_area_harvest <- percent_area_harvest_61to22 %>%
  group_by(item.x) %>%
  filter(year == max(year))

p1 <- percent_area_harvest_61to22 |>
  ggplot(aes(year, percent, color = item.x)) +
  geom_line(size = 2) +
  geom_text(data = last_points_area_harvest, aes(label = item.x), vjust = -1, hjust = 1, size = 10) +
  labs(x = "Year",
       y = "Percentage(%)",
       title = "Tea, Coffee, or Hot Chocolate?",
       subtitle = "What percentage of the raw material's\narea harvested (in hectares) is on African soil?",
       caption = "Data Source: FAO") +
  theme_classic() +
  scale_color_manual(values = c("#a6611a", "#018571", "#ae017e")) +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_rect("bisque1"),
        legend.position = "")

ggsave("sub_pro_19_agriculture_fao/images/percent_area_harvest.png", width = 11.25, height = 11.25, dpi =600)

# 2) Percent Production

last_points_percent_production <- percent_production_61to22 %>%
  group_by(item.x) %>%
  filter(year == (min(year) + 20))

p2 <- percent_production_61to22 |>
  ggplot(aes(year, percent, color = item.x)) +
  geom_line(size = 2) +
  geom_text(data = last_points_percent_production, aes(label = item.x), vjust = -1, hjust = 0.2, size = 10) +
  labs(x = "Year",
       y = "Percentage(%)",
       title = "Tea, Coffee, or Hot Chocolate?",
       subtitle = "What percentage of the raw material's\nworldwide production is from Africa?",
       caption = "Data Source: FAO") +
  theme_classic() +
  scale_color_manual(values = c("#a6611a", "#018571", "#ae017e")) +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_rect("bisque1"),
        legend.position = "")

ggsave("sub_pro_19_agriculture_fao/images/percent_production.png", width = 11.25, height = 11.25, dpi =600)

# 3) Top 5 countries for production

# Cocoa

p3 <- africa_production_2022_clean_cocoa |>
  ggplot(aes(reorder(area, +value), value)) +
  geom_bar(stat = "identity", 
           fill = "#a6611a",
           just = 1, 
           width = 0.5) +
  geom_text(aes(
    x = area, 
    y = 0, 
    label = area),
    hjust = 0,
    vjust = 0,
    nudge_y = -0.3,
    color = '#000000',
    #fontface = 'bold',
    size = 10
            ) +
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Country",
     y = "Production (tonnes)",
     title = "Top 5 Cocoa Bean Producers in Africa (2022)",
     subtitle = "",
     caption = "Data Source: FAO") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_blank(),
        axis.text.x = element_text(size = 28, face = "bold", colour = "#000000"),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks = element_line(linewidth = 2),
        plot.title = element_text(family="Helvetica", face="bold", size = 32, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none")

ggsave("sub_pro_19_agriculture_fao/images/cocoa_production_1.png", width = 11.25, height = 11.25, dpi =600)

# p4 <- africa_production_2022_clean_cocoa |>
#   ggplot(aes(reorder(area, +value), value)) +
#   geom_segment(aes(x = area, xend = area, y = 0, yend = value), color = "#a6611a", size = 2.5) +
#   geom_point(size = 5, color = "#a6611a") +
#   coord_flip() + 
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   labs(x = "Country",
#        y = "Production (tonnes)",
#        title = "Who produced the most cocoa in 2022?",
#        subtitle = "",
#        caption = "Data Source: FAO") +
#   theme_classic() +
#   theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
#         axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
#         axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
#         axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
#         plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
#         plot.title.position = "plot",
#         plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
#         plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
#         plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.background = element_rect("bisque1"),
#         legend.position = "none")
# 
# ggsave("sub_pro_19_agriculture_fao/images/cocoa_production_2.png", width = 11.25, height = 11.25, dpi =600)

# Coffee

p5 <- africa_production_2022_clean_coffee |>
  ggplot(aes(reorder(area, +value), value)) +
  geom_bar(stat = "identity", 
           fill = "#018571",
           just = 1, 
           width = 0.5) +
  geom_text(aes(
    x = area, 
    y = 0, 
    label = area),
    hjust = 0,
    vjust = 0,
    nudge_y = -0.3,
    color = '#000000',
    #fontface = 'bold',
    size = 10
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "Country",
       y = "Production (tonnes)",
       title = "Top 5 Green Coffee Bean Producers in Africa (2022)",
       subtitle = "",
       caption = "Data Source: FAO") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_blank(),
        axis.text.x = element_text(size = 28, face = "bold", colour = "#000000", hjust = 0.75),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks = element_line(linewidth = 2),
        plot.title = element_text(family="Helvetica", face="bold", size = 32, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none")

ggsave("sub_pro_19_agriculture_fao/images/coffee_production_1.png", width = 11.25, height = 11.25, dpi =600)

# p6 <- africa_production_2022_clean_coffee |>
#   ggplot(aes(reorder(area, +value), value)) +
#   geom_segment(aes(x = area, xend = area, y = 0, yend = value), color = "#018571", size = 2.5) +
#   geom_point(size = 5, color = "#018571") +
#   coord_flip() + 
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   labs(x = "Country",
#        y = "Production (tonnes)",
#        title = "Who produced the most coffee in 2022?",
#        subtitle = "",
#        caption = "Data Source: FAO") +
#   theme_classic() +
#   theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
#         axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
#         axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
#         axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
#         axis.ticks.length = unit(0.25, "cm"),
#         axis.ticks = element_line(linewidth = 2),
#         plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
#         plot.title.position = "plot",
#         plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
#         plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
#         plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.background = element_rect("bisque1"),
#         legend.position = "none")
# 
# ggsave("sub_pro_19_agriculture_fao/images/coffee_production_2.png", width = 11.25, height = 11.25, dpi =600)

# Tea

p7 <- africa_production_2022_clean_tea |>
  ggplot(aes(reorder(area, +value), value)) +
  geom_bar(stat = "identity", 
           fill = "#ae017e",
           just = 1, 
           width = 0.5) +
  geom_text(aes(
    x = area, 
    y = 0, 
    label = area),
    hjust = 0,
    vjust = 0,
    nudge_y = -0.3,
    color = '#000000',
    #fontface = 'bold',
    size = 10
  ) +
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "Country",
       y = "Production (tonnes)",
       title = "Top 5 Tea Leaf Producers in Africa (2022)",
       subtitle = "",
       caption = "Data Source: FAO") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_blank(),
        axis.text.x = element_text(size = 28, face = "bold", colour = "#000000"),
        axis.text.y = element_blank(),
        axis.ticks.length = unit(0.25, "cm"),
        axis.ticks = element_line(linewidth = 2),
        plot.title = element_text(family="Helvetica", face="bold", size = 32, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none")

ggsave("sub_pro_19_agriculture_fao/images/tea_production_1.png", width = 11.25, height = 11.25, dpi =600)

# p8 <- africa_production_2022_clean_tea |>
#   ggplot(aes(reorder(area, +value), value)) +
#   geom_segment(aes(x = area, xend = area, y = 0, yend = value), color = "#ae017e", size = 2.5) +
#   geom_point(size = 5, color = "#ae017e") +
#   coord_flip() + 
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   labs(x = "Country",
#        y = "Production (tonnes)",
#        title = "Who produced the most tea in 2022?",
#        subtitle = "",
#        caption = "Data Source: FAO") +
#   theme_classic() +
#   theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
#         axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
#         axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
#         axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
#         plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
#         plot.title.position = "plot",
#         plot.subtitle = element_text(family="Helvetica", face="bold", size = 20, hjust = 0.5),
#         plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
#         plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.background = element_rect("bisque1"),
#         legend.position = "none")
# 
# ggsave("sub_pro_19_agriculture_fao/images/tea_production_2.png", width = 11.25, height = 11.25, dpi =600)

# Plot maps

#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

# Get data for African countries
africa <- ne_countries(continent = "Africa", returnclass = "sf")

# Create a data frame for the Equator line
equator <- data.frame(
  lon = c(-180, 180),
  lat = c(0, 0)
)

# Convert to sf object
equator_sf <- st_as_sf(equator, coords = c("lon", "lat"), crs = st_crs(africa))

# Cocoa

# Highlight specific countries
highlight_countries_cocoa <- c("Côte d'Ivoire", "Ghana", "Cameroon", "Nigeria", "Uganda")

# Create the plot
p9_cocoa <- ggplot(data = africa) +
  geom_sf(fill = "azure2", color = "black", linewidth = 1) +
  geom_sf(data = africa %>% filter(name %in% highlight_countries_cocoa), fill = "#a6611a", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", size = 2, linetype = "dashed") +
  annotate("text", x=-14, y=-2, label="Equator", size=8, color="black") +
  geom_text_repel(data = africa %>% filter(name %in% highlight_countries_cocoa), 
            aes(label = name, geometry = geometry),
            stat = "sf_coordinates", 
            size = 10, 
            nudge_y = 0,
            nudge_x = 0,
            check_overlap = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 34, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
  ) +
  labs(title = "Top Cocoa Bean Producers in Africa (2022)",
       subtitle = "Which countries produced the most cocoa beans in Africa?",
       caption = "Data Source: FAO")

ggsave("sub_pro_19_agriculture_fao/images/cocoa_map.png", width = 11.25, height = 11.25, dpi =600)

# Coffee

# Highlight specific countries
highlight_countries_coffee <- c("Ethiopia", "Uganda", "Central African Rep.", "Guinea", "Côte d'Ivoire")

# Create the plot
p10_coffee <- ggplot(data = africa) +
  geom_sf(fill = "azure2", color = "black", linewidth = 1) +
  geom_sf(data = africa %>% filter(name %in% highlight_countries_coffee), fill = "#018571", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", size = 2, linetype = "dashed") +
  annotate("text", x=-14, y=-2, label="Equator", size=8, color="black") +
  geom_text_repel(data = africa %>% filter(name %in% highlight_countries_coffee), 
            aes(label = name, geometry = geometry),
            stat = "sf_coordinates", 
            size = 10, 
            nudge_y = 0,
            nudge_x = 0,
            check_overlap = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 34, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
  ) +
  labs(title = "Top Green Coffee Bean Producers in Africa (2022)",
       subtitle = "Which countries produced the most green\ncoffee beans in Africa?",
       caption = "Data Source: FAO")

ggsave("sub_pro_19_agriculture_fao/images/coffee_map.png", width = 11.25, height = 11.25, dpi =600)

# Tea

# Highlight specific countries
highlight_countries_tea <- c("Kenya", "Uganda", "Malawi", "Rwanda", "Tanzania")

# Create the plot
p11_tea <- ggplot(data = africa) +
  geom_sf(fill = "azure2", color = "black", linewidth = 1) +
  geom_sf(data = africa %>% filter(name %in% highlight_countries_tea), fill = "#ae017e", linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", size = 2, linetype = "dashed") +
  annotate("text", x=-14, y=-2, label="Equator", size=8, color="black") +
  geom_text_repel(data = africa %>% filter(name %in% highlight_countries_tea), 
            aes(label = name, geometry = geometry),
            stat = "sf_coordinates",
            size = 10, 
            nudge_y = 0,
            nudge_x = 0,
            check_overlap = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 34, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
  ) +
  labs(title = "Top Tea Leaf Producers in Africa (2022)",
       subtitle = "Which countries produced the most tea leaves in Africa?",
       caption = "Data Source: FAO")

ggsave("sub_pro_19_agriculture_fao/images/tea_map.png", width = 11.25, height = 11.25, dpi =600)


