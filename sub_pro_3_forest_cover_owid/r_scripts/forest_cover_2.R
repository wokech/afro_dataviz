# Forests (OWID)

# Forest cover as a share of total land area within the country or region.

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(ggrepel)
library(scales)
library(gghighlight)

# Load data

# Forest as a share of land area
forest_share_land_area <- read_csv("sub_pro_3_forest_cover_owid/datasets/forest-area-as-share-of-land-area.csv")

# Clean the data

forest_share_land_area <- forest_share_land_area %>%
  clean_names()

# Set up datasets for EDA

############### Forests as a share of land area

# Forest as a share of land area (By Continent)
forest_share_land_area_continent <- forest_share_land_area %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Forest as a share of land area (By Country - Worldwide)
forest_share_land_area_countries <- forest_share_land_area %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Forest as a share of land area (By African Region)
forest_share_land_area_africa_regions <- forest_share_land_area %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Middle Africa",
                       "Southern Africa", "Western Africa")) %>%
  mutate(entity = ifelse(entity == 'Middle Africa', 'Central Africa', entity))

# Forest as a share of land area (By African Countries)

# This is list of African Countries but IVORY COAST IS MISSING

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
              "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
              "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
              "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
              "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
              "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
              "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
              "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
              "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
              "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
              "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
              "Uganda", "Zambia", "Zimbabwe")

# Filter the dataset to include only African countries

forest_share_land_area_africa_countries <- forest_share_land_area %>%
  filter(entity %in% african_countries)

# To combine the datasets with mapping dataset - change some of the country names to match

# Change to standard names used in rnaturalearth for maps

forest_share_land_area_africa_countries_rnaturalearth <- forest_share_land_area_africa_countries %>%
  mutate(entity = case_when(
    entity == "Cape Verde"  ~ "Cabo Verde",
    entity == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    entity == "Eswatini"  ~ "eSwatini",
    entity == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    entity == "Tanzania"  ~ "United Republic of Tanzania",
    entity == "Congo"  ~ "Republic of the Congo",
    TRUE ~ entity  # Retain original name if none of the conditions are met
  ))

# B) EDA and Basic Plots

# Forests as a share of land area (within country)

# Line chart for forest as a share of land area (By Continent)

last_points_forest_1 <- forest_share_land_area_continent %>%
  group_by(entity) %>%
  slice_tail(n = 1) %>%
  ungroup()

ggplot(forest_share_land_area_continent, aes(x=year, y=forest_cover, color=entity)) + 
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  geom_text(aes(x = 1991, y = 26, label = "Africa"), 
            inherit.aes = FALSE,
            color = "red",
            fontface = "bold",
            family = "sans",
            size = 10
            ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), 
                     expand = expansion(mult = c(0.05, 0.05))) +
  theme_classic() + 
  labs(x = "Year",
       y = "Forested Area (%)",
       title = "The forested area in Africa decreased\nby approximately 3.5%",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom",
        legend.background = element_rect(fill = "bisque1", color = "black")) 

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_continent.png", width = 12, height = 12, dpi = 300)

# Line chart for forest as a share of land area (By Region)

last_points_forest_1 <- forest_share_land_area_africa_regions %>%
  group_by(entity) %>%
  slice_tail(n = 1) %>%
  ungroup()

ggplot(forest_share_land_area_africa_regions, aes(x=year, y=forest_cover, color=entity)) + 
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), 
                     expand = expansion(mult = c(0.05, 0.05))) +
  theme_classic() + 
  geom_text_repel(data = last_points_forest_1,
            aes(label = entity),
            hjust = 0,
            vjust = 0,
            size = 8,
            color = "black",
            show.legend = FALSE) +  
  labs(x = "Year",
       y = "Forested Area (%)",
       title = "Central Africa had the greatest percentage decrease\nin forested area",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "none",
        legend.background = element_rect(fill = "bisque1", color = "black")) 

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_regions.png", width = 12, height = 12, dpi = 300)

# Countries with the highest and lowest forest as a share of land area (2020)

# Highest

forest_share_land_area_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover)) |>
  top_n(5) |>
  ggplot(aes(x=reorder(entity, forest_cover), y = forest_cover, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Percentage forested area (%)",
       title = "African countries with the highest percentage\nof forested area (2020)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_africa_top_5.png", width = 12, height = 12, dpi = 300)

# Lowest

forest_share_land_area_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover)) |>
  top_n(-5) |>
  ggplot(aes(x=reorder(entity, forest_cover), y = forest_cover, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  geom_text(aes(y = forest_cover, label = entity),
            hjust = -0.15,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Percentage forested area (%)",
       title = "African countries with the lowest percentage\nof forested area (2020)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_africa_bottom_5.png", width = 12, height = 12, dpi = 300)

# Map of countries showing forest as a share of land area between 1990 and 2020

##############.....................

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# Get 1990 data
forest_share_land_area_africa_countries_1990 <- forest_share_land_area_africa_countries_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(forest_cover))

# Now we have the 1990 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

forest_share_land_area_africa_countries_1990_full_join <- full_join(africa, 
                                                                    forest_share_land_area_africa_countries_1990,
                                                                 by = c("admin" = "entity"))

# Find rows only in africa

forest_share_land_area_africa_countries_1990_anti_join_1 <- anti_join(africa, 
                                                                      forest_share_land_area_africa_countries_1990, 
                                                                   by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_1990

forest_share_land_area_africa_countries_1990_anti_join_2 <- anti_join(forest_share_land_area_africa_countries_1990, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p11 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_africa_countries_1990_full_join, aes(fill = forest_cover), linewidth = 1) +
  #scale_fill_viridis_b(trans = "sqrt", alpha = 0.4) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_1990.png", width = 9, height = 16, dpi = 300)

#######################
# Now do a patchwork plot....
# Get 2000 data
forest_share_land_area_africa_countries_2000 <- forest_share_land_area_africa_countries_rnaturalearth |> 
  filter(year == 2000) |>
  arrange(desc(forest_cover))

# Now we have the 2000 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

forest_share_land_area_africa_countries_2000_full_join <- full_join(africa, 
                                                                    forest_share_land_area_africa_countries_2000,
                                                                    by = c("admin" = "entity"))

# Find rows only in africa

forest_share_land_area_africa_countries_2000_anti_join_1 <- anti_join(africa, 
                                                                      forest_share_land_area_africa_countries_2000, 
                                                                      by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2000

forest_share_land_area_africa_countries_2000_anti_join_2 <- anti_join(forest_share_land_area_africa_countries_2000, 
                                                                      africa, 
                                                                      by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p22 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_africa_countries_2000_full_join, aes(fill = forest_cover), linewidth = 1) +
  #scale_fill_viridis_b(trans = "sqrt", alpha = 0.4) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_2000.png", width = 9, height = 16, dpi = 300)

# Get 2010 data
forest_share_land_area_africa_countries_2010 <- forest_share_land_area_africa_countries_rnaturalearth |> 
  filter(year == 2010) |>
  arrange(desc(forest_cover))

# Now we have the 2010 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

forest_share_land_area_africa_countries_2010_full_join <- full_join(africa, 
                                                                    forest_share_land_area_africa_countries_2010,
                                                                    by = c("admin" = "entity"))

# Find rows only in africa

forest_share_land_area_africa_countries_2010_anti_join_1 <- anti_join(africa, 
                                                                      forest_share_land_area_africa_countries_2010, 
                                                                      by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2010

forest_share_land_area_africa_countries_2010_anti_join_2 <- anti_join(forest_share_land_area_africa_countries_2010, 
                                                                      africa, 
                                                                      by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p33 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_africa_countries_2010_full_join, aes(fill = forest_cover), linewidth = 1) +
  #scale_fill_viridis_b(trans = "sqrt", alpha = 0.4) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_2010.png", width = 9, height = 16, dpi = 300)

# Get 2020 data

forest_share_land_area_africa_countries_2020 <- forest_share_land_area_africa_countries_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover))

# Now we have the 2020 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

forest_share_land_area_africa_countries_2020_full_join <- full_join(africa, 
                                                                    forest_share_land_area_africa_countries_2020,
                                                                    by = c("admin" = "entity"))

# Find rows only in africa

forest_share_land_area_africa_countries_2020_anti_join_1 <- anti_join(africa, 
                                                                      forest_share_land_area_africa_countries_2020, 
                                                                      by = c("admin" = "entity"))

# Find rows only in share_global_forest_africa_countries_2020

forest_share_land_area_africa_countries_2020_anti_join_2 <- anti_join(forest_share_land_area_africa_countries_2020, 
                                                                      africa, 
                                                                      by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p44 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = forest_share_land_area_africa_countries_2020_full_join, aes(fill = forest_cover), linewidth = 1) +
  #scale_fill_viridis_b(trans = "sqrt", alpha = 0.4) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
    theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20, hjust = 1),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "")

ggsave("sub_pro_3_forest_cover_owid/images/forest_share/forest_share_2020.png", width = 9, height = 16, dpi = 300)

# Calculate countries with the biggest changes (increase or decrease)

forest_share_land_area_africa_countries_join_1990_2020 <- forest_share_land_area_africa_countries_1990 %>% 
  full_join(forest_share_land_area_africa_countries_2020, by = "entity")

forest_share_land_area_africa_countries_join_1990_2020 <- forest_share_land_area_africa_countries_join_1990_2020 |> 
  mutate(changes = forest_cover.y - forest_cover.x) |>
  mutate(abs_changes = abs(changes)) |>
  arrange(desc(changes))
