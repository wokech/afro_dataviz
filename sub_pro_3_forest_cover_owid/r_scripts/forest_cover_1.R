# Forests by OWID
# Here we look at two datasets:
# 1) Share of global forest area;
# 2) Forest cover as a share of total land area within the country or region.

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

# Load data

# Share of global forests worldwide
share_global_forest <- read_csv("sub_pro_3_forest_cover_owid/datasets/share-global-forest.csv")

# Forest as a share of land area
forest_share_land_area <- read_csv("sub_pro_3_forest_cover_owid/datasets/forest-area-as-share-of-land-area.csv")

# Clean datasets
share_global_forest <- share_global_forest %>%
  clean_names()
forest_share_land_area <- forest_share_land_area %>%
  clean_names()

# Set up datasets 

############## Share of global forests worldwide

# Share of global forest (By Continent)
share_global_forest_continent <- share_global_forest %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Share of global forest (By Country)
share_global_forest_countries <- share_global_forest %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Share of global forest (By African Region)
share_global_forest_africa_regions <- share_global_forest %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Middle Africa",
                       "Southern Africa", "Western Africa")) %>%
  mutate(entity = ifelse(entity == 'Middle Africa', 'Central Africa', entity))

# Share of global forest (By African Countries)
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

share_global_forest_africa_countries <- share_global_forest %>%
  filter(entity %in% african_countries)

share_global_forest_africa_countries_rnaturalearth <- share_global_forest_africa_countries %>%
  mutate(entity = case_when(
    entity == "Cape Verde"  ~ "Cabo Verde",
    entity == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    entity == "Eswatini"  ~ "eSwatini",
    entity == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    entity == "Tanzania"  ~ "United Republic of Tanzania",
    entity == "Congo"  ~ "Republic of the Congo",
    TRUE ~ entity  # Retain original name if none of the conditions are met
  ))

############### Forests as a share of land area

# Forest as a share of land area (By Continent)
forest_share_land_area_continent <- forest_share_land_area %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Forest as a share of land area (By Countries)
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

forest_share_land_area_africa_countries <- forest_share_land_area %>%
  filter(entity %in% african_countries)

##################################################################

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

# Share of global forests worldwide

# Stacked area chart for share of global forest (By Continent)

ggplot(share_global_forest_continent, aes(x=year, y=share_of_global_forest_area, fill=entity)) + 
  geom_area(alpha=0.6 , size=0.5, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("")

# Line plot for share of global forest in Africa (By Region)

ggplot(share_global_forest_africa_regions, aes(x=year, y=share_of_global_forest_area, color=entity)) + 
  geom_line(alpha=0.5, size=1) +
  geom_point(size=2) +
  theme_classic() + 
  ggtitle("")

# Regions with the highest and lowest share of global forest (2020)

share_global_forest_africa_regions |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area)) |>
  ggplot(aes(x=reorder(entity, share_of_global_forest_area), y = share_of_global_forest_area, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Countries with the highest and lowest share of global forest (2020)

# Highest

share_global_forest_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area)) |>
  top_n(5) |>
  ggplot(aes(x=reorder(entity, share_of_global_forest_area), y = share_of_global_forest_area, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Lowest

share_global_forest_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area)) |>
  top_n(-5) |>
  ggplot(aes(x=reorder(entity, share_of_global_forest_area), y = share_of_global_forest_area, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Map of countries showing share of global forest area in 1990 vs 2020

#########.............

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# 1990
share_global_forest_africa_countries_1990 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(share_of_global_forest_area))


# Step 1: Identify rows that don't match
# Left join to keep all rows from africa

share_global_forest_africa_countries_1990_full_join <- full_join(africa, 
                                                          share_global_forest_africa_countries_1990,
                                                          by = c("admin" = "entity"))

# Rows only in africa

share_global_forest_africa_countries_1990_anti_join_1 <- anti_join(africa, 
                                                                   share_global_forest_africa_countries_1990, 
                                                                   by = c("admin" = "entity"))

# Rows only in share_global...
share_global_forest_africa_countries_1990_anti_join_2 <- anti_join(share_global_forest_africa_countries_1990, 
                                                                   africa, 
                                                                   by = c("entity" = "admin"))

# Change


# 2020
share_global_forest_africa_countries_2020 <- share_global_forest_africa_countries_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area))


# Forests as a share of land area (within country)

# Line chart for forest as a share of land area (By Continent)

ggplot(forest_share_land_area_continent, aes(x=year, y=forest_cover, color=entity)) + 
  geom_line(alpha=0.5, size=1) +
  geom_point(size=2) +
  theme_ipsum() + 
  ggtitle("")

# Line chart for forest as a share of land area (By Region)

ggplot(forest_share_land_area_africa_regions, aes(x=year, y=forest_cover, color=entity)) + 
  geom_line(alpha=0.6, size=1) +
  geom_point(size=2) +
  theme_ipsum() + 
  ggtitle("")

# Regions with the highest and lowest forest as a share of land area (2020)

forest_share_land_area_africa_regions |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover)) |>
ggplot(aes(x=reorder(entity, forest_cover), y = forest_cover, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Countries with the highest and lowest forest as a share of land area (2020)

# Highest

forest_share_land_area_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover)) |>
  top_n(5) |>
  ggplot(aes(x=reorder(entity, forest_cover), y = forest_cover, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Lowest

forest_share_land_area_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(forest_cover)) |>
  top_n(-5) |>
  ggplot(aes(x=reorder(entity, forest_cover), y = forest_cover, fill = entity)) + 
  geom_bar(stat = "identity") +
  theme_ipsum() + 
  coord_flip() +
  ggtitle("2020")

# Map of countries showing forest as a share of land area in 1990 vs 2020
##############.....................

forest_share_land_area_africa_countries |> 
  filter(year == 1990) |>
  arrange(desc(share_of_global_forest_area))

forest_share_land_area_africa_countries |> 
  filter(year == 2020) |>
  arrange(desc(share_of_global_forest_area))
