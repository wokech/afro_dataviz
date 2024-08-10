# Forest Cover Worldwide

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)

# Load data

# Share of global forest
share_global_forest <- read_csv("sub_pro_3_forest_cover_owid/data/share-global-forest.csv")

# Forest as a share of land area
forest_share_land_area <- read_csv("sub_pro_3_forest_cover_owid/data/forest-area-as-share-of-land-area.csv")

# Clean datasets
share_global_forest <- share_global_forest %>%
  clean_names()
forest_share_land_area <- forest_share_land_area %>%
  clean_names()

# Set up datasets 

# Share of global forest - Continent
share_global_forest_continent <- share_global_forest %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", "Oceania", "South America"))
# Share of global forest - Countries
share_global_forest_countries <- share_global_forest %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Forest as a share of land area - Continent
forest_share_land_area_continent <- share_global_forest %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", "Oceania", "South America"))
# Forest as a share of land area - Countries
forest_share_land_area_countries <- share_global_forest %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# B) EDA

# Plot

ggplot(share_global_forest_continent, aes(x=year, y=share_of_global_forest_area, fill=entity)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("")
