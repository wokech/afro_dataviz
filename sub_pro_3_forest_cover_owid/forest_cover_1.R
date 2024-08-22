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

##############

# Share of global forest - Continent
share_global_forest_continent <- share_global_forest %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Share of global forest - Countries
share_global_forest_countries <- share_global_forest %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Share of global forest - Africa Regions
share_global_forest_africa_regions <- share_global_forest %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Middle Africa",
                       "Southern Africa", "Western Africa"))

# Share of global forest - Africa Countries

##########IVORY COAST IS MISSING

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

unique(share_global_forest_africa_countries$entity)


###############

# Forest as a share of land area - Continent
forest_share_land_area_continent <- forest_share_land_area %>%
  filter(entity %in% c("Africa", "Asia", "Europe", "Northern America", 
                       "Oceania", "South America", "Central America"))

# Forest as a share of land area - Countries
forest_share_land_area_countries <- forest_share_land_area %>%
  filter(!is.na(code)) %>%
  filter(entity != "World")

# Forest as a share of land area - Africa Regions
forest_share_land_area_africa <- forest_share_land_area %>%
  filter(entity %in% c("Eastern Africa", "Northern Africa", "Middle Africa",
                       "Southern Africa", "Western Africa"))

# Forest as a share of land area - Africa Countries

##########IVORY COAST IS MISSING

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

unique(forest_share_land_area_africa_countries$entity)

# B) EDA

# Try a streamgraph
  
# devtools::install_github("hrbrmstr/streamgraph")

# library(streamgraph)

# pp <- streamgraph(share_global_forest_continent, key="entity", value="share_of_global_forest_area", date="year", height="300px", width="1000px")
# pp

# Stacked area chart for global

ggplot(share_global_forest_continent, aes(x=year, y=share_of_global_forest_area, fill=entity)) + 
  geom_area(alpha=0.6 , size=1, colour="black") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("")

# Line plot
ggplot(share_global_forest_africa, aes(x=year, y=share_of_global_forest_area, color=entity)) + 
  geom_line(alpha=0.6, size=1) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("")

 
