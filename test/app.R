library(readxl)
library(leaflet)

all_data_locations <- read_excel("sub_pro_3_hass_land_prices/my_app/data/all_data_locations.xlsx")

# Create a leaflet map object
map <- leaflet() %>%
  addTiles()  # Add a basemap tile layer

# Add markers for each location
map <- map %>% addMarkers(
  data = all_data_locations,
  lat = ~Latitude,
  lng = ~Longitude,
  popup = ~Location
)

# Display the map
map
