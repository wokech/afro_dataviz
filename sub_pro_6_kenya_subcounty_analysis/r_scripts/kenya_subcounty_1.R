# Kenya Subcounty Analysis
# Census data analyzed at the subcounty level

## Load the required libraries

library(ggplot2)
library(sf)

## Load the shapefile and plot the subcounty map

kenya_subcounties <- st_read("sub_pro_6_kenya_subcounty_analysis/shapefiles/ke_subcounty.shp")

ggplot(kenya_counties) + 
  geom_sf(fill = "bisque2", linewidth = 1, color = "black") + 
  theme_void()
