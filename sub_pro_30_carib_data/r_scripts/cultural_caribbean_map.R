# Map of the Cultural Caribbean

# Load the required libraries and packages

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(patchwork)
library(ggrepel)
library(scales)

# Map of the Cultural Caribbean

# Get the data

caribbean_cultural <- ne_countries(scale = "medium", returnclass = "sf") |>
  subset(admin %in% c(
    "Antigua and Barbuda",
    "The Bahamas",
    "Barbados",
    "Belize",
    "Cuba",
    "Dominica",
    "Dominican Republic",
    "Grenada",
    "Guyana",
    "Haiti",
    "Jamaica",
    "Saint Kitts and Nevis",
    "Saint Lucia",
    "Saint Vincent and the Grenadines",
    "Suriname",
    "Trinidad and Tobago"
  ))

# Plot the map

p1 <- ggplot(data = caribbean_cultural) +
  geom_sf(fill = "lightblue", color = "white") +
  # geom_text_repel(
  #   data = caribbean_cultural,
  #   aes(label = admin, geometry = geometry),
  #   stat = "sf_coordinates",
  #   size = 8,
  #   min.segment.length = 4
  # ) +  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(3, 'cm'), #change legend key width
  ) +
  labs(title = "",
       subtitle = "",
       caption = "") 

p1

#ggsave("carib.png", width = 12, height = 12, dpi = 300)
