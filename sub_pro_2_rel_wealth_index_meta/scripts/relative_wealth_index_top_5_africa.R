# Relative Wealth Index - Top 5 Economies in Africa
# using the Meta DataforGood dataset

# Install necessary packages
# install.packages(c("ggplot2", "sf", "viridis", "rnaturalearth", "rnaturalearthdata"))

# Load the libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(ggrepel)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. South Africa

# Read the CSV file
zaf_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/zaf_relative_wealth_index.csv")

# Convert the data to an sf object
zaf_data_sf <- st_as_sf(zaf_data, coords = c("longitude", "latitude"), crs = 4326)

# South Africa country borders
south_africa <- ne_countries(scale = "medium", country = "South Africa", returnclass = "sf")

# Create a data frame with major South African towns
major_towns_south_africa <- data.frame(
  name = c("Johannesburg", "Cape Town", "Durban", "Pretoria", "Port Elizabeth"),
  longitude = c(28.0473, 18.4241, 31.0218, 28.1881, 25.6022),
  latitude = c(-26.2041, -33.9249, -29.8587, -25.7461, -33.9608)
)

# Map of South Africa

ggplot() +
  geom_sf(data = south_africa, fill = NA, color = "black", linewidth = 1) + # Add South Africa borders
  geom_point(data = zaf_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_south_africa, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_south_africa, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "1. South Africa",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  coord_sf(xlim = c(16, 33), ylim = c(-35, -22), expand = FALSE) + # Bounding Box
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_2_rel_wealth_index_meta/images/south_africa.png", width = 12, height = 12, dpi = 72)

# 2. Egypt

# Read the CSV file
egy_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/egy_relative_wealth_index.csv")

# Convert the data to an sf object
egy_data_sf <- st_as_sf(egy_data, coords = c("longitude", "latitude"), crs = 4326)

# Egypt country borders
egypt <- ne_countries(scale = "medium", country = "Egypt", returnclass = "sf")

# Create a data frame with major Egyptian towns
major_towns_egypt <- data.frame(
  name = c("Cairo", "Alexandria", "Giza", "Shubra El Kheima", "Port Said"),
  longitude = c(31.2357, 29.9187, 31.2089, 31.2445, 32.3019),
  latitude = c(30.0444, 31.2001, 30.0131, 30.1279, 31.2652)
)

# Map of Egypt

ggplot() +
  geom_sf(data = egypt, fill = NA, color = "black", linewidth = 1) + # Add Egypt borders
  geom_point(data = egy_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_egypt, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_egypt, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "2. Egypt",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_2_rel_wealth_index_meta/images/egypt.png", width = 12, height = 12, dpi = 72)

# 3. Algeria

# Read the CSV file
alg_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/alg_relative_wealth_index.csv")

# Convert the data to an sf object
alg_data_sf <- st_as_sf(alg_data, coords = c("longitude", "latitude"), crs = 4326)

# Algeria country borders
algeria <- ne_countries(scale = "medium", country = "Algeria", returnclass = "sf")

# Create a data frame with major Algerian towns
major_towns_algeria <- data.frame(
  name = c("Algiers", "Oran", "Constantine", "Annaba", "Blida"),
  longitude = c(3.0588, -0.6349, 6.6147, 7.7453, 2.8275),
  latitude = c(36.7538, 35.6969, 36.3650, 36.9009, 36.4722)
)

# Map of Algeria

ggplot() +
  geom_sf(data = algeria, fill = NA, color = "black", linewidth = 1) + # Add Algeria borders
  geom_point(data = alg_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_algeria, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_algeria, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "3. Algeria",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_2_rel_wealth_index_meta/images/algeria.png", width = 12, height = 12, dpi = 72)

# 4. Nigeria

# Read the CSV file
nga_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/nga_relative_wealth_index.csv")

# Convert the data to an sf object
nga_data_sf <- st_as_sf(nga_data, coords = c("longitude", "latitude"), crs = 4326)

# Nigeria country borders
nigeria <- ne_countries(scale = "medium", country = "Nigeria", returnclass = "sf")

# Create a data frame with major Nigerian towns
major_towns_nigeria <- data.frame(
  name = c("Lagos", "Kano", "Ibadan", "Abuja", "Port Harcourt"),
  longitude = c(3.3792, 8.5167, 3.9000, 7.4951, 7.0498),
  latitude = c(6.5244, 12.0000, 7.3878, 9.0579, 4.8156)
)

# Map of Nigeria

ggplot() +
  geom_sf(data = nigeria, fill = NA, color = "black", linewidth = 1) + # Add Nigeria borders
  geom_point(data = nga_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_nigeria, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_nigeria, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "4. Nigeria",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_2_rel_wealth_index_meta/images/nigeria.png", width = 12, height = 12, dpi = 72)

# 5. Morocco

# Read the CSV file
mor_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/mor_relative_wealth_index.csv") %>%
  select(-quadkey)

# Convert the data to an sf object
mor_data_sf <- st_as_sf(mor_data, coords = c("longitude", "latitude"), crs = 4326)

# Morocco country borders
morocco <- ne_countries(scale = "medium", country = "Morocco", returnclass = "sf")

# Create a data frame with major Moroccan towns
major_towns_morocco <- data.frame(
  name = c("Casablanca", "Marrakech", "Fez", "Rabat", "Tangier"),
  longitude = c(-7.5898, -7.9811, -5.0003, -6.8416, -5.8339),
  latitude = c(33.5731, 31.6295, 34.0331, 34.0209, 35.7595)
)

# Map of Moroccan towns

ggplot() +
  geom_sf(data = morocco, fill = NA, color = "black", linewidth = 1) + # Add Morocco borders
  geom_point(data = mor_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_morocco, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_morocco, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma", breaks = c(-1, 0, 1)) +
  theme_void() +
  labs(title = "5. Morocco",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_2_rel_wealth_index_meta/images/morocco.png", width = 12, height = 12, dpi = 72)

