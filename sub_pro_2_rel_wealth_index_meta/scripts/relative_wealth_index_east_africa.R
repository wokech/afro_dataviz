# Relative Wealth Index - East Africa
# using the Meta DataforGood dataset

# Install necessary packages
# install.packages(c("ggplot2", "sf", "viridis", "rnaturalearth", "rnaturalearthdata"))

# Load the libraries
library(ggplot2)
library(sf)
library(ggrepel)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Kenya

# Read the CSV file
ken_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/ken_relative_wealth_index.csv")

# Convert the data to an sf object
ken_data_sf <- st_as_sf(ken_data, coords = c("longitude", "latitude"), crs = 4326)

# Kenya country borders
kenya <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")

# Create a data frame with major Kenyan towns
major_towns_kenya <- data.frame(
  name = c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret"),
  longitude = c(36.8219, 39.6682, 34.7617, 36.0667, 35.2699),
  latitude = c(-1.2921, -4.0435, -0.1022, -0.3031, 0.5143)
)

# Map of Kenya

ggplot() +
  geom_sf(data = kenya, fill = NA, color = "black", linewidth = 1) + # Add Kenya borders
  geom_point(data = ken_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_kenya, aes(x = longitude, y = latitude, label = name), 
            color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_kenya, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin Kenya",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.25, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/kenya.png", width = 12, height = 12, dpi = 72)

# Tanzania

# Read the CSV file
tza_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/tza_relative_wealth_index.csv")

# Convert the data to an sf object
tza_data_sf <- st_as_sf(tza_data, coords = c("longitude", "latitude"), crs = 4326)

# Tanzania country borders
tanzania <- ne_countries(scale = "medium", country = "United Republic of Tanzania", returnclass = "sf")

# Create a data frame with major Tanzanian towns
major_towns_tanzania <- data.frame(
  name = c("Dodoma", "Dar es Salaam", "Mwanza", "Arusha", "Mbeya"),
  latitude = c(-6.1630, -6.7924, -2.5164, -3.3869, -8.9094),
  longitude = c(35.7516, 39.2083, 32.9175, 36.6828, 33.4582)
)

# Map of Tanzania

ggplot() +
  geom_sf(data = tanzania, fill = NA, color = "black", linewidth = 1) + # Add Tanzania borders
  geom_point(data = tza_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.9) +
  geom_text_repel(data = major_towns_tanzania, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_tanzania, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin Tanzania",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.25, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/tanzania.png", width = 12, height = 12, dpi = 72)


# Uganda

# Read the CSV file
uga_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/uga_relative_wealth_index.csv")

# Convert the data to an sf object
uga_data_sf <- st_as_sf(uga_data, coords = c("longitude", "latitude"), crs = 4326)

# Uganda country borders
uganda <- ne_countries(scale = "medium", country = "Uganda", returnclass = "sf")

# Create a data frame with major Ugandan towns
major_towns_uganda <- data.frame(
    name = c("Kampala", "Gulu", "Lira", "Jinja", "Mbarara"),
    latitude = c(0.3476, 2.7716, 2.2491, 0.4443, -0.6072),
    longitude = c(32.5825, 32.2989, 32.8998, 33.2086, 30.6580)
  )

# Map of Uganda

ggplot() +
  geom_sf(data = uganda, fill = NA, color = "black", linewidth = 1) + # Add Uganda borders
  geom_point(data = uga_data, aes(x = longitude, y = latitude, color = rwi), size = 1, alpha = 0.9) +
  geom_text_repel(data = major_towns_uganda, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_uganda, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin Uganda",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.3, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/uganda.png", width = 12, height = 12, dpi = 72)


# Burundi

# Read the CSV file
bdi_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/bdi_relative_wealth_index.csv")

# Convert the data to an sf object
bdi_data_sf <- st_as_sf(bdi_data, coords = c("longitude", "latitude"), crs = 4326)

# Burundi country borders
burundi <- ne_countries(scale = "medium", country = "Burundi", returnclass = "sf")

# Create a data frame with major Burundian towns
major_towns_burundi <- data.frame(
  name = c("Bujumbura", "Gitega", "Ngozi", "Ruyigi", "Kayanza"),
  latitude = c(-3.3731, -3.4264, -2.9075, -3.4753, -2.9227),
  longitude = c(29.3644, 29.9246, 29.8301, 30.2485, 29.6295)
)

# Map of Burundi

ggplot() +
  geom_sf(data = burundi, fill = NA, color = "black", linewidth = 1) + # Add Burundi borders
  geom_point(data = bdi_data, aes(x = longitude, y = latitude, color = rwi), size = 3, alpha = 0.9) +
  geom_text_repel(data = major_towns_burundi, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_burundi, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin Burundi",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.5, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/burundi.png", width = 12, height = 12, dpi = 72)


# Rwanda

# Read the CSV file
rwa_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/rwa_relative_wealth_index.csv")

# Convert the data to an sf object
rwa_data_sf <- st_as_sf(rwa_data, coords = c("longitude", "latitude"), crs = 4326)

# Rwanda country borders
rwanda <- ne_countries(scale = "medium", country = "Rwanda", returnclass = "sf")

# Create a data frame with major Rwandan towns
major_towns_rwanda <- data.frame(
  name = c("Kigali", "Butare", "Gisenyi", "Ruhengeri", "Muhanga"),
  latitude = c(-1.9706, -2.5985, -1.6840, -1.4998, -2.0736),
  longitude = c(30.1044, 29.7370, 29.2625, 29.6342, 29.7565)
)

# Map of Rwanda

ggplot() +
  geom_sf(data = rwanda, fill = NA, color = "black", linewidth = 1) + # Add Rwanda borders
  geom_point(data = rwa_data, aes(x = longitude, y = latitude, color = rwi), size = 3.5, alpha = 0.9) +
  geom_text_repel(data = major_towns_rwanda, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_rwanda, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin Rwanda",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.1, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/rwanda.png", width = 12, height = 12, dpi = 72)


# DR Congo

# Read the CSV file
cod_data <- read.csv("sub_pro_20_relative_wealth_index/datasets/cod_relative_wealth_index.csv")

# Convert the data to an sf object
cod_data_sf <- st_as_sf(cod_data, coords = c("longitude", "latitude"), crs = 4326)

# Democratic Republic of the Congo country borders
drcongo <- ne_countries(scale = "medium", country = "Democratic Republic of the Congo", returnclass = "sf")

# Create a data frame with major Democratic Republic of the Congo towns
major_towns_cod <- data.frame(
  name = c("Kinshasa", "Lubumbashi", "Mbuji-Mayi", "Kananga", "Kisangani"),
  latitude = c(-4.4419, -11.6600, -6.1573, -5.8962, 0.5153),
  longitude = c(15.2663, 27.4794, 23.6045, 22.4166, 25.1900)
)

# Map of DR Congo

ggplot() +
  geom_sf(data = drcongo, fill = NA, color = "black", linewidth = 1) + # Add DR Congo borders
  geom_point(data = cod_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.9) +
  geom_text_repel(data = major_towns_cod, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_cod, aes(x = longitude, y = latitude), 
             color = "black", size = 5, shape = 13) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Wealth Index Distribution\nin the DR Congo",
       subtitle = "",
       caption = "Data Source: Meta (Data for Good)",
       color = "Relative Wealth Index") +
  theme(plot.title = element_text(family="Helvetica", face="bold", size = 44, hjust = 0.5),
        #plot.subtitle = element_text(family="Helvetica", size = 24, hjust = 0.5),
        plot.title.position = 'plot',
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = -0.25, vjust = 0.5),
        plot.caption.position = 'plot',
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_text(family="Helvetica", size = 24),
        legend.text = element_text(size = 24), 
        legend.position = "top")

ggsave("sub_pro_20_relative_wealth_index/images/drcongo.png", width = 12, height = 12, dpi = 72)

# 6. Ethiopia

# Read the CSV file
eth_data <- read_csv("sub_pro_2_rel_wealth_index_meta/datasets/eth_relative_wealth_index.csv")

# Convert the data to an sf object
eth_data_sf <- st_as_sf(eth_data, coords = c("longitude", "latitude"), crs = 4326)

# Ethiopia country borders
ethiopia <- ne_countries(scale = "medium", country = "Ethiopia", returnclass = "sf")

# Create a data frame with major Ethiopian towns
major_towns_ethiopia <- data.frame(
  name = c("Addis Ababa", "Dire Dawa", "Mek'ele", "Adama", "Gondar"),
  longitude = c(38.7578, 41.8661, 39.4753, 39.2671, 37.4667),
  latitude = c(9.0252, 9.5951, 13.4967, 8.5400, 12.6000)
)

# Map of Ethiopian towns

ggplot() +
  geom_sf(data = ethiopia, fill = NA, color = "black", linewidth = 1) + # Add Ethiopia borders
  geom_point(data = eth_data, aes(x = longitude, y = latitude, color = rwi), size = 0.5, alpha = 0.8) +
  geom_text_repel(data = major_towns_ethiopia, aes(x = longitude, y = latitude, label = name), 
                  color = "black", fontface = "bold", check_overlap = TRUE, size = 10, vjust = 1.5) +
  geom_point(data = major_towns_ethiopia, aes(x = longitude, y = latitude), 
             color = "black", size = 7, shape = 16) +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  labs(title = "6. Ethiopia",
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

ggsave("sub_pro_2_rel_wealth_index_meta/images/ethiopia.png", width = 12, height = 12, dpi = 72)