# BlackMarbleR
# R package that provides a simple way to use nighttime 
# lights data from NASA’s Black Marble

# https://worldbank.github.io/blackmarbler/
# https://blogs.worldbank.org/en/opendata/illuminating-insights-harnessing-nasas-black-marble-r-and-python-packages

# A) Installation

# install.packages("blackmarbler")
# devtools::install_github("worldbank/blackmarbler")

# B) Setup

#### Setup
# Load packages
library(blackmarbler)
library(geodata)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(lubridate)

#### Define NASA bearer token 

#### Found in the system environment

bearer <- Sys.getenv('BEARER')

# C) Example using Ghana

### ROI
# Define region of interest (roi). The roi must be (1) an sf polygon and (2)
# in the WGS84 (epsg:4326) coordinate reference system. Here, we use the
# getData function to load a polygon of Ghana
roi_sf <- gadm(country = "GHA", level=1, path = tempdir()) 

# D) Make raster of nighttime lights

# The below example shows making daily, monthly, and annual rasters of 
# nighttime lights for Ghana.

### Daily data: raster for February 5, 2021
r_20210205 <- bm_raster(roi_sf = roi_sf,
                        product_id = "VNP46A2",
                        date = "2021-02-05",
                        bearer = bearer)

### Monthly data: raster for October 2021
r_202110 <- bm_raster(roi_sf = roi_sf,
                      product_id = "VNP46A3",
                      date = "2021-10-01", # The day is ignored
                      bearer = bearer)

### Annual data: raster for 2021
r_2021 <- bm_raster(roi_sf = roi_sf,
                    product_id = "VNP46A4",
                    date = 2021,
                    bearer = bearer)

# E) Make raster of nighttime lights across multiple time periods

# To extract data for multiple time periods, add multiple time periods to date.

#### Daily data in March 2021
r_daily <- bm_raster(roi_sf = roi_sf,
                     product_id = "VNP46A2",
                     date = seq.Date(from = ymd("2021-03-01"), to = ymd("2021-03-31"), by = "day"),
                     bearer = bearer)

#### Monthly aggregated data in 2021 and 2022
r_monthly <- bm_raster(roi_sf = roi_sf,
                       product_id = "VNP46A3",
                       date = seq.Date(from = ymd("2021-01-01"), to = ymd("2022-12-01"), by = "month"),
                       bearer = bearer)

#### Yearly aggregated data in 2012 and 2021
r_annual <- bm_raster(roi_sf = roi_sf,
                      product_id = "VNP46A4",
                      date = 2012:2021,
                      bearer = bearer)

# F) Map of nighttime lights

# Using one of the rasters, we can make a map of nighttime lights

#### Make raster
r <- bm_raster(roi_sf = roi_sf,
               product_id = "VNP46A3",
               date = "2021-10-01",
               bearer = bearer)

#### Prep data
r <- r |> terra::mask(roi_sf)

## Distribution is skewed, so log
r[] <- log(r[] + 1)

##### Map
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5,
                       na.value = "transparent") +
  labs(title = "Nighttime Lights: October 2021") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

# G) Trends over time

# We can use the bm_extract function to observe changes in nighttime lights over time.

#### Extract annual data
ntl_df <- bm_extract(roi_sf = roi_sf,
                     product_id = "VNP46A4",
                     date = 2021:2022,
                     bearer = bearer)

#### Trends over time
ntl_df |>
  ggplot() +
  geom_col(aes(x = date,
               y = ntl_mean),
           fill = "darkorange") +
  facet_wrap(~NAME_1) +
  labs(x = NULL,
       y = "NTL Luminosity",
       title = "Ghana Admin Level 1: Annual Average Nighttime Lights") +
  scale_x_continuous(labels = seq(2012, 2022, 4),
                     breaks = seq(2012, 2022, 4)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

