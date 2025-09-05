# Green Coffee coffee_bean Production Time Series

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

# 1) Load the required datasets and data cleaning

coffee_bean_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/coffee-bean-production-tonnes.csv")

# Clean the datasets

coffee_bean_prod_clean <- coffee_bean_prod %>%
  clean_names() 

# Only include African Countries

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

#############
# Check if the values in the african_countries dataset are present in new dataframes

# african_countries[!(african_countries %in% unique(global_meat_clean_africa$country))]

# african_countries[!(african_countries %in% unique(share_net_clean_africa$country))]
#############

# Global Meat Production in Africa

coffee_bean_prod_clean_africa <- coffee_bean_prod_clean |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

# To combine the datasets with mapping dataset - change some of the country names to match

# Change to standard names used in rnaturalearth for maps

coffee_bean_prod_clean_africa_rnaturalearth <- coffee_bean_prod_clean_africa %>%
  mutate(country = case_when(
    country == "Cape Verde"  ~ "Cabo Verde",
    country == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    country == "Eswatini"  ~ "eSwatini",
    country == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    country == "Tanzania"  ~ "United Republic of Tanzania",
    country == "Congo"  ~ "Republic of the Congo",
    TRUE ~ country  # Retain original name if none of the conditions are met
  )) |>
  rename(coffee_bean_production = "coffee_beans_dry_00000176_production_005510_tonnes")

# 2) Map of countries showing global meat production between 1965 and 2020

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# Get 1965 data

coffee_bean_prod_clean_africa_1965 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1965) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1965 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1965_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1965,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1965_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1965, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1965

coffee_bean_prod_clean_africa_1965_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1965, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1965_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1965",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1965.png", width = 9, height = 16, dpi = 300)



# Get 1970 data

coffee_bean_prod_clean_africa_1970 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1970) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1970 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1970_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1970,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1970_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1970, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1970

coffee_bean_prod_clean_africa_1970_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1970, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p2 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1970_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1970",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1970.png", width = 9, height = 16, dpi = 300)



# Get 1975 data

coffee_bean_prod_clean_africa_1975 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1975) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1975 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1975_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1975,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1975_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1975, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1975

coffee_bean_prod_clean_africa_1975_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1975, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p3 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1975_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1975",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1975.png", width = 9, height = 16, dpi = 300)



# Get 1980 data

coffee_bean_prod_clean_africa_1980 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1980) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1980 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1980_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1980,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1980_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1980, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1980

coffee_bean_prod_clean_africa_1980_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1980, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p4 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1980_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1980",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1980.png", width = 9, height = 16, dpi = 300)



# Get 1985 data

coffee_bean_prod_clean_africa_1985 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1985) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1985 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1985_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1985,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1985_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1985, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1985

coffee_bean_prod_clean_africa_1985_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1985, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p5 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1985_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1985",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1985.png", width = 9, height = 16, dpi = 300)



# Get 1990 data

coffee_bean_prod_clean_africa_1990 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1990 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1990_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1990,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1990_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1990, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1990

coffee_bean_prod_clean_africa_1990_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1990, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p6 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1990_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1990.png", width = 9, height = 16, dpi = 300)



# Get 1995 data

coffee_bean_prod_clean_africa_1995 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 1995) |>
  arrange(desc(coffee_bean_production))

# Now we have the 1995 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_1995_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_1995,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_1995_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_1995, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_1995

coffee_bean_prod_clean_africa_1995_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_1995, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p7 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_1995_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1995",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_1995.png", width = 9, height = 16, dpi = 300)



# Get 2000 data

coffee_bean_prod_clean_africa_2000 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 2000) |>
  arrange(desc(coffee_bean_production))

# Now we have the 2000 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_2000_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_2000,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_2000_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_2000, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_2000

coffee_bean_prod_clean_africa_2000_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_2000, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p8 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_2000_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_2000.png", width = 9, height = 16, dpi = 300)



# Get 2005 data

coffee_bean_prod_clean_africa_2005 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 2005) |>
  arrange(desc(coffee_bean_production))

# Now we have the 2005 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_2005_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_2005,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_2005_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_2005, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_2005

coffee_bean_prod_clean_africa_2005_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_2005, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p9 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_2005_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2005",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_2005.png", width = 9, height = 16, dpi = 300)



# Get 2010 data

coffee_bean_prod_clean_africa_2010 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 2010) |>
  arrange(desc(coffee_bean_production))

# Now we have the 2010 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_2010_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_2010,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_2010_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_2010, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_2010

coffee_bean_prod_clean_africa_2010_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_2010, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p10 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_2010_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_2010.png", width = 9, height = 16, dpi = 300)



# Get 2015 data

coffee_bean_prod_clean_africa_2015 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 2015) |>
  arrange(desc(coffee_bean_production))

# Now we have the 2015 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_2015_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_2015,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_2015_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_2015, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_2015

coffee_bean_prod_clean_africa_2015_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_2015, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p11 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_2015_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2015",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_2015.png", width = 9, height = 16, dpi = 300)



# Get 2020 data

coffee_bean_prod_clean_africa_2020 <- coffee_bean_prod_clean_africa_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(coffee_bean_production))

# Now we have the 2020 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

coffee_bean_prod_clean_africa_2020_full_join <- full_join(africa, 
                                                   coffee_bean_prod_clean_africa_2020,
                                                   by = c("admin" = "country"))

# Find rows only in africa

coffee_bean_prod_clean_africa_2020_anti_join_1 <- anti_join(africa, 
                                                     coffee_bean_prod_clean_africa_2020, 
                                                     by = c("admin" = "country"))

# Find rows only in global_meat_clean_africa_2020

coffee_bean_prod_clean_africa_2020_anti_join_2 <- anti_join(coffee_bean_prod_clean_africa_2020, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p12 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = coffee_bean_prod_clean_africa_2020_full_join, aes(fill = coffee_bean_production), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 600000),
                       labels  = label_number(scale = 1e-6),
                       name = "Millions of Tonnes",
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
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_7_agriculture_owid/images/coffee_bean_time_series/coffee_bean_clean_africa_map_2020.png", width = 9, height = 16, dpi = 300)
