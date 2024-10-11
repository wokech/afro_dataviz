# ICT in East Africa (Part 1)

#THE CASE FOR STARLINK#

# Share of the population who used the Internet in the last three months
# (International Telecommunication Union).

# Our World In Data and World Bank

# Load the required libraries and packages

# install.packages()
# library()

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

# Load the required datasets

# a) Share of individuals using the internet

share_net <- read_csv("sub_pro_8_ict_owid/datasets/share-of-individuals-using-the-internet.csv")

# Clean the datasets

share_net_clean <- share_net %>%
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

# african_countries[!(african_countries %in% unique(ict_per_100_clean_africa$country))]

# african_countries[!(african_countries %in% unique(share_net_clean_africa$country))]
#############

# Share of individuals using the internet in Africa

share_net_clean_africa <- share_net_clean |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)

############## Share using the internet worldwide

# Share of Net Usage (By Continent) 
share_net_clean_continent <- share_net_clean |>
  filter(entity %in% c("East Asia and Pacific (WB)", "Europe and Central Asia (WB)", 
                       "Latin America and Caribbean (WB)", "Middle East and North Africa (WB)", 
                       "North America (WB)", "South Asia (WB)", "Sub-Saharan Africa (WB)")) |>
  mutate(entity = gsub("\\(WB\\)", "", entity))

# To combine the datasets with mapping dataset - change some of the country names to match

# Change to standard names used in rnaturalearth for maps

share_net_clean_africa_rnaturalearth <- share_net_clean_africa %>%
  mutate(country = case_when(
    country == "Cape Verde"  ~ "Cabo Verde",
    country == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    country == "Eswatini"  ~ "eSwatini",
    country == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    country == "Tanzania"  ~ "United Republic of Tanzania",
    country == "Congo"  ~ "Republic of the Congo",
    TRUE ~ country  # Retain original name if none of the conditions are met
  )) |>
  rename(share_net_use = "individuals_using_the_internet_percent_of_population")

# EDA plots

# 1) Share with Internet in SSA

share_net_clean_ssa <- share_net_clean |>
  rename(share_using_net = "individuals_using_the_internet_percent_of_population") |>
  rename("country" = "entity") |>
  filter(country == "Sub-Saharan Africa (WB)") |>
  mutate(country = case_when(
    country == "Sub-Saharan Africa (WB)" ~ "Sub-Saharan Africa",
    TRUE ~ country))

share_net_clean_ssa %>%
  ggplot(aes(x = year, 
             y = share_using_net)) + 
  geom_line(linewidth = 1) +
  geom_rect(
    fill = "brown", alpha = 0.01, 
    xmin = 2010,
    xmax = 2020,
    ymin = 0,
    ymax = 30
  ) +
  labs(x = "Year",
       y = "Individuals using the Internet (% of population)",
       title = "Over a quarter of Africa's population has\naccess to the internet",
       subtitle = "Majority of the internet access was obtained between 2010 and 2020",
       caption = "Data Source: Our World in Data") +
  theme_classic() +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust = 0.5),
        plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

ggsave("sub_pro_8_ict_owid/images/share_net_ssa.png", width = 12, height = 12, dpi = 300)

# 2) Top 5 African Countries (Internet Access)

share_net_clean_africa |> 
  rename(share_using_net = "individuals_using_the_internet_percent_of_population") |>
  filter(year == 2020) |>
  arrange(desc(share_using_net)) |>
  top_n(5) |>
  ggplot(aes(x=reorder(country, share_using_net), y = share_using_net, fill = country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = country),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Share of the population using the Internet (%)",
       title = "African countries with the highest share of\nthe population using the Internet (%)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_8_ict_owid/images/share_net_top_5.png", width = 12, height = 12, dpi = 300)

# 3) Bottom 5 African Countries (Internet Access)

share_net_clean_africa |> 
  rename(share_using_net = "individuals_using_the_internet_percent_of_population") |>
  filter(year == 2020) |>
  arrange(desc(share_using_net)) |>
  top_n(-5) |>
  ggplot(aes(x=reorder(country, share_using_net), y = share_using_net, fill = country)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_color_brewer(palette = "Set3") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.02))) +
  geom_text(aes(y = 0.02, label = country),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  labs(x = "Country",
       y = "Share of the population using the Internet (%)",
       title = "African countries with the lowest share of\nthe population using the Internet (%)",
       subtitle = "",
       caption = "Data Source: Our World In Data") +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        axis.ticks.minor.x = element_line(color = "black", size = 2),  # Show minor ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_8_ict_owid/images/share_net_bottom_5.png", width = 12, height = 12, dpi = 300)

# 4) Map of countries showing share using internet between 1990 and 2020

#########.............

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# Get 1990 data
share_net_clean_africa_1990 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(share_net_use))

# Now we have the 1990 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_1990_full_join <- full_join(africa, 
                                                   share_net_clean_africa_1990,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_1990_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_1990, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_1990

share_net_clean_africa_1990_anti_join_2 <- anti_join(share_net_clean_africa_1990, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_1990_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_1990.png", width = 9, height = 16, dpi = 300)

# Get 1995 data
share_net_clean_africa_1995 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 1995) |>
  arrange(desc(share_net_use))

# Now we have the 1995 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_1995_full_join <- full_join(africa, 
                                                   share_net_clean_africa_1995,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_1995_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_1995, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_1995

share_net_clean_africa_1995_anti_join_2 <- anti_join(share_net_clean_africa_1995, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p2 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_1995_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "1995",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_1995.png", width = 9, height = 16, dpi = 300)

# Get 2000 data
share_net_clean_africa_2000 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 2000) |>
  arrange(desc(share_net_use))

# Now we have the 2000 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_2000_full_join <- full_join(africa, 
                                                   share_net_clean_africa_2000,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_2000_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_2000, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_2020

share_net_clean_africa_2000_anti_join_2 <- anti_join(share_net_clean_africa_2000, 
                                                     africa, 
                                                     by = c("country" = "admin"))


p3 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_2000_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "")

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_2000.png", width = 9, height = 16, dpi = 300)

# Get 2005 data
share_net_clean_africa_2005 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 2005) |>
  arrange(desc(share_net_use))

# Now we have the 2005 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_2005_full_join <- full_join(africa, 
                                                   share_net_clean_africa_2005,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_2005_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_2005, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_2005

share_net_clean_africa_2005_anti_join_2 <- anti_join(share_net_clean_africa_2005, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p4 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_2005_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2005",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_2005.png", width = 9, height = 16, dpi = 300)

# Get 2010 data
share_net_clean_africa_2010 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 2010) |>
  arrange(desc(share_net_use))

# Now we have the 2010 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_2010_full_join <- full_join(africa, 
                                                   share_net_clean_africa_2010,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_2010_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_2010, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_2010

share_net_clean_africa_2010_anti_join_2 <- anti_join(share_net_clean_africa_2010, 
                                                     africa, 
                                                     by = c("country" = "admin"))


p5 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_2010_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       ))+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "")

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_2010.png", width = 9, height = 16, dpi = 300)

# Get 2015 data
share_net_clean_africa_2015 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 2015) |>
  arrange(desc(share_net_use))

# Now we have the 2015 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_2015_full_join <- full_join(africa, 
                                                   share_net_clean_africa_2015,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_2015_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_2015, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_2015

share_net_clean_africa_2015_anti_join_2 <- anti_join(share_net_clean_africa_2015, 
                                                     africa, 
                                                     by = c("country" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p6 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_2015_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
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
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2015",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_2015.png", width = 9, height = 16, dpi = 300)


# Get 2020 data
share_net_clean_africa_2020 <- share_net_clean_africa_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(share_net_use))

# Now we have the 2020 dataset and the africa dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from africa

share_net_clean_africa_2020_full_join <- full_join(africa, 
                                                   share_net_clean_africa_2020,
                                                   by = c("admin" = "country"))

# Find rows only in africa

share_net_clean_africa_2020_anti_join_1 <- anti_join(africa, 
                                                     share_net_clean_africa_2020, 
                                                     by = c("admin" = "country"))

# Find rows only in share_net_clean_africa_2020

share_net_clean_africa_2020_anti_join_2 <- anti_join(share_net_clean_africa_2020, 
                                                     africa, 
                                                     by = c("country" = "admin"))


p7 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_net_clean_africa_2020_full_join, aes(fill = share_net_use), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       #labels = scales::percent_format(accuracy = 1),
                       name = "Percentage (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       ))+
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 150, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom",
    legend.key.height = unit(1, 'cm'), #change legend key height,
    legend.key.width = unit(1, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "")

ggsave("sub_pro_8_ict_owid/images/share_net_clean_africa_map_2020.png", width = 9, height = 16, dpi = 300)

######################

######################

# Create the top and bottom 3 share of internet users in 2020

# Morocco / Seychelles / Egypt (%)

# Burundi / South Sudan / Uganda (%) Only 43 countries listed...

share_net_clean_africa_top_3 <- share_net_clean_africa |>
  filter(country %in% c("Morocco", "Seychelles", "Egypt")) |>
  filter(year %in% c(1990:2020))

share_net_clean_africa_bottom_3 <- share_net_clean_africa |>
  filter(country %in% c("Burundi", "South Sudan", "Uganda")) |>
  filter(year %in% c(1990:2020))

# EDA Plots

# Top 3

share_net_clean_africa_top_3 %>%
  ggplot(aes(x = year, 
             y = individuals_using_the_internet_percent_of_population, 
             color = country)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "",
       title = "Top 3",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  # scale_color_manual(values = c("darkred", "navy", "darkred", "navy", "darkgreen", "darkgreen")) +# figure out what the order does
  theme_classic() +
  # geom_text_repel(data = ict_per_100_clean_ssa_long_label_5,
  #                 aes(label = connection_type), 
  #                 nudge_x = 0.5,
  #                 nudge_y = 0.5,
  #                 size = 7) +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust = 0.5),
        plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

# ggsave("sub_pro_8_ict_owid/images/communication_ssa.png", width = 12, height = 12, dpi = 300)


# Bottom 3

share_net_clean_africa_bottom_3 %>%
  ggplot(aes(x = year, 
             y = individuals_using_the_internet_percent_of_population, 
             color = country)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "",
       title = "Bottom 3",
       subtitle = "",
       caption = "Data Source: Our World in Data") +
  # scale_color_manual(values = c("darkred", "navy", "darkred", "navy", "darkgreen", "darkgreen")) +# figure out what the order does
  theme_classic() +
  # geom_text_repel(data = ict_per_100_clean_ssa_long_label_5,
  #                 aes(label = connection_type), 
  #                 nudge_x = 0.5,
  #                 nudge_y = 0.5,
  #                 size = 7) +
  theme(axis.title.x =element_text(size = 28, vjust = 0, face = "bold"),
        axis.title.y =element_text(size = 28,  vjust = 2, face = "bold"),
        axis.text.x = element_text(size = 24, face = "bold", colour = "#000000"),
        axis.text.y = element_text(size = 24, face = "bold", colour = "#000000"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust = 0.5),
        plot.subtitle.position = "plot",
        plot.caption = element_text(family = "Helvetica",size = 20, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.background = element_rect("bisque1"),
        legend.position = "none") 

# ggsave("sub_pro_8_ict_owid/images/communication_ssa.png", width = 12, height = 12, dpi = 300)

