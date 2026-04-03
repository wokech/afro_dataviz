# Share of Women in Parliament

# A) Load the required libraries and set up data

# Load libraries
library(tidyverse)
library(janitor)
library(viridis)
#library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Also ensure that rnatural hi res is installed
library(ggrepel)
library(scales)
library(gghighlight)

# Load data
#share_women_parliament <- read_csv("https://ourworldindata.org/grapher/share-of-women-in-parliament.csv?v=1&csvType=full&useColumnShortNames=true")

# Save data
#write_csv(share_women_parliament, "sub_pro_19_politics/datasets/share_women_parliament.csv")

# Load data again
share_women_parliament <- read_csv("sub_pro_19_politics/datasets/share_women_parliament.csv")

# Only include African Countries

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
                       "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                       "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                       "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", 
                       "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
                       "Uganda", "Zambia", "Zimbabwe")

share_women_parliament_select_africa <- share_women_parliament %>%
  clean_names() %>%
  filter(entity %in% african_countries)

#############
# Check if the values in the african_countries dataset are present in new dataframes

african_countries[!(african_countries %in% unique(share_women_parliament_select_africa$entity))]

#############

# Change names to allow for mapping

share_women_parliament_select_africa_rnaturalearth <- share_women_parliament_select_africa %>%
  mutate(entity = case_when(
    entity == "Cape Verde"  ~ "Cabo Verde",
    entity == "Sao Tome and Principe"  ~ "São Tomé and Principe",
    entity == "Eswatini"  ~ "eSwatini",
    entity == "Democratic Republic of Congo"  ~ "Democratic Republic of the Congo",
    entity == "Tanzania"  ~ "United Republic of Tanzania",
    entity == "Congo"  ~ "Republic of the Congo",
    entity == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ entity  # Retain original name if none of the conditions are met
  ))


# 2) Map of countries showing percentage forest cover (%)

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))


# Get 1990 data

share_women_parliament_select_africa_1990 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 1990) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 1990 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_1990_full_join <- full_join(africa, 
                                                                share_women_parliament_select_africa_1990,
                                                                by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_1990_anti_join <- anti_join(africa, 
                                                                share_women_parliament_select_africa_1990,
                                                                by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_1990_anti_join_2 <- anti_join(share_women_parliament_select_africa_1990,
                                                                   africa,
                                                                  by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_1990_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1990",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_1990.png", width = 9, height = 16, dpi = 300)


# Get 1995 data

share_women_parliament_select_africa_1995 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 1995) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 1995 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_1995_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_1995,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_1995_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_1995,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_1995_anti_join_2 <- anti_join(share_women_parliament_select_africa_1995,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_1995_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "1995",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_1995.png", width = 9, height = 16, dpi = 300)


# Get 2000 data

share_women_parliament_select_africa_2000 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 2000) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 2000 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_2000_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_2000,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2000_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_2000,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2000_anti_join_2 <- anti_join(share_women_parliament_select_africa_2000,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_2000_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2000",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_2000.png", width = 9, height = 16, dpi = 300)


# Get 2005 data

share_women_parliament_select_africa_2005 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 2005) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 2005 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_2005_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_2005,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2005_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_2005,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2005_anti_join_2 <- anti_join(share_women_parliament_select_africa_2005,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_2005_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2005",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_2005.png", width = 9, height = 16, dpi = 300)


# Get 2010 data

share_women_parliament_select_africa_2010 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 2010) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 2010 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_2010_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_2010,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2010_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_2010,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2010_anti_join_2 <- anti_join(share_women_parliament_select_africa_2010,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_2010_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2010",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_2010.png", width = 9, height = 16, dpi = 300)



# Get 2015 data

share_women_parliament_select_africa_2015 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 2015) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 2015 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_2015_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_2015,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2015_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_2015,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2015_anti_join_2 <- anti_join(share_women_parliament_select_africa_2015,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_2015_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2015",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_2015.png", width = 9, height = 16, dpi = 300)


# Get 2020 data

share_women_parliament_select_africa_2020 <- share_women_parliament_select_africa_rnaturalearth |> 
  filter(year == 2020) |>
  arrange(desc(wom_parl_vdem_estimate_best))

# Now we have the 2020 dataset and the caribbean dataset.
# These two need to be joined together.

# Identify rows that don't match

# Left join to keep all rows from caribbean

share_women_parliament_select_africa_2020_full_join <- full_join(africa, 
                                                                 share_women_parliament_select_africa_2020,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2020_anti_join <- anti_join(africa, 
                                                                 share_women_parliament_select_africa_2020,
                                                                 by = c("admin" = "entity"))

# Find missing

share_women_parliament_select_africa_2020_anti_join_2 <- anti_join(share_women_parliament_select_africa_2020,
                                                                   africa,
                                                                   by = c("entity" = "admin"))

###############
# As you plot the different years, remember that not all years had all countries measured
###############

p1 <- ggplot(data = africa) +
  geom_sf() + 
  geom_sf(data = share_women_parliament_select_africa_2020_full_join, aes(fill = wom_parl_vdem_estimate_best), linewidth = 1) +
  scale_fill_distiller(palette = "YlGnBu", 
                       direction = 1,
                       limits = c(0, 100),
                       name = "Percent share (%)",
                       guide = guide_colorbar(     # Adjustments specific to continuous scales
                         title.position = "top",   # Position the title ('top', 'bottom', 'left', 'right')
                         title.hjust = 0.5         # Center the title horizontally) 
                       )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    plot.title = element_text(family="Helvetica", face="bold", size = 600, hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_text(family="Helvetica", face="bold", size = 26, hjust = 0.5),
    plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0, vjust = 1),
    legend.title = element_text(size = 100),
    legend.text = element_text(size = 100, vjust = 0.5, hjust = 0.75),
    legend.position = "bottom",
    legend.key.height = unit(2, 'cm'), #change legend key height,
    legend.key.width = unit(2, 'cm'), #change legend key width
  ) +
  labs(title = "2020",
       subtitle = "",
       caption = "") 

ggsave("sub_pro_19_politics/images/share_women_parliament_series/share_women_parliament_select_africa_2020.png", width = 9, height = 16, dpi = 300)


