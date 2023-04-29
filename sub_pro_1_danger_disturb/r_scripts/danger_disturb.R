## Disturbed and Dangerous Counties in Kenya
## Crime and Livestock Stats for the Counties

# 1) Load the packages required for the maps

# Solve package loading issues with options(timeout = 600) 
# increase download length time

#install.packages("sf")
library(sf) # simple features
library(tidyverse)
library(ggplot2)
library(ggrepel)
#install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(rKenyaCensus)
library(patchwork)
library(janitor)
#install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(ggthemes)
library(scales)
# install.packages("treemapify")
library(treemapify)

# 2) Map of the disturbed counties

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census, KenyaCounties_SHP

# a) Sample plot of the map of Kenya

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

p0 <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = "bisque2", linewidth = 1, color = "black") + 
  theme_void()

p0

ggsave("sub_pro_1_danger_disturb/images/kenya_map_1.png", width = 11.25, height = 11.25, dpi =600)


# b) Dangerous and disturbed counties in Kenya

# Remove the "/"

kenya_counties_sf$County <- gsub("/", 
                                 " ", 
                                 kenya_counties_sf$County)

# c) Highlight the required area

# Select counties to highlight
highlight_counties <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# Filter the states dataset to only include the highlight states
highlighted <- kenya_counties_sf %>% filter(County %in% highlight_counties)


p1 <- ggplot() + 
  geom_sf(data = kenya_counties_sf, fill = "bisque2", linewidth = 1, color = "black") + 
  geom_sf(data  = highlighted, fill = "chocolate4", linewidth = 1, color = "black") +
  theme_void()
p1

ggsave("sub_pro_1_danger_disturb/images/kenya_map_2.png", width = 11.25, height = 11.25, dpi =600)


# create a ggplot2 plot with the states and the highlighted states
p2 <- ggplot(data = highlighted) +
  geom_sf(aes(fill = County), linewidth = 1, show.legend = FALSE) +
  geom_sf_label_repel(aes(label = County), size = 10) +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "",
       caption = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12)) +
  theme_void() 
p2

ggsave("sub_pro_1_danger_disturb/images/county_map_1.png", width = 11.25, height = 11.25, dpi =600)


p1 + 
  p2 + 
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
                                plot.background = element_rect(fill = "bisque1"))) &
  theme(text = element_text('Helvetica'))

ggsave("sub_pro_1_danger_disturb/images/county_map_combi.png", width = 11.25, height = 11.25, dpi =600)


# d) Table with the land area
# Review this section to see how to style and save the image
# Require the webshot::install_phantomjs() to install package
# also include the magick package

livestock_area_county %>%
  select(county, land_area_in_sq_km) %>%
  mutate(county = str_to_title(county)) %>%
  arrange(desc(land_area_in_sq_km)) %>%
  adorn_totals("row") %>%
  rename("County" = "county",
         "Land Area (sq. km)" = "land_area_in_sq_km") %>%
  kbl(align = "c") %>%
  kable_classic() %>% 
  row_spec(row = 0, font_size = 28, color = "white", background = "#000000") %>%
  row_spec(row = c(1:7), font_size = 20) %>%
  row_spec(row = 6, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(row = 7, bold = T) %>%
  save_kable(file = "sub_pro_1_danger_disturb/images/area_table.png",
             zoom = 5)

# 3) Generate the various dataframes required for analysis

# a) View the data available in the data catalogue

data("DataCatalogue")

# b) Load the required data

# First, list the "Dangerous and disturbed" counties
dan_dist <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# Then, select the livestock data 
df_livestock <- V4_T2.24
livestock <- df_livestock[2:393,]
livestock <- livestock %>%
  clean_names()

# Remove the "/" from the county names
livestock$county <- gsub("/", " ", livestock$county)

# Create a pastoralist livestock dataframe with
# new variables for animals per household

livestock_select <- livestock %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) %>%
  mutate(pasto_livestock = sheep + goats + indigenous_cattle) %>%
  mutate(ind_cattle_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_household = round(goats/farming)) %>%
  mutate(sheep_household = round(sheep/farming)) %>%
  mutate(pasto_livestock_household = round(pasto_livestock/farming))

# Get data for the selected "disturbed and dangerous" counties

livestock_select_county <- livestock_select %>%
  filter(admin_area == "County") %>%
  filter(county %in% dan_dist)

# Get subcounty data for the "disturbed and dangerous" counties
livestock_select_subcounty <- livestock_select %>%
  filter(admin_area == "SubCounty") %>%
  filter(county %in% dan_dist)

# Next, get the area data
df_land_area <- V1_T2.7
land_area <- df_land_area[2:396,]
land_area <- land_area %>%
  clean_names()

# Remove the "/"
land_area$county <- gsub("/", " ", land_area$county)
land_area$county <- gsub(" County", "", land_area$county)
land_area$county <- toupper(land_area$county)
land_area$sub_county <- toupper(land_area$sub_county)


# Obtain the area data for "disturbed and dangerous" counties
land_area_county <- land_area %>%
  filter(admin_area == "County") %>%
  select(county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist)

# Get the subcounty area data for "disturbed and dangerous" counties
land_area_subcounty <- land_area %>%
  filter(admin_area == "SubCounty") %>%
  select(county, sub_county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist) %>%
  select(-county)

################### Final datasets used for the analysis#############

###### Get county data (area and livestock) for the disturbed and dangerous regions

livestock_area_county <- inner_join(livestock_select_county, land_area_county, by = "county")

livestock_area_county <- livestock_area_county %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# Get subcounty data (area and livestock) for the disturbed and dangerous regions

livestock_area_subcounty <- inner_join(livestock_select_subcounty, land_area_subcounty, by = "sub_county")

livestock_area_subcounty <- livestock_area_subcounty %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# 4) Plots of relevant graphs (EDA)

# a) Farming Households

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, farming), y = farming, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_1_danger_disturb/images/county_farm_house_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, farming), y = farming, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top") 

ggsave("sub_pro_1_danger_disturb/images/subcounty_farm_house_1.png", width = 11.25, height = 11.25, dpi = 600)

# b) Pastoral Livestock

#label = paste(county, comma(pasto_livestock), sep = "\n")

ggplot(livestock_area_county, aes(area = pasto_livestock, fill = county, label = comma(pasto_livestock)
                              )) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 24) +
  scale_fill_brewer(palette = "OrRd") +
  labs(x = "",
       y = "",
       title = "",
       caption = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 28),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "bottom") 

ggsave("sub_pro_1_danger_disturb/images/county_past_live_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Pastoral Livestock",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_1_danger_disturb/images/county_past_live_2.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "Subcounty",
       y = "Number of Pastoral Livestock",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top")

ggsave("sub_pro_1_danger_disturb/images/subcounty_past_live_1.png", width = 11.25, height = 11.25, dpi = 600)

# c) Pastoral Livestock per household

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Pastoral Livestock per household",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none")

ggsave("sub_pro_1_danger_disturb/images/county_past_live_house_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "Subcounty",
       y = "Number of Pastoral Livestock per household",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top")

ggsave("sub_pro_1_danger_disturb/images/subcounty_past_live_house_1.png", width = 11.25, height = 11.25, dpi = 600)