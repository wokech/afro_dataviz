# African Single Countries with Maps

# Load the required libraries and packages

library(tidyverse)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data
#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps 
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)
library(ggrepel)
library(ggsflabel)
library(ggsci)
library(ggthemes)
library(viridis)
library(hrbrthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)


# Only include African Countries

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                       "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", 
                       "Chad", "Comoros", "Democratic Republic of the Congo", 
                       "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
                       "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
                       "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
                       "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                       "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", "Sao Tome and Principe", 
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
                       "South Sudan", "Sudan", "Togo", "Tunisia", 
                       "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe")

### HOW MANY COUNTRIES ARE THERE IN AFRICA ###

# 2) Get country data for mapping

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

# Map of the whole of Africa

ggplot(data = africa) +
  geom_sf(fill = "goldenrod2", linewidth = 0.5) +
  theme_void()

# Capital Cities

# Read in the data
capital_cities <- read_excel("sub_pro_0_tutorials_tools/single_countries/datasets/africa_countries_capitals.xlsx")
# trim whitespaces and clean the names
capital_cities_split <- capital_cities |>
  mutate(across(everything(), ~trimws(.))) |>
  clean_names()
# transform to sf data
capital_cities_sf <- st_as_sf(capital_cities_split, coords = c("longitude", "latitude"), crs = 4326)
# transform to match the shapefile CRS
capital_cities_sf <- st_transform(capital_cities_sf, st_crs(africa))


# 1) Algeria

algeria <- c("Algeria")
algeria_df <- africa |> filter(admin == "Algeria")
capital_cities_algeria_sf <- capital_cities_sf |> filter(country %in% algeria)

map_algeria <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% algeria_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_algeria

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/algeria_map.png", width = 12, height = 12, dpi = 300)

map_algeria_zoom <- ggplot(data = algeria_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_algeria_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_algeria_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/algeria_map_zoom.png", width = 12, height = 12, dpi = 300)

# 2) Angola

angola <- c("Angola")
angola_df <- africa |> filter(admin == "Angola")
capital_cities_angola_sf <- capital_cities_sf |> filter(country %in% angola)

map_angola <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% angola_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_angola

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/angola_map.png", width = 12, height = 12, dpi = 300)

map_angola_zoom <- ggplot(data = angola_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_angola_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_angola_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/angola_map_zoom.png", width = 12, height = 12, dpi = 300)

# 3) Benin

benin <- c("Benin")
benin_df <- africa |> filter(admin == "Benin")
capital_cities_benin_sf <- capital_cities_sf |> filter(country %in% benin)

map_benin <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% benin_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_benin

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/benin_map.png", width = 12, height = 12, dpi = 300)

map_benin_zoom <- ggplot(data = benin_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_benin_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_benin_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/benin_map_zoom.png", width = 12, height = 12, dpi = 300)

# 4) Botswana

botswana <- c("Botswana")
botswana_df <- africa |> filter(admin == "Botswana")
capital_cities_botswana_sf <- capital_cities_sf |> filter(country %in% botswana)

map_botswana <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% botswana_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_botswana

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/botswana_map.png", width = 12, height = 12, dpi = 300)

map_botswana_zoom <- ggplot(data = botswana_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_botswana_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_botswana_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/botswana_map_zoom.png", width = 12, height = 12, dpi = 300)

# 5) Burkina Faso

burkina_faso <- c("Burkina Faso")
burkina_faso_df <- africa |> filter(admin == "Burkina Faso")
capital_cities_burkina_faso_sf <- capital_cities_sf |> filter(country %in% burkina_faso)

map_burkina_faso <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% burkina_faso_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_burkina_faso

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burkina_faso_map.png", width = 12, height = 12, dpi = 300)

map_burkina_faso_zoom <- ggplot(data = burkina_faso_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_burkina_faso_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_burkina_faso_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burkina_faso_map_zoom.png", width = 12, height = 12, dpi = 300)

# 6) Burundi

burundi <- c("Burundi")
burundi_df <- africa |> filter(admin == "Burundi")
capital_cities_burundi_sf <- capital_cities_sf |> filter(country %in% burundi)

map_burundi <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% burundi_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_burundi

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burundi_map.png", width = 12, height = 12, dpi = 300)

map_burundi_zoom <- ggplot(data = burundi_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_burundi_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_burundi_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burundi_map_zoom.png", width = 12, height = 12, dpi = 300)

# 7) Cabo Verde

cabo_verde <- c("Cabo Verde")
cabo_verde_df <- africa |> filter(admin == "Cabo Verde")
capital_cities_cabo_verde_sf <- capital_cities_sf |> filter(country %in% cabo_verde)

map_cabo_verde <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% cabo_verde_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cabo_verde

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cabo_verde_map.png", width = 12, height = 12, dpi = 300)

map_cabo_verde_zoom <- ggplot(data = cabo_verde_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_cabo_verde_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cabo_verde_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cabo_verde_map_zoom.png", width = 12, height = 12, dpi = 300)


# 8) Cameroon

cameroon <- c("Cameroon")
cameroon_df <- africa |> filter(admin == "Cameroon")
capital_cities_cameroon_sf <- capital_cities_sf |> filter(country %in% cameroon)

map_cameroon <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% cameroon_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cameroon

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cameroon_map.png", width = 12, height = 12, dpi = 300)

map_cameroon_zoom <- ggplot(data = cameroon_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_cameroon_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_cameroon_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cameroon_map_zoom.png", width = 12, height = 12, dpi = 300)


# 9) Central African Republic

car <- c("Central African Republic")
car_df <- africa |> filter(admin == "Central African Republic")
capital_cities_car_sf <- capital_cities_sf |> filter(country %in% car)

map_car <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% car_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_car

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/car_map.png", width = 12, height = 12, dpi = 300)

map_car_zoom <- ggplot(data = car_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_car_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_car_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/car_map_zoom.png", width = 12, height = 12, dpi = 300)


# 10) Chad

chad <- c("Chad")
chad_df <- africa |> filter(admin == "Chad")
capital_cities_chad_sf <- capital_cities_sf |> filter(country %in% chad)

map_chad <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% chad_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_chad

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/chad_map.png", width = 12, height = 12, dpi = 300)

map_chad_zoom <- ggplot(data = chad_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_chad_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_chad_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/chad_map_zoom.png", width = 12, height = 12, dpi = 300)


# 11) Comoros

comoros <- c("Comoros")
comoros_df <- africa |> filter(admin == "Comoros")
capital_cities_comoros_sf <- capital_cities_sf |> filter(country %in% comoros)

map_comoros <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% comoros_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_comoros

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/comoros_map.png", width = 12, height = 12, dpi = 300)

map_comoros_zoom <- ggplot(data = comoros_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_comoros_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_comoros_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/comoros_map_zoom.png", width = 12, height = 12, dpi = 300)


# 12) Democratic Republic of the Congo

drc <- c("Democratic Republic of the Congo")
drc_df <- africa |> filter(admin == "Democratic Republic of the Congo")
capital_cities_drc_sf <- capital_cities_sf |> filter(country %in% drc)

map_drc <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% drc_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_drc

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/drc_map.png", width = 12, height = 12, dpi = 300)

map_drc_zoom <- ggplot(data = drc_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_drc_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_drc_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/drc_map_zoom.png", width = 12, height = 12, dpi = 300)

# 13) Djibouti

djibouti <- c("Djibouti")
djibouti_df <- africa |> filter(admin == "Djibouti")
capital_cities_djibouti_sf <- capital_cities_sf |> filter(country %in% djibouti)

map_djibouti <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% djibouti_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_djibouti

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/djibouti_map.png", width = 12, height = 12, dpi = 300)

map_djibouti_zoom <- ggplot(data = djibouti_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_djibouti_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_djibouti_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/djibouti_map_zoom.png", width = 12, height = 12, dpi = 300)


# 14) Egypt

egypt <- c("Egypt")
egypt_df <- africa |> filter(admin == "Egypt")
capital_cities_egypt_sf <- capital_cities_sf |> filter(country %in% egypt)

map_egypt <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% egypt_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_egypt

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/egypt_map.png", width = 12, height = 12, dpi = 300)

map_egypt_zoom <- ggplot(data = egypt_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_egypt_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_egypt_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/egypt_map_zoom.png", width = 12, height = 12, dpi = 300)


# 15) Equatorial Guinea

eq_gui <- c("Equatorial Guinea")
eq_gui_df <- africa |> filter(admin == "Equatorial Guinea")
capital_cities_eq_gui_sf <- capital_cities_sf |> filter(country %in% eq_gui)

map_eq_gui <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% eq_gui_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eq_gui

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eq_gui_map.png", width = 12, height = 12, dpi = 300)

map_eq_gui_zoom <- ggplot(data = eq_gui_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_eq_gui_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eq_gui_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eq_gui_map_zoom.png", width = 12, height = 12, dpi = 300)

# 16) Eritrea

eritrea <- c("Eritrea")
eritrea_df <- africa |> filter(admin == "Eritrea")
capital_cities_eritrea_sf <- capital_cities_sf |> filter(country %in% eritrea)

map_eritrea <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% eritrea_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eritrea

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eritrea_map.png", width = 12, height = 12, dpi = 300)

map_eritrea_zoom <- ggplot(data = eritrea_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_eritrea_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eritrea_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eritrea_map_zoom.png", width = 12, height = 12, dpi = 300)

# 17) eSwatini

eswatini <- c("eSwatini")
eswatini_df <- africa |> filter(admin == "eSwatini")
capital_cities_eswatini_sf <- capital_cities_sf |> filter(country %in% eswatini)

map_eswatini <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% eswatini_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eswatini

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eswatini_map.png", width = 12, height = 12, dpi = 300)

map_eswatini_zoom <- ggplot(data = eswatini_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_eswatini_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_eswatini_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eswatini_map_zoom.png", width = 12, height = 12, dpi = 300)

# 18) Ethiopia

ethiopia <- c("Ethiopia")
ethiopia_df <- africa |> filter(admin == "Ethiopia")
capital_cities_ethiopia_sf <- capital_cities_sf |> filter(country %in% ethiopia)

map_ethiopia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% ethiopia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ethiopia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ethiopia_map.png", width = 12, height = 12, dpi = 300)

map_ethiopia_zoom <- ggplot(data = ethiopia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_ethiopia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ethiopia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ethiopia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 19) Gabon

gabon <- c("Gabon")
gabon_df <- africa |> filter(admin == "Gabon")
capital_cities_gabon_sf <- capital_cities_sf |> filter(country %in% gabon)

map_gabon <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% gabon_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_gabon

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/gabon_map.png", width = 12, height = 12, dpi = 300)

map_gabon_zoom <- ggplot(data = gabon_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_gabon_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_gabon_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/gabon_map_zoom.png", width = 12, height = 12, dpi = 300)

# 20) Gambia

gambia <- c("Gambia")
gambia_df <- africa |> filter(admin == "Gambia")
capital_cities_gambia_sf <- capital_cities_sf |> filter(country %in% gambia)

map_gambia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% gambia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_gambia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/gambia_map.png", width = 12, height = 12, dpi = 300)

map_gambia_zoom <- ggplot(data = gambia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_gambia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_gambia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/gambia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 21) Ghana

ghana <- c("Ghana")
ghana_df <- africa |> filter(admin == "Ghana")
capital_cities_ghana_sf <- capital_cities_sf |> filter(country %in% ghana)

map_ghana <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% ghana_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ghana

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ghana_map.png", width = 12, height = 12, dpi = 300)

map_ghana_zoom <- ggplot(data = ghana_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_ghana_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ghana_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ghana_map_zoom.png", width = 12, height = 12, dpi = 300)

# 22) Guinea

guinea <- c("Guinea")
guinea_df <- africa |> filter(admin == "Guinea")
capital_cities_guinea_sf <- capital_cities_sf |> filter(country %in% guinea)

map_guinea <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% guinea_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guinea

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/guinea_map.png", width = 12, height = 12, dpi = 300)

map_guinea_zoom <- ggplot(data = guinea_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_guinea_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guinea_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/guinea_map_zoom.png", width = 12, height = 12, dpi = 300)

# 23) Guinea-Bissau

guinea_bissau <- c("Guinea-Bissau")
guinea_bissau_df <- africa |> filter(admin == "Guinea-Bissau")
capital_cities_guinea_bissau_sf <- capital_cities_sf |> filter(country %in% guinea_bissau)

map_guinea_bissau <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% guinea_bissau_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guinea_bissau

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/guinea_bissau_map.png", width = 12, height = 12, dpi = 300)

map_guinea_bissau_zoom <- ggplot(data = guinea_bissau_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_guinea_bissau_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_guinea_bissau_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/guinea_bissau_map_zoom.png", width = 12, height = 12, dpi = 300)

# 24) Ivory Coast

ivory_coast <- c("Ivory Coast")
ivory_coast_df <- africa |> filter(admin == "Ivory Coast")
capital_cities_ivory_coast_sf <- capital_cities_sf |> filter(country %in% ivory_coast)

map_ivory_coast <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% ivory_coast_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ivory_coast

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ivory_coast_map.png", width = 12, height = 12, dpi = 300)

map_ivory_coast_zoom <- ggplot(data = ivory_coast_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_ivory_coast_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ivory_coast_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ivory_coast_map_zoom.png", width = 12, height = 12, dpi = 300)

# 25) Kenya

kenya <- c("Kenya")
kenya_df <- africa |> filter(admin == "Kenya")
capital_cities_kenya_sf <- capital_cities_sf |> filter(country %in% kenya)

map_kenya <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% kenya_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kenya

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/kenya_map.png", width = 12, height = 12, dpi = 300)

map_kenya_zoom <- ggplot(data = kenya_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_kenya_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_kenya_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/kenya_map_zoom.png", width = 12, height = 12, dpi = 300)

# 26) Lesotho

lesotho <- c("Lesotho")
lesotho_df <- africa |> filter(admin == "Lesotho")
capital_cities_lesotho_sf <- capital_cities_sf |> filter(country %in% lesotho)

map_lesotho <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% lesotho_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_lesotho

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/lesotho_map.png", width = 12, height = 12, dpi = 300)

map_lesotho_zoom <- ggplot(data = lesotho_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_lesotho_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_lesotho_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/lesotho_map_zoom.png", width = 12, height = 12, dpi = 300)

# 27) Liberia

liberia <- c("Liberia")
liberia_df <- africa |> filter(admin == "Liberia")
capital_cities_liberia_sf <- capital_cities_sf |> filter(country %in% liberia)

map_liberia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% liberia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_liberia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/liberia_map.png", width = 12, height = 12, dpi = 300)

map_liberia_zoom <- ggplot(data = liberia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_liberia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_liberia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/liberia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 28) Libya

libya <- c("Libya")
libya_df <- africa |> filter(admin == "Libya")
capital_cities_libya_sf <- capital_cities_sf |> filter(country %in% libya)

map_libya <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% libya_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_libya

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/libya_map.png", width = 12, height = 12, dpi = 300)

map_libya_zoom <- ggplot(data = libya_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_libya_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_libya_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/libya_map_zoom.png", width = 12, height = 12, dpi = 300)

# 29) Madagascar

madagascar <- c("Madagascar")
madagascar_df <- africa |> filter(admin == "Madagascar")
capital_cities_madagascar_sf <- capital_cities_sf |> filter(country %in% madagascar)

map_madagascar <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% madagascar_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_madagascar

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/madagascar_map.png", width = 12, height = 12, dpi = 300)

map_madagascar_zoom <- ggplot(data = madagascar_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_madagascar_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_madagascar_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/madagascar_map_zoom.png", width = 12, height = 12, dpi = 300)

# 30) Malawi

malawi <- c("Malawi")
malawi_df <- africa |> filter(admin == "Malawi")
capital_cities_malawi_sf <- capital_cities_sf |> filter(country %in% malawi)

map_malawi <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% malawi_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_malawi

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/malawi_map.png", width = 12, height = 12, dpi = 300)

map_malawi_zoom <- ggplot(data = malawi_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_malawi_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_malawi_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/malawi_map_zoom.png", width = 12, height = 12, dpi = 300)

# 31) Mali

mali <- c("Mali")
mali_df <- africa |> filter(admin == "Mali")
capital_cities_mali_sf <- capital_cities_sf |> filter(country %in% mali)

map_mali <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% mali_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mali

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mali_map.png", width = 12, height = 12, dpi = 300)

map_mali_zoom <- ggplot(data = mali_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_mali_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mali_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mali_map_zoom.png", width = 12, height = 12, dpi = 300)

# 32) Mauritania

mauritania <- c("Mauritania")
mauritania_df <- africa |> filter(admin == "Mauritania")
capital_cities_mauritania_sf <- capital_cities_sf |> filter(country %in% mauritania)

map_mauritania <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% mauritania_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mauritania

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mauritania_map.png", width = 12, height = 12, dpi = 300)

map_mauritania_zoom <- ggplot(data = mauritania_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_mauritania_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mauritania_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mauritania_map_zoom.png", width = 12, height = 12, dpi = 300)

# 33) Mauritius

mauritius <- c("Mauritius")
mauritius_df <- africa |> filter(admin == "Mauritius")
capital_cities_mauritius_sf <- capital_cities_sf |> filter(country %in% mauritius)

map_mauritius <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% mauritius_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mauritius

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mauritius_map.png", width = 12, height = 12, dpi = 300)

map_mauritius_zoom <- ggplot(data = mauritius_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_mauritius_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mauritius_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mauritius_map_zoom.png", width = 12, height = 12, dpi = 300)

# 34) Morocco

morocco <- c("Morocco")
morocco_df <- africa |> filter(admin == "Morocco")
capital_cities_morocco_sf <- capital_cities_sf |> filter(country %in% morocco)

map_morocco <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% morocco_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_morocco

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/morocco_map.png", width = 12, height = 12, dpi = 300)

map_morocco_zoom <- ggplot(data = morocco_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_morocco_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_morocco_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/morocco_map_zoom.png", width = 12, height = 12, dpi = 300)

# 35) Mozambique

mozambique <- c("Mozambique")
mozambique_df <- africa |> filter(admin == "Mozambique")
capital_cities_mozambique_sf <- capital_cities_sf |> filter(country %in% mozambique)

map_mozambique <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% mozambique_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mozambique

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mozambique_map.png", width = 12, height = 12, dpi = 300)

map_mozambique_zoom <- ggplot(data = mozambique_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_mozambique_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_mozambique_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/mozambique_map_zoom.png", width = 12, height = 12, dpi = 300)

# 36) Namibia

namibia <- c("Namibia")
namibia_df <- africa |> filter(admin == "Namibia")
capital_cities_namibia_sf <- capital_cities_sf |> filter(country %in% namibia)

map_namibia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% namibia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_namibia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/namibia_map.png", width = 12, height = 12, dpi = 300)

map_namibia_zoom <- ggplot(data = namibia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_namibia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_namibia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/namibia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 37) Niger

niger <- c("Niger")
niger_df <- africa |> filter(admin == "Niger")
capital_cities_niger_sf <- capital_cities_sf |> filter(country %in% niger)

map_niger <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% niger_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_niger

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/niger_map.png", width = 12, height = 12, dpi = 300)

map_niger_zoom <- ggplot(data = niger_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_niger_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_niger_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/niger_map_zoom.png", width = 12, height = 12, dpi = 300)

# 38) Nigeria

nigeria <- c("Nigeria")
nigeria_df <- africa |> filter(admin == "Nigeria")
capital_cities_nigeria_sf <- capital_cities_sf |> filter(country %in% nigeria)

map_nigeria <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% nigeria_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nigeria

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/nigeria_map.png", width = 12, height = 12, dpi = 300)

map_nigeria_zoom <- ggplot(data = nigeria_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_nigeria_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_nigeria_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/nigeria_map_zoom.png", width = 12, height = 12, dpi = 300)

# 39) Republic of the Congo

congo_rep <- c("Republic of the Congo")
congo_rep_df <- africa |> filter(admin == "Republic of the Congo")
capital_cities_congo_rep_sf <- capital_cities_sf |> filter(country %in% congo_rep)

map_congo_rep <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% congo_rep_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_congo_rep

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/congo_rep_map.png", width = 12, height = 12, dpi = 300)

map_congo_rep_zoom <- ggplot(data = congo_rep_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_congo_rep_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_congo_rep_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/congo_rep_map_zoom.png", width = 12, height = 12, dpi = 300)

# 40) Rwanda

rwanda <- c("Rwanda")
rwanda_df <- africa |> filter(admin == "Rwanda")
capital_cities_rwanda_sf <- capital_cities_sf |> filter(country %in% rwanda)

map_rwanda <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% rwanda_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_rwanda

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/rwanda_map.png", width = 12, height = 12, dpi = 300)

map_rwanda_zoom <- ggplot(data = rwanda_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_rwanda_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_rwanda_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/rwanda_map_zoom.png", width = 12, height = 12, dpi = 300)

# 41) São Tomé and Principe

stp <- c("São Tomé and Principe")
stp_df <- africa |> filter(admin == "São Tomé and Principe")
capital_cities_stp_sf <- capital_cities_sf |> filter(country %in% stp)

map_stp <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% stp_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_stp

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/stp_map.png", width = 12, height = 12, dpi = 300)

map_stp_zoom <- ggplot(data = stp_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_stp_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_stp_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/stp_map_zoom.png", width = 12, height = 12, dpi = 300)

# 42) Senegal

senegal <- c("Senegal")
senegal_df <- africa |> filter(admin == "Senegal")
capital_cities_senegal_sf <- capital_cities_sf |> filter(country %in% senegal)

map_senegal <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% senegal_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_senegal

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/senegal_map.png", width = 12, height = 12, dpi = 300)

map_senegal_zoom <- ggplot(data = senegal_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_senegal_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_senegal_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/senegal_map_zoom.png", width = 12, height = 12, dpi = 300)

# 43) Seychelles

seychelles <- c("Seychelles")
seychelles_df <- africa |> filter(admin == "Seychelles")
capital_cities_seychelles_sf <- capital_cities_sf |> filter(country %in% seychelles)

map_seychelles <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% seychelles_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_seychelles

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/seychelles_map.png", width = 12, height = 12, dpi = 300)

map_seychelles_zoom <- ggplot(data = seychelles_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_seychelles_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_seychelles_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/seychelles_map_zoom.png", width = 12, height = 12, dpi = 300)


# 44) Sierra Leone

sierra_leone <- c("Sierra Leone")
sierra_leone_df <- africa |> filter(admin == "Sierra Leone")
capital_cities_sierra_leone_sf <- capital_cities_sf |> filter(country %in% sierra_leone)

map_sierra_leone <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% sierra_leone_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_sierra_leone

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/sierra_leone_map.png", width = 12, height = 12, dpi = 300)

map_sierra_leone_zoom <- ggplot(data = sierra_leone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_sierra_leone_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_sierra_leone_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/sierra_leone_map_zoom.png", width = 12, height = 12, dpi = 300)

# 45) Somalia

somalia <- c("Somalia")
somalia_df <- africa |> filter(admin == "Somalia")
capital_cities_somalia_sf <- capital_cities_sf |> filter(country %in% somalia)

map_somalia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% somalia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_somalia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/somalia_map.png", width = 12, height = 12, dpi = 300)

map_somalia_zoom <- ggplot(data = somalia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_somalia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_somalia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/somalia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 46) South Africa

south_africa <- c("South Africa")
south_africa_df <- africa |> filter(admin == "South Africa")
capital_cities_south_africa_sf <- capital_cities_sf |> filter(country %in% south_africa)

map_south_africa <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% south_africa_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_south_africa

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/south_africa_map.png", width = 12, height = 12, dpi = 300)

map_south_africa_zoom <- ggplot(data = south_africa_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_south_africa_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_south_africa_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/south_africa_map_zoom.png", width = 12, height = 12, dpi = 300)

# 47) South Sudan

south_sudan <- c("South Sudan")
south_sudan_df <- africa |> filter(admin == "South Sudan")
capital_cities_south_sudan_sf <- capital_cities_sf |> filter(country %in% south_sudan)

map_south_sudan <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% south_sudan_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_south_sudan

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/south_sudan_map.png", width = 12, height = 12, dpi = 300)

map_south_sudan_zoom <- ggplot(data = south_sudan_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_south_sudan_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_south_sudan_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/south_sudan_map_zoom.png", width = 12, height = 12, dpi = 300)

# 48) Sudan

sudan <- c("Sudan")
sudan_df <- africa |> filter(admin == "Sudan")
capital_cities_sudan_sf <- capital_cities_sf |> filter(country %in% sudan)

map_sudan <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% sudan_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_sudan

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/sudan_map.png", width = 12, height = 12, dpi = 300)

map_sudan_zoom <- ggplot(data = sudan_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_sudan_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_sudan_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/sudan_map_zoom.png", width = 12, height = 12, dpi = 300)

# 49) Togo

togo <- c("Togo")
togo_df <- africa |> filter(admin == "Togo")
capital_cities_togo_sf <- capital_cities_sf |> filter(country %in% togo)

map_togo <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% togo_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_togo

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/togo_map.png", width = 12, height = 12, dpi = 300)

map_togo_zoom <- ggplot(data = togo_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_togo_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_togo_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/togo_map_zoom.png", width = 12, height = 12, dpi = 300)

# 50) Tunisia 

tunisia_df <- africa |> filter(admin == "Tunisia")

map_tunisia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% tunisia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tunisia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/tunisia_map.png", width = 12, height = 12, dpi = 300)

map_tunisia_zoom <- ggplot(data = tunisia_df)+
  geom_sf(aes(fill = admin), linewidth = 1)+
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_tunisia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/tunisia_map_zoom.png", width = 12, height = 12, dpi = 300)



# 51) Uganda

uganda <- c("Uganda")
uganda_df <- africa |> filter(admin == "Uganda")
capital_cities_uganda_sf <- capital_cities_sf |> filter(country %in% uganda)

map_uganda <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% uganda_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_uganda

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/uganda_map.png", width = 12, height = 12, dpi = 300)

map_uganda_zoom <- ggplot(data = uganda_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_uganda_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_uganda_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/uganda_map_zoom.png", width = 12, height = 12, dpi = 300)

# 52) United Republic of Tanzania

ur_tanzania <- c("United Republic of Tanzania")
ur_tanzania_df <- africa |> filter(admin == "United Republic of Tanzania")
capital_cities_ur_tanzania_sf <- capital_cities_sf |> filter(country %in% ur_tanzania)

map_ur_tanzania <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% ur_tanzania_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ur_tanzania

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ur_tanzania_map.png", width = 12, height = 12, dpi = 300)

map_ur_tanzania_zoom <- ggplot(data = ur_tanzania_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_ur_tanzania_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_ur_tanzania_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/ur_tanzania_map_zoom.png", width = 12, height = 12, dpi = 300)

# 53) Zambia

zambia <- c("Zambia")
zambia_df <- africa |> filter(admin == "Zambia")
capital_cities_zambia_sf <- capital_cities_sf |> filter(country %in% zambia)

map_zambia <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% zambia_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_zambia

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/zambia_map.png", width = 12, height = 12, dpi = 300)

map_zambia_zoom <- ggplot(data = zambia_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_zambia_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_zambia_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/zambia_map_zoom.png", width = 12, height = 12, dpi = 300)

# 54) Zimbabwe

zimbabwe <- c("Zimbabwe")
zimbabwe_df <- africa |> filter(admin == "Zimbabwe")
capital_cities_zimbabwe_sf <- capital_cities_sf |> filter(country %in% zimbabwe)

map_zimbabwe <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_manual(values = "salmon1") +
  gghighlight(admin %in% zimbabwe_df) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_zimbabwe

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/zimbabwe_map.png", width = 12, height = 12, dpi = 300)

map_zimbabwe_zoom <- ggplot(data = zimbabwe_df)+
  geom_sf(aes(fill = admin), linewidth = 0.25, color = "salmon1")+
  geom_sf(data = capital_cities_zimbabwe_sf, color = "#6488EA", size = 4) +
  scale_fill_manual(values = "salmon1") +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_blank(), 
        panel.background = element_blank())

map_zimbabwe_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/zimbabwe_map_zoom.png", width = 12, height = 12, dpi = 300)

