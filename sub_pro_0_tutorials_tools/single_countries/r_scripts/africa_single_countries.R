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

ggplot(data = africa) +
  geom_sf(fill = "goldenrod2", linewidth = 0.5) +
  theme_void()

# 1) Algeria

algeria_df <- africa |> filter(admin == "Algeria")

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

map_algeria_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/algeria_map_zoom.png", width = 12, height = 12, dpi = 300)


# 2) Angola

angola_df <- africa |> filter(admin == "Angola")

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

map_angola_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/angola_map_zoom.png", width = 12, height = 12, dpi = 300)


# 3) Benin

benin_df <- africa |> filter(admin == "Benin")

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

map_benin_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/benin_map_zoom.png", width = 12, height = 12, dpi = 300)


# 4) Botswana

botswana_df <- africa |> filter(admin == "Botswana")

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

map_botswana_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/botswana_map_zoom.png", width = 12, height = 12, dpi = 300)


# 5) Burkina Faso

burkina_faso_df <- africa |> filter(admin == "Burkina Faso")

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

map_burkina_faso_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burkina_faso_map_zoom.png", width = 12, height = 12, dpi = 300)


# 6) Burundi


burundi_df <- africa |> filter(admin == "Burundi")

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

map_burundi_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/burundi_map_zoom.png", width = 12, height = 12, dpi = 300)


# 7) Cabo Verde

cabo_verde_df <- africa |> filter(admin == "Cabo Verde")

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

map_cabo_verde_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cabo_verde_map_zoom.png", width = 12, height = 12, dpi = 300)



# 8) Cameroon

cameroon_df <- africa |> filter(admin == "Cameroon")

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

map_cameroon_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/cameroon_map_zoom.png", width = 12, height = 12, dpi = 300)



# 9) Central African Republic

car_df <- africa |> filter(admin == "Central African Republic")

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

map_car_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/car_map_zoom.png", width = 12, height = 12, dpi = 300)


# 10) Chad

chad_df <- africa |> filter(admin == "Chad")

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

map_chad_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/chad_map_zoom.png", width = 12, height = 12, dpi = 300)



# 11) Comoros

comoros_df <- africa |> filter(admin == "Comoros")

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

map_comoros_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/comoros_map_zoom.png", width = 12, height = 12, dpi = 300)



# 12) Democratic Republic of the Congo

drc_df <- africa |> filter(admin == "Democratic Republic of the Congo")

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

map_drc_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/drc_map_zoom.png", width = 12, height = 12, dpi = 300)


# 13) Djibouti

djibouti_df <- africa |> filter(admin == "Djibouti")

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

map_djibouti_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/djibouti_map_zoom.png", width = 12, height = 12, dpi = 300)


# 14) Egypt

egypt_df <- africa |> filter(admin == "Egypt")

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

map_egypt_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/egypt_map_zoom.png", width = 12, height = 12, dpi = 300)


# 15) Equatorial Guinea


eq_gui_df <- africa |> filter(admin == "Equatorial Guinea")

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

map_eq_gui_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/single_countries/images/eq_gui_map_zoom.png", width = 12, height = 12, dpi = 300)


# 16) Eritrea




# 17) eSwatini



# 18) Ethiopia



# 19) Gabon
# 20) Gambia
# 21) Ghana
# 22) Guinea
# 23) Guinea-Bissau
# 24) Ivory Coast
# 25) Kenya
# 26) Lesotho
# 27) Liberia
# 28) Libya
# 29) Madagascar
# 30) Malawi
# 31) Mali
# 32) Mauritania
# 33) Mauritius
# 34) Morocco
# 35) Mozambique
# 36) Namibia, 
# 37) Niger, 
# 38) Nigeria, 
# 39) Republic of the Congo
# 40) Rwanda
# 41) São Tomé and Principe 
# 42) Senegal
# 43) Seychelles
# 44) Sierra Leone
# 45) Somalia
# 46) South Africa
# 47) South Sudan
# 48) Sudan
# 49) Togo
# 50) Tunisia 
# 51) Uganda
# 52) United Republic of Tanzania
# 53) Zambia
# 54) Zimbabwe