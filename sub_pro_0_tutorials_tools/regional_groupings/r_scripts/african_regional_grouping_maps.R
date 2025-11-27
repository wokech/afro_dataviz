# African Regional Groupings with Maps

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

# 2) Get country data for mapping

# Fetch high-resolution country data
world <- ne_countries(scale = "large", returnclass = "sf")

# Filter African countries, including Seychelles and Mauritius
africa <- world %>%
  filter(continent == "Africa" | admin %in% c("Seychelles", "Mauritius"))

ggplot(data = africa) +
  geom_sf()

# Arab Maghreb Union (AMU)
amu <- c("Algeria", "Libya", "Mauritania", "Morocco", "Tunisia")
amu_df <- africa |> filter(admin %in% amu)

map_amu <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% amu) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_amu

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/amu_map.png", width = 12, height = 12, dpi = 300)

map_amu_zoom <- ggplot(data = amu_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_amu_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/amu_map_zoom.png", width = 12, height = 12, dpi = 300)


map_amu_zoom_no_label <- ggplot(data = amu_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_amu_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/amu_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)




# Common Market for Eastern and Southern Africa (COMESA)
comesa <- c("Burundi", "Comoros", "Democratic Republic of the Congo", "Djibouti", 
            "Egypt", "Eritrea", "Eswatini", "Ethiopia", "Kenya", "Libya", "Madagascar", 
            "Malawi", "Mauritius", "Rwanda", "Seychelles", "Somalia", "Sudan", 
            "Tunisia", "Uganda", "Zambia", "Zimbabwe")
comesa_df <- africa |> filter(admin %in% comesa)


map_comesa <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% comesa) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_comesa

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/comesa_map.png", width = 12, height = 12, dpi = 300)

map_comesa_zoom <- ggplot(data = comesa_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_comesa_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/comesa_map_zoom.png", width = 12, height = 12, dpi = 300)


map_comesa_zoom_no_label <- ggplot(data = comesa_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_comesa_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/comesa_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# Community of Sahel-Saharan States (CEN-SAD)
cen_sad <- c("Benin", "Burkina Faso", "Cape Verde", "Central African Republic", "Chad", 
             "Comoros", "Djibouti", "Egypt", "Eritrea", "Gambia", "Ghana", "Guinea", 
             "Guinea-Bissau", "Ivory Coast", "Kenya", "Liberia", "Libya", "Mali", 
             "Mauritania", "Morocco", "Niger", "Nigeria", "Sao Tome and Principe", 
             "Senegal", "Sierra Leone", "Somalia", "Sudan", "Togo", "Tunisia")
cen_sad_df <- africa |> filter(admin %in% cen_sad)

map_cen_sad <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% cen_sad) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_cen_sad

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/cen_sad_map.png", width = 12, height = 12, dpi = 300)

map_cen_sad_zoom <- ggplot(data = cen_sad_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_cen_sad_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/cen_sad_map_zoom.png", width = 12, height = 12, dpi = 300)


map_cen_sad_zoom_no_label <- ggplot(data = cen_sad_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_cen_sad_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/cen_sad_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)


# East African Community (EAC)
eac <- c("Burundi", "Democratic Republic of the Congo", "Kenya", 
         "Rwanda", "South Sudan", "United Republic of Tanzania", "Uganda")
eac_df <- africa |> filter(admin %in% eac)


map_eac <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% eac) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eac

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eac_map.png", width = 12, height = 12, dpi = 300)

map_eac_zoom <- ggplot(data = eac_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eac_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eac_map_zoom.png", width = 12, height = 12, dpi = 300)


map_eac_zoom_no_label <- ggplot(data = eac_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eac_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eac_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# Economic Community of Central African States (ECCAS/CEEAC)
eccas <- c("Angola", "Burundi", "Cameroon", "Central African Republic", "Chad", 
           "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", 
           "Republic of Congo", "Rwanda", "Sao Tome and Principe")
eccas_df <- africa |> filter(admin %in% eccas)


map_eccas <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% eccas) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eccas

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eccas_map.png", width = 12, height = 12, dpi = 300)

map_eccas_zoom <- ggplot(data = eccas_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eccas_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eccas_map_zoom.png", width = 12, height = 12, dpi = 300)


map_eccas_zoom_no_label <- ggplot(data = eccas_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_eccas_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/eccas_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# Economic Community of West African States (ECOWAS) - All (2025)
ecowas <- c("Benin", "Cape Verde", "Ivory Coast", "The Gambia", 
            "Ghana", "Guinea", "Guinea-Bissau", "Liberia", 
            "Nigeria", "Senegal", "Sierra Leone", "Togo")
ecowas_df <- africa |> filter(admin %in% ecowas)


map_ecowas <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% ecowas) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_map.png", width = 12, height = 12, dpi = 300)

map_ecowas_zoom <- ggplot(data = ecowas_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_map_zoom.png", width = 12, height = 12, dpi = 300)


map_ecowas_zoom_no_label <- ggplot(data = ecowas_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)




# Economic Community of West African States (ECOWAS) - Francophone (2025)
ecowas_francophone <- c("Benin", "Ivory Coast", "Guinea", 
                        "Senegal", "Togo")
ecowas_francophone_df <- africa |> filter(admin %in% ecowas_francophone)

map_ecowas_francophone <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% ecowas_francophone) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_francophone

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_francophone_map.png", width = 12, height = 12, dpi = 300)

map_ecowas_francophone_zoom <- ggplot(data = ecowas_francophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_francophone_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_francophone_map_zoom.png", width = 12, height = 12, dpi = 300)


map_ecowas_francophone_zoom_no_label <- ggplot(data = ecowas_francophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_francophone_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_francophone_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)




# Economic Community of West African States (ECOWAS) - Anglophone (2025)
ecowas_anglophone <- c("Gambia", "Ghana", "Liberia", 
                       "Nigeria", "Sierra Leone")
ecowas_anglophone_df <- africa |> filter(admin %in% ecowas_anglophone)

map_ecowas_anglophone <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% ecowas_anglophone) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_anglophone

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_anglophone_map.png", width = 12, height = 12, dpi = 300)

map_ecowas_anglophone_zoom <- ggplot(data = ecowas_anglophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_anglophone_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_anglophone_map_zoom.png", width = 12, height = 12, dpi = 300)


map_ecowas_anglophone_zoom_no_label <- ggplot(data = ecowas_anglophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_anglophone_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_anglophone_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# Economic Community of West African States (ECOWAS) - Lusophone (2025)
ecowas_lusophone <- c("Cabo Verde", "Guinea-Bissau")
ecowas_lusophone_df <- africa |> filter(admin %in% ecowas_lusophone)

map_ecowas_lusophone <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% ecowas_lusophone) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_lusophone

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_lusophone_map.png", width = 12, height = 12, dpi = 300)

map_ecowas_lusophone_zoom <- ggplot(data = ecowas_lusophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_lusophone_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_lusophone_map_zoom.png", width = 12, height = 12, dpi = 300)


map_ecowas_lusophone_zoom_no_label <- ggplot(data = ecowas_lusophone_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_ecowas_lusophone_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/ecowas_lusophone_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# The Alliance of Sahel States (AES)
aes <- c("Burkina Faso", "Mali", "Niger")
aes_df <- africa |> filter(admin %in% aes)


map_aes <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% aes) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_aes

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/aes_map.png", width = 12, height = 12, dpi = 300)

map_aes_zoom <- ggplot(data = aes_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_aes_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/aes_map_zoom.png", width = 12, height = 12, dpi = 300)



map_aes_zoom_no_label <- ggplot(data = aes_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_aes_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/aes_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)



# Intergovernmental Authority on Development (IGAD)
igad <- c("Djibouti", "Eritrea", "Ethiopia", "Kenya", 
          "Somalia", "South Sudan", "Sudan", "Uganda")
igad_df <- africa |> filter(admin %in% igad)


map_igad <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% igad) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_igad

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/igad_map.png", width = 12, height = 12, dpi = 300)

map_igad_zoom <- ggplot(data = igad_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_igad_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/igad_map_zoom.png", width = 12, height = 12, dpi = 300)


map_igad_zoom_no_label <- ggplot(data = igad_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_igad_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/igad_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)




# Southern African Development Community (SADC)
sadc <- c("Angola", "Botswana", "Comoros", "Democratic Republic of the Congo", "Eswatini", 
          "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", 
          "Seychelles", "South Africa", "United Republic of Tanzania", "Zambia", "Zimbabwe")
sadc_df <- africa |> filter(admin %in% sadc)


map_sadc <- ggplot(data = africa)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  gghighlight(admin %in% sadc) +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_sadc

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/sadc_map.png", width = 12, height = 12, dpi = 300)

map_sadc_zoom <- ggplot(data = sadc_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  geom_sf_text_repel(aes(label = admin), size = 10,
                     force = 10, nudge_x = -1, seed = 10,
                     min.segment.length = 0.25,
                     force_pull = 0.1,
                     max.overlaps = Inf,  # allow as many as possible
                     box.padding = 0.5, 
                     point.padding = 0.3) +
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_sadc_zoom

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/sadc_map_zoom.png", width = 12, height = 12, dpi = 300)



map_sadc_zoom_no_label <- ggplot(data = sadc_df)+
  geom_sf(aes(fill = admin), linewidth = 0.5)+
  scale_fill_d3(palette = 'category20') +
  theme_void()+
  labs(title = "",
       caption = "",
       fill = "")+
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "bisque1", color = "bisque1"), 
        panel.background = element_rect(fill = "bisque1", color = "bisque1"))

map_sadc_zoom_no_label

# Save the plot
ggsave("sub_pro_0_tutorials_tools/regional_groupings/images/sadc_map_zoom_no_label.png", width = 12, height = 12, dpi = 300)
