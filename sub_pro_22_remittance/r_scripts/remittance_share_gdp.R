# Remittance as a share of GDP (%)

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
# remittance_share_gdp <- read.csv("https://ourworldindata.org/grapher/money-sent-or-brought-back-by-migrants-as-a-share-of-gdp.csv?v=1&csvType=full&useColumnShortNames=false")

# Save data
# write_csv(remittance_share_gdp, "sub_pro_22_remittance/datasets/remittance_share_gdp.csv")

# Load data again
remittance_share_gdp <- read_csv("sub_pro_22_remittance/datasets/remittance_share_gdp.csv")

# Clean the data

# List the African countries first

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

remittance_share_gdp_select <- remittance_share_gdp %>%
  clean_names() %>%
  filter(entity %in% african_countries)

# Check for number of countries selected
unique(remittance_share_gdp_select$entity)

# B) EDA and Basic Plot

remittance_share_gdp_select |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 5,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=15, y=9.5, label="One-Tenth Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024.png", width = 12, height = 12, dpi = 300)


# B) One-Third Plot

remittance_share_gdp_select |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  gghighlight(personal_remittances_received_of_gdp > 10) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 5,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=15, y=9.5, label="One-Tenth Share", size = 7.5, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_one_third.png", width = 12, height = 12, dpi = 300)

# By region

# Northern Africa

select_northern_africa <- c("Morocco", "Algeria", "Egypt", 
                            "Tunisia", "Libya", "Mauritania")

remittance_share_gdp_select_northern_africa <- remittance_share_gdp_select |>
  filter(entity %in% select_northern_africa)

unique(remittance_share_gdp_select_northern_africa$entity)

remittance_share_gdp_select_northern_africa |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=2, y=9.5, label="One-Tenth Share", size = 10, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_northern_africa.png", width = 12, height = 12, dpi = 300)


# Southern Africa

select_southern_africa <- c("South Africa", "Angola", "Zambia", "Mozambique", 
                            "Madagascar", "Comoros", "Namibia", "Malawi", 
                            "Zimbabwe", "Lesotho", "Botswana", 
                            "Eswatini", "Mauritius", "Seychelles")

remittance_share_gdp_select_southern_africa <- remittance_share_gdp_select |>
  filter(entity %in% select_southern_africa)

remittance_share_gdp_select_southern_africa |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=5, y=9.5, label="One-Tenth Share", size = 10, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_southern_africa.png", width = 12, height = 12, dpi = 300)


# Western Africa

select_western_africa <- c("Senegal", "Mali", "Cape Verde", "Guinea", "Gambia", 
                           "Sierra Leone", "Guinea-Bissau", "Liberia", "Nigeria", 
                           "Cote d'Ivoire", "Burkina Faso", "Ghana", 
                           "Benin", "Niger", "Togo")


remittance_share_gdp_select_western_africa <- remittance_share_gdp_select |>
  filter(entity %in% select_western_africa)

remittance_share_gdp_select_western_africa |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=5, y=9.5, label="One-Tenth Share", size = 10, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_western_africa.png", width = 12, height = 12, dpi = 300)


# Eastern Africa

select_eastern_africa <- c("Uganda", "Tanzania", "Kenya", "Sudan", "Rwanda", 
                           "Burundi", "Ethiopia", "South Sudan", 
                           "Djibouti", "Somalia")

remittance_share_gdp_select_eastern_africa <- remittance_share_gdp_select |>
  filter(entity %in% select_eastern_africa)

select_eastern_africa[!select_eastern_africa %in% remittance_share_gdp_select_eastern_africa$entity]

remittance_share_gdp_select_eastern_africa |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=2, y=9.5, label="One-Tenth Share", size = 10, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_eastern_africa.png", width = 12, height = 12, dpi = 300)


# Central Africa

select_central_africa <- c("Cameroon", "Democratic Republic of Congo", "Gabon", 
                           "Equatorial Guinea", "Congo", 
                           "Central African Republic", "Chad", 
                           "Sao Tome and Principe")

remittance_share_gdp_select_central_africa <- remittance_share_gdp_select |>
  filter(entity %in% select_central_africa)

remittance_share_gdp_select_central_africa |> 
  filter(year == 2024) |>
  arrange(desc(personal_remittances_received_of_gdp)) |>
  ggplot(aes(x=reorder(entity, personal_remittances_received_of_gdp), y = personal_remittances_received_of_gdp, fill = entity)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  #scale_fill_brewer(palette = "PuRd") +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  geom_text(aes(y = 0.02, label = entity),
            hjust = 0,
            vjust = 0.5,
            size = 10,
            color = "black",
            show.legend = FALSE) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  annotate("text", x=2, y=9.5, label="One-Tenth Share", size = 10, angle=90) +
  labs(x = "Country",
       y = "Remittance as a share of GDP (%, 2024)",
       title = "",
       subtitle = "",
       caption = "") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_blank(), 
        axis.ticks.length.x = unit(0.2, "cm"),  # Lengthen the ticks
        plot.title = element_text(family="Helvetica", face="bold", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family="Helvetica", size = 25),
        plot.caption = element_text(family = "Helvetica",size = 25, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        legend.title = element_blank(),
        legend.position = "none") 

ggsave("sub_pro_22_remittance/images/remittance_share_gdp_2024_central_africa.png", width = 12, height = 12, dpi = 300)
