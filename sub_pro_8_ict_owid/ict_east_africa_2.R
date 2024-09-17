# ICT in East Africa (Part 2)

# Load the required libraries and packages

# install.packages()
# library()

library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemes)

# Load the required datasets

# a) ICT adoption per 100 people

ict_per_100 <- read_csv("sub_pro_8_ict_owid/datasets/ict-adoption-per-100-people.csv")

# Clean the datasets

ict_per_100_clean <- ict_per_100 %>%
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

# 1) Number of fixed vs mobile in SS Africa

ict_per_100_clean_ssa <- ict_per_100_clean %>%
  rename("country" = "entity") %>%
  filter(country == "Sub-Saharan Africa (WB)") %>%
  select(!c(code, individuals_using_the_internet_percent_of_population)) 

ict_per_100_clean_ssa_long <- ict_per_100_clean_ssa %>%
  pivot_longer(!c(country, year), names_to = "connection_type", values_to = "numbers_per_100")

# Label

ict_per_100_clean_ssa_long_label_5 <- ict_per_100_clean_ssa_long %>%
  group_by(connection_type) %>%
  filter(year == 2015) 

ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "fixed_telephone_subscriptions_per_100_people"] <- "Fixed Telephone"
ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "fixed_broadband_subscriptions_per_100_people"] <- "Fixed Broadband"
ict_per_100_clean_ssa_long_label_5[ict_per_100_clean_ssa_long_label_5 == "mobile_cellular_subscriptions_per_100_people"] <- "Mobile"

ict_per_100_clean_ssa_long %>%
  filter(year>=2000 & year <= 2021) %>%
  ggplot(aes(x = year, 
             y = numbers_per_100, 
             color = connection_type)) + 
  geom_line(linewidth = 1) +
  labs(x = "Year",
       y = "Number of users (per 100 people)",
       title = "Cellular dominates in Sub-Saharan Africa",
       subtitle = "Fixed line and Broadband lag significantly behind ",
       caption = "Data Source: Our World in Data") +
  scale_color_manual(values = c("darkred", "navy", "darkred", "navy", "darkgreen", "darkgreen")) +# figure out what the order does
  theme_classic() +
  geom_text_repel(data = ict_per_100_clean_ssa_long_label_5,
                  aes(label = connection_type), 
                  nudge_x = 0.5,
                  nudge_y = 0.5,
                  size = 7) +
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

ggsave("sub_pro_8_ict_owid/images/communication_ssa.png", width = 12, height = 12, dpi = 300)

# 2) 

# ICT per 100 in Africa

ict_per_100_clean_africa <- ict_per_100_clean |>
  rename("country" = "entity") |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Ivory Coast",
    TRUE ~ country
  )) |>
  filter(country %in% african_countries)