# Most Reviewed Locations in African Countries (February 2025)

# Source: top-rated.online

# Load required libraries
library(tidyverse)
library(ggtext)
library(ggflags)
library(showtext)  # For better font handling
library(readxl)
library(janitor)

# Add Google fonts
font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

# Import data
africa_top_rated <- read_excel("sub_pro_24_top_rated_online/top_rated_online_feb_2025.xlsx", sheet = 1)

africa_top_rated_clean <- africa_top_rated %>%
  clean_names()

str(africa_top_rated_clean)

africa_top_rated_clean <- africa_top_rated_clean %>%
  mutate(
    number_of_reviews = as.numeric(number_of_reviews),
    average_rating = as.numeric(average_rating),
    country_population = as.numeric(country_population),
    internet_penetration = as.numeric(internet_penetration)
  ) %>%
  filter(!is.na(number_of_reviews)) %>%
  mutate(label = paste0(most_reviewed, " [", average_rating, "]")) 

# Create the main plot
p <- ggplot(africa_top_rated_clean, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 2.5
  ) +
  # Place names with custom formatting using ggtext
  geom_richtext(
    aes(label = label),
    hjust = 0, nudge_x = 500, fill = NA, label.color = NA,
    family = "roboto"
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 5) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 175000),
    breaks = seq(50000, 175000, 50000),
    labels = c("50K", "100K", "150K"),
    expand = expansion(mult = 0.02),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = "Most Reviewed Places in every African country",
    subtitle = "Highest number of reviews on Google Maps",
    caption = "Source: top-rated.online"
  ) +
  # Theme customization
  theme_classic() +
  theme(
    # Text elements
    text = element_text(family = "roboto"),
    plot.title = element_text(
      family = "roboto_slab", face = "bold", 
      size = 18, color = "#2B4570", hjust = 0, margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "roboto", color = "#637381", 
      size = 12, hjust = 0, margin = margin(b = 20)
    ),
    plot.caption = element_text(
      family = "roboto", color = "#637381", 
      hjust = 0, size = 8, margin = margin(t = 15)
    ),
    # Grid elements
    panel.grid = element_blank(),
    # Axis elements
    axis.text.y = element_text(family = "roboto", size = 10, color = "#637381"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 10, color = "#637381"),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "#FFFFFF", color = NA)
  )

p

