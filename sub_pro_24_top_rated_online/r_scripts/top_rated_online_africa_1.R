# Most Reviewed Locations in African Countries (February 2025)

# Source: top-rated.online

# Load required libraries
library(tidyverse)
library(ggtext)
library(showtext)  # For better font handling
library(readxl)
library(janitor)

# install.packages("ggflags", repos = c(
#   "https://jimjam-slam.r-universe.dev",
#   "https://cloud.r-project.org"))
library(ggflags)

# Add Google fonts
font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")
showtext_auto()

# Import data
africa_top_rated_feb_2025 <- read_excel("sub_pro_24_top_rated_online/datasets/top_rated_online_africa_feb_2025.xlsx", sheet = 1)

africa_top_rated_clean_feb_2025 <- africa_top_rated_feb_2025 %>%
  clean_names()

str(africa_top_rated_clean_feb_2025)

africa_top_rated_clean_feb_2025 <- africa_top_rated_clean_feb_2025 %>%
  mutate(
    number_of_reviews = as.numeric(number_of_reviews),
    average_rating = as.numeric(average_rating),
    country_population = as.numeric(country_population),
    internet_penetration = as.numeric(internet_penetration)
  ) %>%
  filter(!is.na(number_of_reviews)) %>%
  mutate(label = paste0(most_reviewed, " [", average_rating, "]", " - ", number_of_reviews, " reviews")) 

# All countries

p <- ggplot(africa_top_rated_clean_feb_2025, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 5
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 10,
    hjust = 0, nudge_x = 500
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
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 40),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 40),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025.png", width = 12, height = 12, dpi = 300)

# Top 10 countries

africa_top_rated_clean_feb_2025_top_10 <- africa_top_rated_clean_feb_2025 %>%
  slice_max(number_of_reviews, n = 10)

p <- ggplot(africa_top_rated_clean_feb_2025_top_10, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
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
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 80),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_top_10.png", width = 12, height = 12, dpi = 300)

# Northern Africa

select_northern_africa <- c("Morocco", "Algeria", "Egypt", 
                            "Tunisia", "Libya", "Mauritania", "Libyan") %>%
  str_to_upper()

africa_top_rated_clean_feb_2025_northern_africa <- africa_top_rated_clean_feb_2025 %>%
  filter(country %in% select_northern_africa) %>%
  mutate(country = if_else(country == "LIBYAN", "LIBYA", country))


p <- ggplot(africa_top_rated_clean_feb_2025_northern_africa, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 150000),
    breaks = seq(50000, 150000, 50000),
    labels = c("50K", "100K", "150K"),
    expand = expansion(mult = c(0, 0.05)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 80),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_northern_africa.png", width = 12, height = 12, dpi = 300)


# Southern Africa

select_southern_africa <- c("South Africa", "Angola", "Zambia", "Mozambique", 
                            "Madagascar", "Comoros", "Namibia", "Malawi", 
                            "Zimbabwe", "Lesotho", "Botswana", "Eswatini", 
                            "Swaziland", "Mauritius", "Seychelles") %>%
  str_to_upper()

africa_top_rated_clean_feb_2025_southern_africa <- africa_top_rated_clean_feb_2025 %>%
  filter(country %in% select_southern_africa) %>%
  mutate(country = if_else(country == "SWAZILAND", "ESWATINI", country))


p <- ggplot(africa_top_rated_clean_feb_2025_southern_africa, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 120000),
    breaks = seq(40000, 120000, 40000),
    labels = c("40K", "80K", "120K"),
    expand = expansion(mult = c(0, 0.05)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 80),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_southern_africa.png", width = 12, height = 12, dpi = 300)



# Western Africa

select_western_africa <- c("Senegal", "Mali", "Cape Verde", "Guinea", "Gambia", 
                            "Sierra Leone", "Guinea-Bissau", "Liberia", "Nigeria", 
                            "Cote d’Ivoire", "Burkina Faso", "Ghana", 
                            "Benin", "Niger", "Togo") %>%
  str_to_upper()


africa_top_rated_clean_feb_2025_western_africa <- africa_top_rated_clean_feb_2025 %>%
  filter(country %in% select_western_africa) 


p <- ggplot(africa_top_rated_clean_feb_2025_western_africa, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 50000),
    breaks = seq(25000, 50000, 25000),
    labels = c("25K", "50K"),
    expand = expansion(mult = c(0, 0.05)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 80),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_western_africa.png", width = 12, height = 12, dpi = 300)


# Eastern Africa

select_eastern_africa <- c("Uganda", "Tanzania", "Kenya", "Sudan", "Rwanda", 
                           "Burundi", "Ethiopia", "South Sudan", 
                           "Djibouti", "Somalia") %>%
  str_to_upper()

africa_top_rated_clean_feb_2025_eastern_africa <- africa_top_rated_clean_feb_2025 %>%
  filter(country %in% select_eastern_africa) 


p <- ggplot(africa_top_rated_clean_feb_2025_eastern_africa, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 30000),
    breaks = seq(15000, 30000, 15000),
    labels = c("15K", "30K"),
    expand = expansion(mult = c(0, 0.05)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 80),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_eastern_africa.png", width = 12, height = 12, dpi = 300)



# Central Africa

select_central_africa <- c("Cameroon", "Congo, DR", "Gabon", 
                           "Equatorial Guinea", "Congo, Republic", 
                           "Central African Republic", "Chad", 
                           "Sao Tome and Principe") %>%
  str_to_upper()

africa_top_rated_clean_feb_2025_central_africa <- africa_top_rated_clean_feb_2025 %>%
  filter(country %in% select_central_africa) 


p <- ggplot(africa_top_rated_clean_feb_2025_central_africa, aes(x = number_of_reviews, y = reorder(country, number_of_reviews))) +
  # Main data segments
  geom_segment(
    aes(x = 0, xend = number_of_reviews, 
        y = reorder(country, number_of_reviews), yend = reorder(country, number_of_reviews)),
    color = "#A5D7F7", size = 20
  ) +
  # Place names with custom formatting using ggtext
  geom_text(
    aes(x = 0,
        y = country,
        label = label),
    size = 20,
    hjust = 0, nudge_x = 500
  ) +
  # Country flags
  geom_flag(aes(x = -5000, country = tolower(iso2)), size = 15) +
  # X-axis formatting
  scale_x_continuous(
    name = NULL,
    limits = c(-10000, 10000),
    breaks = seq(5000, 10000, 5000),
    labels = c("5K", "10K"),
    expand = expansion(mult = c(0, 1.4)),
    position = "bottom"
  ) +
  # Title and caption
  labs(
    title = ", ",
    subtitle = ", ",
    caption = ", "
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
    axis.text.y = element_text(family = "roboto", size = 60),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = 80),
    # Plot margins
    plot.margin = margin(3, 3, 3, 3),
    # Plot background
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1")
  )

p

ggsave("sub_pro_24_top_rated_online/images/top_rated_online_africa_feb_2025_central_africa.png", width = 12, height = 12, dpi = 300)
