# African Lions vs the Asian Tigers / Asian Tiger Cubs / Pacific Pumas

# Constant GDP per capita = GDP calculated using prices from a 
# fixed base year and divided by population. Allows comparison 
# across years without the effect of inflation.

# Load all the libraries

library(tidyverse)
library(janitor)
library(ggauto)
library(ggrepel)
library(scales)
library(patchwork)

# Load the datasets

total_gdp_per_capita <- read_csv("sub_pro_12_tiger_tiger_cubs_lions_pumas/datasets/combined_datasets/total_gdp_per_capita.csv")

total_gdp_per_capita_tidy <- total_gdp_per_capita %>%
  pivot_longer(
    cols = -observation_date,
    names_to = "Country",
    values_to = "GDP per Capita"     
  ) |>
  clean_names()

total_gdp_per_capita_tidy <- total_gdp_per_capita_tidy |>
  mutate(region = case_when(
    country == "Vietnam" ~ "Asian Tiger Cubs",
    country == "Indonesia" ~ "Asian Tiger Cubs",
    country == "Malaysia" ~ "Asian Tiger Cubs",
    country == "Thailand" ~ "Asian Tiger Cubs",
    country == "The Philippines" ~ "Asian Tiger Cubs",
    country == "Peru" ~ "Pacific Pumas",
    country == "Mexico" ~ "Pacific Pumas",
    country == "Chile" ~ "Pacific Pumas",
    country == "Colombia" ~ "Pacific Pumas",
    country == "Republic of Korea" ~ "Asian Tigers",
    country == "Hong Kong SAR" ~ "Asian Tigers",
    country == "Singapore" ~ "Asian Tigers",
    country == "South Africa" ~ "African Lions",
    country == "Ethiopia" ~ "African Lions",
    country == "Ghana" ~ "African Lions",
    country == "Nigeria" ~ "African Lions",
    country == "Mozambique" ~ "African Lions",
    country == "Kenya" ~ "African Lions",
    TRUE ~ "Other"
  ))

# Test Plot

total_gdp_per_capita_tidy |>
  dplyr::mutate(observation_date = ymd(observation_date)) |>
  ggauto(observation_date, gdp_per_capita, country)

################################################################################
# A. Asian Tigers
################################################################################

total_gdp_per_capita_tidy |>
  filter(country %in% c("Republic of Korea", 
                        "Hong Kong SAR", 
                        "Singapore")) |>
  ggplot() + 
  geom_line(aes(observation_date, 
                gdp_per_capita, color = country))

# Enhanced image

plot_data_asian_tigers <- total_gdp_per_capita_tidy |>
  filter(
    country %in% c(
      "Republic of Korea",
      "Hong Kong SAR",
      "Singapore"
    )
  )

# Create labels for the latest observation
label_data_asian_tigers <- plot_data_asian_tigers |>
  group_by(country) |>
  filter(observation_date == max(observation_date)) |>
  ungroup()

asian_tigers_plot <- 
ggplot(
  plot_data_asian_tigers,
  aes(
    x = observation_date,
    y = gdp_per_capita,
    colour = country
  )
) +
  geom_line(
    linewidth = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = label_data_asian_tigers,
    aes(label = case_when(
      country == "Republic of Korea" ~ "South Korea",
      country == "Hong Kong SAR" ~ "Hong Kong",
      TRUE ~ country
    )
    ),
    direction = "y",
    hjust = 0,
    nudge_x = 500,
    segment.color = "grey50",
    size = 7,
    show.legend = FALSE, 
    force = 10,
    box.padding = 1,
    point.padding = 1,
    max.overlaps = Inf
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("1960-01-01"),
      as.Date("2020-01-01"),
      by = "20 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.35))
  ) +
  
  scale_y_continuous(
    labels = dollar_format(prefix = "$")
  ) +
  
  scale_colour_manual(
    values = c(
      "Republic of Korea" = "#9ACD32",
      "Hong Kong SAR" = "#8FBC8F",
      "Singapore" = "#008080"
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Constant GDP per capita\n(2010 U.S. Dollars)",
    caption = ""
  ) +
  
  theme_minimal(base_size = 32) +
  
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    plot.margin = margin(
      20, 20, 20, 20
    ),
    plot.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ), 
    panel.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ),
    axis.line.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.3, "cm")
  )

asian_tigers_plot

#Save the plot
ggsave("sub_pro_12_tiger_tiger_cubs_lions_pumas/images/total_gdp_per_capita_africa_vs_others/asia_tigers.png", width = 12, height = 6, dpi = 300)

################################################################################
# B. Asian Tiger Cubs
################################################################################

total_gdp_per_capita_tidy |>
  filter(country %in% c("Vietnam", 
                        "Malaysia", 
                        "Thailand",
                        "The Philippines")) |>
  ggplot() + 
  geom_line(aes(observation_date, 
                gdp_per_capita, color = country))

# Enhanced image

plot_data_asian_tiger_cubs <- total_gdp_per_capita_tidy |>
  filter(
    country %in% c(
      "Vietnam", 
      "Malaysia", 
      "Thailand",
      "The Philippines"
    )
  )

# Create labels for the latest observation
label_data_asian_tiger_cubs <- plot_data_asian_tiger_cubs |>
  group_by(country) |>
  filter(observation_date == max(observation_date)) |>
  ungroup()

asian_tiger_cubs_plot <- 
ggplot(
  plot_data_asian_tiger_cubs,
  aes(
    x = observation_date,
    y = gdp_per_capita,
    colour = country
  )
) +
  geom_line(
    linewidth = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = label_data_asian_tiger_cubs,
    aes(label = country),
    direction = "y",
    hjust = 0,
    nudge_x = 500,
    segment.color = "grey50",
    size = 7,
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("1960-01-01"),
      as.Date("2020-01-01"),
      by = "20 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.35))
  ) +
  
  scale_y_continuous(
    labels = dollar_format(prefix = "$")
  ) +
  scale_colour_manual(
    values = c(
      "Vietnam" = "#32CD32",
      "Malaysia" = "#9ACD32",
      "Thailand" = "#8FBC8F",
      "The Philippines" = "#008080"
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Constant GDP per capita\n(2010 U.S. Dollars)",
    caption = ""
  ) +
  
  theme_minimal(base_size = 32) +
  
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    plot.margin = margin(
      20, 20, 20, 20
    ),
    plot.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ), 
    panel.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ),
    axis.line.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.3, "cm")
  )

asian_tiger_cubs_plot

#Save the plot
ggsave("sub_pro_12_tiger_tiger_cubs_lions_pumas/images/total_gdp_per_capita_africa_vs_others/asia_tiger_cubs.png", width = 12, height = 6, dpi = 300)


################################################################################
# C. Pacific Pumas
################################################################################

total_gdp_per_capita_tidy |>
  filter(country %in% c("Peru", 
                        "Mexico", 
                        "Chile",
                        "Colombia")) |>
  ggplot() + 
  geom_line(aes(observation_date, 
                gdp_per_capita, color = country))

# Enhanced image

plot_data_pacific_pumas <- total_gdp_per_capita_tidy |>
  filter(
    country %in% c(
      "Peru", 
      "Mexico", 
      "Chile",
      "Colombia"
    )
  )


# Create labels for the latest observation
label_data_pacific_pumas <- plot_data_pacific_pumas |>
  group_by(country) |>
  filter(observation_date == max(observation_date)) |>
  ungroup()

pacific_pumas_plot <-
ggplot(
  plot_data_pacific_pumas,
  aes(
    x = observation_date,
    y = gdp_per_capita,
    colour = country
  )
) +
  geom_line(
    linewidth = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = label_data_pacific_pumas,
    aes(label = country),
    direction = "y",
    hjust = 0,
    nudge_x = 500,
    segment.color = "grey50",
    size = 7,
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("1960-01-01"),
      as.Date("2020-01-01"),
      by = "20 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.35))
  ) +
  
  scale_y_continuous(
    labels = dollar_format(prefix = "$")
  ) +
  scale_colour_manual(
    values = c(
      "Peru" = "#32CD32",
      "Mexico" = "#9ACD32",
      "Chile" = "#8FBC8F",
      "Colombia" = "#008080"
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Constant GDP per capita\n(2010 U.S. Dollars)",
    caption = ""
  ) +
  
  theme_minimal(base_size = 32) +
  
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    plot.margin = margin(
      20, 20, 20, 20
    ),
    plot.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ), 
    panel.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ),
    axis.line.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.3, "cm")
  )

pacific_pumas_plot

#Save the plot
ggsave("sub_pro_12_tiger_tiger_cubs_lions_pumas/images/total_gdp_per_capita_africa_vs_others/pacific_pumas.png", width = 12, height = 6, dpi = 300)


################################################################################
# D. African Lions 
################################################################################

total_gdp_per_capita_tidy |>
  filter(country %in% c("Kenya", 
                        "South Africa", 
                        "Ethiopia", 
                        "Ghana",
                        "Nigeria",
                        "Mozambique")) |>
  ggplot() + 
  geom_line(aes(observation_date, 
                gdp_per_capita, color = country))

# Enhanced image

plot_data_african_lions <- total_gdp_per_capita_tidy |>
  filter(
    country %in% c(
      "Kenya", 
      "South Africa", 
      "Ethiopia", 
      "Ghana",
      "Nigeria",
      "Mozambique"
    )
  )


# Create labels for the latest observation
label_data_african_lions <- plot_data_african_lions |>
  group_by(country) |>
  filter(observation_date == max(observation_date)) |>
  ungroup()

african_lions_plot <-
ggplot(
  plot_data_african_lions,
  aes(
    x = observation_date,
    y = gdp_per_capita,
    colour = country
  )
) +
  geom_line(
    linewidth = 2,
    show.legend = FALSE
  ) +
  
  geom_text_repel(
    data = label_data_african_lions,
    aes(label = country),
    direction = "y",
    hjust = 0,
    nudge_x = 500,
    segment.color = "grey50",
    size = 7,
    show.legend = FALSE
  ) +
  
  scale_x_date(
    breaks = seq(
      as.Date("1960-01-01"),
      as.Date("2020-01-01"),
      by = "20 years"
    ),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.35))
  ) +
  
  scale_y_continuous(
    labels = dollar_format(prefix = "$")
  ) +
  
  scale_colour_manual(
    values = c(
      "Kenya" = "#5F9EA0",
      "South Africa" = "#6495ED",
      "Ethiopia" = "#87CEFA",
      "Ghana" = "#00BFFF",
      "Nigeria" = "#0000FF",
      "Mozambique" = "#191970"
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = NULL,
    y = "Constant GDP per capita\n(2010 U.S. Dollars)",
    caption = ""
  ) +
  
  theme_minimal(base_size = 32) +
  
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    plot.margin = margin(
      20, 20, 20, 20
    ),
    plot.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ), 
    panel.background = element_rect(
      fill = "bisque1", color = "bisque1"
    ),
    axis.line.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.x = element_line(
      colour = "grey65",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.3, "cm")
  )

african_lions_plot
  
#Save the plot
ggsave("sub_pro_12_tiger_tiger_cubs_lions_pumas/images/total_gdp_per_capita_africa_vs_others/african_lions.png", width = 12, height = 6, dpi = 300)

