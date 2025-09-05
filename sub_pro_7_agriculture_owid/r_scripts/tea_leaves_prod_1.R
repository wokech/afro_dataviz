# Tea Leaves Production

# 1) Load the Required Libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(tidyverse)
library(janitor)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(janitor)
library(scales)
library(devtools)
library(treemapify)
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)
library(jsonlite)

# 2) Data Cleaning and Organization

# Fetch the data (downloaded from FAO Stat and read in to R)

############# NORTH AMERICA = AMERICA minus SOUTH AMERICA ##########################

# Read in the data
tea_leaves_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/tea-production-fao-stat.csv")

# Clean the column headings

tea_leaves_prod_clean <- tea_leaves_prod %>%
  clean_names() 

# Change the column title names

tea_leaves_prod_clean <- tea_leaves_prod_clean %>%
  select(area, year, value)

# Filter by region

tea_leaves_prod_clean_region <- tea_leaves_prod_clean %>%
  filter(area %in% c("Africa", "Americas", "South America", "Asia", "Europe", "Oceania"))

# Filter by Country

# Separate A and B
A <- tea_leaves_prod_clean_region$value[tea_leaves_prod_clean_region$area == "Americas"]
B <- tea_leaves_prod_clean_region$value[tea_leaves_prod_clean_region$area == "South America"]

# Subtract A from B
C <- A - B

# Create new rows with C
df_C <- data.frame(
  area = "North America",
  value = C,
  year = 1961:2023
) |>
  select(area, year, value)

# Bind together (C rows will be below)
tea_leaves_prod_clean_region_complete <- rbind(tea_leaves_prod_clean_region, df_C)

tea_leaves_prod_clean_region_complete

# Remove "Americas" rows

tea_leaves_prod_clean_region_complete <- tea_leaves_prod_clean_region_complete |>
  filter(area != "Americas")

# 3) Continental tea_leaves production

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Oceania", "Africa", "Europe", "North America", "South America", "Asia")

tea_leaves_prod_clean_region_complete <- tea_leaves_prod_clean_region_complete %>%
  mutate(area = factor(area, levels = desired_order)) %>%
  arrange(desc(area))

#  calculate cumulative positions for label placement

label_df_tea_leaves <- tea_leaves_prod_clean_region_complete %>%
  filter(year == max(year)) %>%
  mutate(area = factor(area, levels = rev(desired_order))) %>%
  arrange(area) %>%
  mutate(x_label = max(year),
         y_top = cumsum(value),
         y_bottom = y_top - value,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(area, year, x_label, y_top, y_mid) 

# plot the stack area chart

tea_leaves_prod_clean_region_complete %>% 
  ggplot(aes(year, value, fill = area, label = area, color = area)) +
  geom_area() +
  geom_text_repel(
    data = label_df_tea_leaves,
    aes(x = x_label, y = y_mid, label = area, color = area),
    hjust = 0,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    hjust = 0,
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Tea Leaves Production\n(Millions of Tonnes)",
       title = "About 15% of global tea production was\nfrom Africa in 2020",
       subtitle = "",
       caption = "Data Source: FAOSTAT") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 35000000), labels  = 
                       label_number(scale = 1e-6, big.mark = ",")) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust  = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 24, hjust  = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, , hjust = 0, vjust = 1),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.subtitle.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none"
  ) +
  geom_vline(xintercept = 1991, linetype = "dashed", color = "black") +
  annotate("text", x = 1990, y = 20000000, size = 8, 
           label = "Switch from Official Figure\nto Estimated Value",
           color = "black", vjust = 0, angle = 90)

# ggsave("sub_pro_7_agriculture_owid/images/continental/continent_tea_leaves_1.png", width = 12, height = 12, dpi = 72)


tea_leaves_prod_clean_region_complete %>%
  filter(year == 2020) %>%
  mutate(percent = 100 * value/sum(value))




################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_tea_leaves_percent <- tea_leaves_prod_clean_region_non_fao_continent %>%
  group_by(year) %>%
  mutate(share = tea_leaves_production_tonnes / sum(tea_leaves_production_tonnes, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) %>%
  select(region, year, x_label, y_top, y_mid)


tea_leaves_prod_clean_region_non_fao_continent %>% 
  ggplot(aes(year, tea_leaves_production_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_tea_leaves_percent,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0,
    fontface = "bold",
    size = 8,
    inherit.aes = FALSE,
    direction = "y",
    nudge_x = 15,
    segment.curvature = 0.1,
    segment.size = 0.5,
    segment.ncp = 1,
    min.segment.length = 0
  ) +
  labs(x = "Year",
       y = "Share of Tea Leaves Production (%)",
       title = "Regional Share of Global Tea Leaves Production (1960–2020)",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 28, vjust = 1, face = "bold"),
    axis.title.y = element_text(size = 28, vjust = 1, face = "bold"),
    axis.text.x = element_text(size = 28, face = "bold", color = "black"),
    axis.text.y = element_text(size = 28, face = "bold", color = "black"),
    plot.title = element_text(family="Helvetica", face="bold", size = 36, colour = "#000000", hjust = 0),
    plot.caption = element_text(family = "Helvetica", size = 24, hjust = 0, vjust = 1),
    plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
    panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
    legend.position = "none"
  )


