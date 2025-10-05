# Tomato production

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

# Fetch the data

# tomato_prod <- read.csv("https://ourworldindata.org/grapher/tomato-production.csv?v=1&csvType=full&useColumnShortNames=true",
#                       na.strings = "")
# 
# # Save the data
# write.csv(tomato_prod, "sub_pro_7_agriculture_owid/datasets/tomato-production-tonnes.csv",
#           row.names = FALSE)

# Read in the data
tomato_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/tomato-production-tonnes.csv")

# Clean the column headings

tomato_prod_clean <- tomato_prod |>
  clean_names() 

# Change the column title names

tomato_prod_clean <- tomato_prod_clean |>
  rename("region" = "entity",
         "tomato_production_tonnes" = "tomatoes_00000388_production_005510_tonnes") 

# Filter by region

tomato_prod_clean_region <- tomato_prod_clean |>
  filter(is.na(code)) |>
  select(c(1,3,4)) 

# Filter by FAO region

tomato_prod_clean_region_fao <- tomato_prod_clean_region |>
  filter(grepl('(FAO)', region))

# Filter by non-FAO region

tomato_prod_clean_region_non_fao <- tomato_prod_clean_region |>
  filter(!grepl('(FAO)', region))

# 3) Continental (Non-FAO) tomato meat production

# a) Stacked area chart

tomato_prod_clean_region_non_fao_continent <- tomato_prod_clean_region_non_fao |>
  filter(region %in% c("Africa", "Asia", "Europe", 
                       "North America", "South America", 
                       "Oceania"))

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Oceania", "Africa", "Europe", "North America", "South America", "Asia")

tomato_prod_clean_region_non_fao_continent <- tomato_prod_clean_region_non_fao_continent |>
  mutate(region = factor(region, levels = desired_order)) |>
  arrange(desc(region))

#  calculate cumulative positions for label placement

label_df_tomato <- tomato_prod_clean_region_non_fao_continent |>
  filter(year == max(year)) |>
  mutate(region = factor(region, levels = rev(desired_order))) |>
  arrange(region) |>
  mutate(x_label = max(year),
         y_top = cumsum(tomato_production_tonnes),
         y_bottom = y_top - tomato_production_tonnes,
         y_mid = (y_bottom + y_top) / 2) |>
  select(region, year, x_label, y_top, y_mid) 

# plot the stack area chart

p1 <- tomato_prod_clean_region_non_fao_continent |> 
  ggplot(aes(year, tomato_production_tonnes, fill = region, label = region, color = region)) +
  geom_area() +
  geom_text_repel(
    data = label_df_tomato,
    aes(x = x_label, y = y_mid, label = region, color = region),
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
       y = "Millions of Tonnes",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 200000000), labels  = 
                       label_number(scale = 1e-6)) +
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
  )

# #ggsave("sub_pro_7_agriculture_owid/images/continental/continent_tomato_1.png", width = 12, height = 12, dpi = 72)


tomato_prod_clean_region_non_fao_continent |>
  filter(year == 2020) |>
  mutate(percent = 100 * tomato_production_tonnes/sum(tomato_production_tonnes))


################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_tomato_percent <- tomato_prod_clean_region_non_fao_continent |>
  group_by(year) |>
  mutate(share = tomato_production_tonnes / sum(tomato_production_tonnes, na.rm = TRUE)) |>
  ungroup() |>
  filter(year == max(year)) |>
  mutate(region = factor(region, levels = rev(desired_order))) |>
  arrange(region) |>
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) |>
  select(region, year, x_label, y_top, y_mid)


p2 <- tomato_prod_clean_region_non_fao_continent |> 
  ggplot(aes(year, tomato_production_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_tomato_percent,
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
       y = "",
       title = "",
       caption = "") +
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

##ggsave("sub_pro_7_agriculture_owid/images/continental_stack_perc/continent_tomato_1.png", width = 12, height = 12, dpi = 72)

(p1/p2) + plot_annotation() & theme(plot.margin = margin(0,0,0,0))
ggsave("sub_pro_7_agriculture_owid/images/continental_combi/tomato.png", width = 12, height = 16, dpi = 300)

################################################################################
# AFRICA ONLY CHARTS
################################################################################

# 4) Tomato production in Africa

# Organize data

# Africa total combined with regions data
tomato_prod_clean_region_fao_africa <- tomato_prod_clean_region_fao |>
  filter(str_detect(region, "frica"))

# Africa data alone
tomato_prod_clean_region_fao_africa_only <- tomato_prod_clean_region_fao_africa |>
  filter(region %in% c("Africa (FAO)"))

# Africa regions alone
tomato_prod_clean_region_fao_africa_segment <- tomato_prod_clean_region_fao_africa |>
  filter(region %in% c("Eastern Africa (FAO)", "Middle Africa (FAO)", 
                       "Northern Africa (FAO)", "Southern Africa (FAO)",
                       "Western Africa (FAO)")) |>
  mutate(region = str_remove(region, " \\(FAO\\)"))

# Use afro_stack color palette for the design on bisque1 background

afro_stack_palette <- c(
  "#0072B2", "#E69F00", "#009E73",
  "#D55E00", "#CC79A7", "#56B4E9"
)

# reorder the stacks

desired_order <- c("Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa", "Western Africa")

# order of the colored regions

tomato_prod_clean_region_fao_africa_segment <- tomato_prod_clean_region_fao_africa_segment |>
  mutate(region = factor(region, levels = desired_order)) |>
  arrange(desc(region))

label_df_tomato_africa <- tomato_prod_clean_region_fao_africa_segment |>
  filter(year == max(year)) |>
  mutate(region = factor(region, levels = rev(desired_order))) |>
  arrange(region) |>
  mutate(x_label = max(year),
         y_top = cumsum(tomato_production_tonnes),
         y_bottom = y_top - tomato_production_tonnes,
         y_mid = (y_bottom + y_top) / 2) |>
  select(region, year, x_label, y_top, y_mid) 

# b) Stacked Area chart for Africa regions

p3 <- tomato_prod_clean_region_fao_africa_segment |> 
  ggplot(aes(year, tomato_production_tonnes, fill = region, label = region, color = region)) +
  geom_area() +
  geom_text_repel(
    data = label_df_tomato_africa,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0.1,
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
       y = "Millions of Tonnes",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 27500000), labels  = 
                       label_number(scale = 1e-6)) +
  scale_fill_manual(values = afro_stack_palette) +
  scale_color_manual(values = afro_stack_palette) +
  theme(axis.title.x =element_text(size = 28, vjust = 1, face = "bold"),
        axis.title.y =element_text(size = 28, vjust = 1, face = "bold"),
        axis.text.x = element_text(size = 28, face = "bold", color = "black"),
        axis.text.y = element_text(size = 28, face = "bold", color = "black"),
        plot.title = element_text(family="Helvetica", face="bold", size = 40, hjust = 0),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15, hjust = 0),
        plot.caption = element_text(family = "Helvetica",size = 24, hjust = 0),
        plot.background = element_rect(fill = "bisque1", colour = "bisque1"),
        panel.background = element_rect(fill = "bisque1", colour = "bisque1"),
        plot.title.position = 'plot',
        plot.subtitle.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(5, 5, 5, 5),
        legend.position = "none")

#ggsave("sub_pro_7_agriculture_owid/images/continental_africa_only/continent_tomato_1.png", width = 12, height = 12, dpi = 300)

################################################################################
# Stacked Percentage Area Chart
################################################################################

label_df_tomato_percent_africa <- tomato_prod_clean_region_fao_africa_segment |>
  group_by(year) |>
  mutate(share = tomato_production_tonnes / sum(tomato_production_tonnes, na.rm = TRUE)) |>
  ungroup() |>
  filter(year == max(year)) |>
  mutate(region = factor(region, levels = rev(desired_order))) |>
  arrange(region) |>
  mutate(
    x_label = max(year),
    y_top = cumsum(share),
    y_bottom = y_top - share,
    y_mid = (y_bottom + y_top) / 2
  ) |>
  select(region, year, x_label, y_top, y_mid)


p4 <- tomato_prod_clean_region_fao_africa_segment |> 
  ggplot(aes(year, tomato_production_tonnes, fill = region, color = region)) +
  geom_area(position = "fill") +
  geom_text_repel(
    data = label_df_tomato_percent_africa,
    aes(x = x_label, y = y_mid, label = region, color = region),
    hjust = 0.1,
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
       y = "",
       title = "",
       caption = "") +
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

#ggsave("sub_pro_7_agriculture_owid/images/continental_africa_only_stack_perc/continent_tomato_1.png", width = 12, height = 12, dpi = 300)


(p3/p4) + plot_annotation() & theme(plot.margin = margin(0,0,0,0))
ggsave("sub_pro_7_agriculture_owid/images/continental_africa_only_combi/tomato.png", width = 12, height = 16, dpi = 300)
