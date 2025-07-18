# Dry Bean production

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

bean_prod <- read.csv("https://ourworldindata.org/grapher/bean-production.csv?v=1&csvType=full&useColumnShortNames=true",
                       na.strings = "")

# Save the data
write.csv(bean_prod, "sub_pro_7_agriculture_owid/datasets/bean-production-tonnes.csv",
          row.names = FALSE)

# Read in the data
bean_prod <- read.csv("sub_pro_7_agriculture_owid/datasets/bean-production-tonnes.csv")

# Clean the column headings

bean_prod_clean <- bean_prod %>%
  clean_names() 

# Change the column title names

bean_prod_clean <- bean_prod_clean %>%
  rename("region" = "entity",
         "bean_production_tonnes" = "beans_dry_00000176_production_005510_tonnes") 

# Filter by region

bean_prod_clean_region <- bean_prod_clean %>%
  filter(is.na(code)) %>%
  select(c(1,3,4)) 

# Filter by FAO region

bean_prod_clean_region_fao <- bean_prod_clean_region %>%
  filter(grepl('(FAO)', region))

# Filter by non-FAO region

bean_prod_clean_region_non_fao <- bean_prod_clean_region %>%
  filter(!grepl('(FAO)', region))

# 3) Continental (Non-FAO) bean production

# a) Stacked area chart

bean_prod_clean_region_non_fao_continent <- bean_prod_clean_region_non_fao %>%
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

bean_prod_clean_region_non_fao_continent <- bean_prod_clean_region_non_fao_continent %>%
  mutate(region = factor(region, levels = desired_order)) %>%
  arrange(desc(region))

#  calculate cumulative positions for label placement

label_df_bean <- bean_prod_clean_region_non_fao_continent %>%
  filter(year == max(year)) %>%
  mutate(region = factor(region, levels = rev(desired_order))) %>%
  arrange(region) %>%
  mutate(x_label = max(year),
         y_top = cumsum(bean_production_tonnes),
         y_bottom = y_top - bean_production_tonnes,
         y_mid = (y_bottom + y_top) / 2) %>%
  select(region, year, x_label, y_top, y_mid) 

# plot the stack area chart

bean_prod_clean_region_non_fao_continent %>% 
  ggplot(aes(year, bean_production_tonnes, fill = region, label = region, color = region)) +
  geom_area() +
  geom_text_repel(
    data = label_df_bean,
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
       y = "Bean Production\n(Millions of Tonnes)",
       title = "Africa contributed to slightly over a quarter of\nglobal dry bean production in 2020",
       subtitle = "",
       caption = "Data Source: Our World in Data | FAO | World Bank") +
  theme_classic() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020), labels = c("1960", "1980", "2000", "2020")) +
  scale_y_continuous(limits = c(0, 30000000), labels  = 
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

ggsave("sub_pro_7_agriculture_owid/images/continental/continent_bean_1.png", width = 12, height = 12, dpi = 72)


bean_prod_clean_region_non_fao_continent %>%
  filter(year == 2020) %>%
  mutate(percent = 100 * bean_production_tonnes/sum(bean_production_tonnes))


  