# Malaria death rates
# Racing bar chart

## Example
## https://www.infoworld.com/article/3633448/easy-racing-bar-charts-in-r-with-ddplot.html

# 1) Load the packages required for the maps

# Solve package loading issues with options(timeout = 600) 
# increase download length time

#install.packages("sf")
library(sf) # simple features
library(tidyverse)
library(ggplot2)
library(ggrepel)
#install.packages("devtools")
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(rKenyaCensus)
library(patchwork)
library(janitor)
#install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(ggthemes)
library(scales)
library(readxl)
library(readr)
#install.packages("ddplot")
library(ddplot)

# 2) Load the data

malaria_death <- read_csv("sub_pro_7_malaria_owid/processed_tables/malaria_death_rates.csv")

# Clean the column headings

malaria_death_clean <- malaria_death %>%
  clean_names()

# Change the column title names

malaria_death_clean <- malaria_death_clean %>%
  rename("country" = "entity",
         "death_rate" = "deaths_malaria_sex_both_age_age_standardized_rate") %>%
  select(c(1,3,4)) 

# Get the EAC malaria death rates

malaria_death_clean_eac <- malaria_death_clean %>%
  filter(country %in% c("Kenya", "Uganda", "Tanzania", 
           "Burundi", "Rwanda", "South Sudan", 
           "Democratic Republic of Congo"))

## For Flourish pivot the data

malaria_death_clean_eac_wide <- malaria_death_clean_eac %>%
    pivot_wider(names_from = year, values_from = death_rate)

flags <- c("https://public.flourish.studio/country-flags/svg/bi.svg",
           "https://public.flourish.studio/country-flags/svg/cd.svg",
           "https://public.flourish.studio/country-flags/svg/ke.svg",
           "https://public.flourish.studio/country-flags/svg/rw.svg",
           "https://public.flourish.studio/country-flags/svg/ss.svg",
           "https://public.flourish.studio/country-flags/svg/tz.svg",
           "https://public.flourish.studio/country-flags/svg/ug.svg")

malaria_death_clean_eac_wide$flags <- flags

write.csv(malaria_death_clean_eac_wide, file = "sub_pro_7_malaria_owid/processed_tables/malaria_death_rates_wide_fluorish.csv")

# Get the EAC malaria mean death rates


# 3) Draw the plots

# A) Racing bar charts

# Static Bar chart for the average death rates

malaria_death_clean_eac %>% 
  group_by(country) %>%
  summarise(mean_death_rate = mean(death_rate)) %>%
  barChart(
    x = "country",
    y = "mean_death_rate",
    fill = "blue",
    stroke = "black",
    title = "",
    sort = "descending"
  )

# Racing Bar chart for the average death rates

malaria_death_clean_eac %>%
  barChartRace(
    x = "death_rate",
    y = "country",
    time = "year",
    title = ""
  )

# Customized Racing Bar chart for the average death rates

malaria_death_clean_eac %>%
  barChartRace(
    x = "death_rate",
    y = "country",
    time = "year",
    xtitle = "Country",
    title = "Malaria Death Rates",
    frameDur = 750,
    colorCategory = "Dark2",
    panelcol = "white",
    bgcol = "#DCDCDC",  # a light gray
    xgridlinecol = "#EBEBEBFF",
    timeLabelOpts = list(size = 16)
  )


malaria_death_clean_eac %>%
  barChartRace(
  x = "death_rate",
  y = "country",
  time = "year",
  ease = "Linear",
  frameDur = 1000,
  transitionDur = 1000,
  colorCategory = "Accent",
  sort = "descending",
  paddingWidth = 0.5,
  xFontSize = 20,
  yFontSize =14,
  xticks = 10,
  xtitle = "Number of deaths per 100,000 individuals",
  xtitleFontSize = 24,
  ytitle = NULL,
  ytitleFontSize = 24,
  title = "Death rate from malaria",
  titleFontSize = 32,
  stroke = "black",
  strokeWidth = NULL,
  font = "Verdana, Geneva, Tahoma, sans-serif",
  bgcol = "bisque",
  panelcol = "bisque",
  xgridlinecol = "bisque",
  opacity = 1,
  timeLabel = TRUE,
  timeLabelOpts = list(size = 50, prefix = "", suffix = "", xOffset = 0.5, yOffset = 1),
  width = NULL,
  height = NULL
)

# B) Interactive visualizations with ggiraph
# https://albert-rapp.de/posts/ggplot2-tips/17_ggiraph/17_ggiraph.html

color_palette <- thematic::okabe_ito(7)
names(color_palette) <- unique(malaria_death_clean_eac$country)
base_size <- 18

line_chart <- malaria_death_clean_eac %>%
  ggplot(aes(x = year, y = death_rate, col = country)) +
  geom_line(linewidth = 2.5) +
  geom_point(size = 4) +
  theme_minimal(base_size = base_size) +
  labs(
    x = element_blank(),
    y = '',
    title = ''
  ) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'left',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) +
  scale_color_manual(values = color_palette)
line_chart

# Add interactivity

library(ggiraph)

line_chart <- malaria_death_clean_eac %>% 
  ggplot(aes(x = year, y = death_rate, col = country)) +
  geom_line_interactive(linewidth = 2.5) +
  geom_point_interactive(size = 4) +
  theme_minimal(base_size = base_size) +
  labs(
    x = element_blank(),
    y = '',
    title = ''
  ) +
  theme(
    text = element_text(
      color = 'grey20'
    ),
    legend.position = 'left',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot'
  ) +
  scale_color_manual(values = color_palette)

girafe(ggobj = line_chart)

girafe(
  ggobj = line_chart,
  options = list(
    opts_hover(css = ''), ## CSS code of line we're hovering over
    opts_hover_inv(css = "opacity:0.1;"), ## CSS code of all other lines
    opts_sizing(rescale = FALSE) ## Fixes sizes to dimensions below
  ),
  height_svg = 6,
  width_svg = 9
)

# Racing Bar Chart by Eva Maerey
# https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#1

malaria_death_clean_eac %>%  
  # for each year we assign a rank
  group_by(year) %>%  
  arrange(year, -death_rate) %>%  
  # assign ranking
  mutate(rank = 1:n()) %>%  
  filter(rank <= 10) ->  
  ranked_by_year

my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))

ranked_by_year %>%  
  ggplot() +  
  aes(xmin = 0 ,  
      xmax = death_rate) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  facet_wrap(~ year) +  
  geom_rect(alpha = .7) +  
  aes(fill = country) +  
  scale_fill_viridis_d(option = "magma",  
                       direction = -1) +  
  scale_x_continuous() +  
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = country),  
            x = -50) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = '') +  
  labs(y = "") +  
  my_theme ->  
  my_plot
my_plot

library(gganimate) 


my_plot +  
  facet_null() +  
  scale_x_continuous() +  
  geom_text(x = 1000 , y = -10,  
            family = "Times",  
            aes(label = as.character(year)),  
            size = 30, col = "grey18") +  
  aes(group = country) +  
  gganimate::transition_time(year)

# Racing Bar Chart
# https://r-graph-gallery.com/288-animated-barplot-transition.html

# libraries  
library(ggplot2)
library(gganimate)


# Basic barplot:
malaria_death_clean_eac %>%
ggplot(aes(x=death_rate, y=country, fill=country)) + 
  geom_bar(stat='identity') +
  facet_wrap(~year)

# Make a ggplot, but add frame=year: one image per year

malaria_death_clean_eac %>%
  ggplot(aes(x=death_rate, y=country, fill=country)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')

# Save at gif:
anim_save("288-animated-barplot-transition.gif")

