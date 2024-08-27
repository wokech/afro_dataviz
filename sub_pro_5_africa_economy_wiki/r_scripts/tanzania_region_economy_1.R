# An analysis of Tanzanian regions by GDP

## Include an explanation of data and a map....
# year??

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(ggthemes)
library(scales)


# (B) Get the data from Wikipedia

link <- "https://en.wikipedia.org/wiki/List_of_regions_of_Tanzania_by_GDP"
tanzania_region <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

tanzania_region_GDP <- tanzania_region[[1]]
tanzania_region_GDP_capita <- tanzania_region[[2]]

# (C) Clean the data, fix columns and county labels

tanzania_region_GDP_clean <- tanzania_region_GDP %>%
  clean_names() %>%
  mutate(region = str_remove_all(region, " Region| region")) %>%
  filter(region != "Tanzania (Mainland)") 

# parsing out the number is very critical as a simple conversion using 
# as._____() will not work

tanzania_region_GDP_clean$gdp_in_t_sh_million <- parse_number(tanzania_region_GDP_clean$gdp_in_t_sh_million)
tanzania_region_GDP_clean$gdp_in_us_million_ppp <- parse_number(tanzania_region_GDP_clean$gdp_in_us_million_ppp)

str(tanzania_region_GDP_clean)

tanzania_region_GDP_capita_clean <- tanzania_region_GDP_capita  %>%
  clean_names() %>%
  mutate(region = str_remove_all(region, " Region| region")) %>%
  filter(region != "Tanzania (Mainland)")

tanzania_region_GDP_capita_clean$gdp_per_capita_in_t_sh <- parse_number(tanzania_region_GDP_capita_clean$gdp_per_capita_in_t_sh)
tanzania_region_GDP_capita_clean$gdp_per_capita_in_us_ppp <- parse_number(tanzania_region_GDP_capita_clean$gdp_per_capita_in_us_ppp)

str(tanzania_region_GDP_capita_clean)

# Plot 1

ggplot(tanzania_region_GDP_clean, aes(reorder(region, +gdp_in_us_million_ppp), gdp_in_us_million_ppp, fill = region)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_in_us_million_ppp) > 9700) +
  scale_fill_brewer(palette="OrRd") +
  labs(x = "Region",
       y = "GDP (millions of US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 
