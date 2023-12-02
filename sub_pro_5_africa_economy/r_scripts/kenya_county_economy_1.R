# An analysis of Kenyan counties by GDP in 2017 to 2018

## Include an explanation of data and a map....
# year??

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(scales)

# To export the images
# camcorder::gg_record()

# library(camcorder)

# gg_record(
#   dir = 'sub_pro_5_africa_economy/images',
#   width = 12,
#   height = 12 * 9 / 16,
#   dpi = 300,
#   bg = 'white'
# )

# (B) Get the data from Wikipedia

link <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP"
kenya_county <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

kenya_county_GDP <- kenya_county[[1]]
kenya_county_GDP_capita <- kenya_county[[2]]

# (C) Clean the data, fix columns and county labels

kenya_county_GDP_clean <- kenya_county_GDP %>%
  clean_names() %>%
  mutate(county = str_remove_all(county, " County| county")) %>%
  filter(county != "Kenya") 

# parsing out the number is very critical as a simple conversion using 
# as._____() will not work

kenya_county_GDP_clean$gdp_in_k_sh_millions <- parse_number(kenya_county_GDP_clean$gdp_in_k_sh_millions)
kenya_county_GDP_clean$gdp_in_us_millions_ppp <- parse_number(kenya_county_GDP_clean$gdp_in_us_millions_ppp)

str(kenya_county_GDP_clean)

kenya_county_GDP_capita_clean <- kenya_county_GDP_capita  %>%
  clean_names() %>%
  mutate(county = str_remove_all(county, " County| county")) %>%
  filter(county != "Kenya") 

kenya_county_GDP_capita_clean$gdp_per_capita_in_k_sh <- parse_number(kenya_county_GDP_capita_clean$gdp_per_capita_in_k_sh)
kenya_county_GDP_capita_clean$gdp_per_capita_in_us_ppp <- parse_number(kenya_county_GDP_capita_clean$gdp_per_capita_in_us_ppp)

str(kenya_county_GDP_capita_clean)

# Plot 1

ggplot(kenya_county_GDP_clean, aes(reorder(county, +gdp_in_us_millions_ppp), gdp_in_us_millions_ppp, fill = county)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_in_us_millions_ppp) > 4800) + 
  scale_fill_brewer(palette="OrRd") +
  labs(x = "County",
       y = "GDP (millions of US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

# Plot 2

# Also plot a national average line at USD 3224

ggplot(kenya_county_GDP_capita_clean, aes(reorder(county, +gdp_per_capita_in_us_ppp), gdp_per_capita_in_us_ppp, fill = county)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_per_capita_in_us_ppp) > 4800) + 
  scale_fill_brewer(palette="OrRd") +
  labs(x = "County",
       y = "GDP per capita (US$)",
       title = "",
       subtitle = "",
       caption = "") +
  theme_classic() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 
