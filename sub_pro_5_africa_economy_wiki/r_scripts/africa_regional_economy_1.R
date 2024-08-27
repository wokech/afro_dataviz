# An analysis of Regional Economies in Africa

# 1) List of counties of Kenya by GDP (2018/2019)
# Website: https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP

# 2) List of Nigerian states by GDP (2021/2022)
# Website: https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_GDP

# 3) List of regions of Tanzania by GDP (2018/2019)
# Website: https://en.wikipedia.org/wiki/List_of_regions_of_Tanzania_by_GDP

# 4) List of South African provinces by gross domestic product (2022)
# Website: https://en.wikipedia.org/wiki/List_of_South_African_provinces_by_gross_domestic_product

# 5) List of governorates of Egypt by GDP (2021)
# Website: https://en.wikipedia.org/wiki/List_of_governorates_of_Egypt_by_GDP

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)
library(scales)

############KENYA#################

# (B) Get the data from Wikipedia

link_kenya <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP"
kenya_county <- link_kenya %>%
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

write.csv()
write.csv()

# Plot 1 - County GDP

ggplot(kenya_county_GDP_clean, aes(reorder(county, +gdp_in_us_millions_ppp), gdp_in_us_millions_ppp, fill = county)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_in_us_millions_ppp) > 4800) + 
  scale_fill_brewer(palette="Blues") +
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 

# Plot 2 - County GDP per Capita

# Also plot a national average line at USD 3224

ggplot(kenya_county_GDP_capita_clean, aes(reorder(county, +gdp_per_capita_in_us_ppp), gdp_per_capita_in_us_ppp, fill = county)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_per_capita_in_us_ppp) > 4800) + 
  scale_fill_brewer(palette="Blues") +
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 

############TANZANIA#################

# (B) Get the data from Wikipedia

link_tanzania <- "https://en.wikipedia.org/wiki/List_of_regions_of_Tanzania_by_GDP"
tanzania_region <- link_tanzania %>%
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

write.csv()
write.csv()

# Plot 1 - County GDP

ggplot(tanzania_region_GDP_clean, aes(reorder(region, +gdp_in_us_million_ppp), gdp_in_us_million_ppp, fill = region)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_in_us_million_ppp) > 9700) +
  scale_fill_brewer(palette="Blues") +
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 

# Plot 2 - County GDP per Capita

ggplot(tanzania_region_GDP_capita_clean, aes(reorder(region, +gdp_per_capita_in_us_ppp), gdp_per_capita_in_us_ppp, fill = region)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_per_capita_in_us_ppp) > 4000) +
  scale_fill_brewer(palette="Blues") +
  labs(x = "Region",
       y = "GDP per capita (US$)",
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 

############NIGERIA#########################

# (B) Get the data from Wikipedia

link_nigeria <- "https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_GDP"
nigeria_state <- link_nigeria %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

nigeria_state_GDP <- nigeria_state[[1]]

# (C) Clean the data, fix columns and county labels

nigeria_state_GDP_clean <- nigeria_state_GDP %>%
  clean_names() %>%
  mutate(state = str_remove_all(state, " State| state"))

# parsing out the number is very critical as a simple conversion using 
# as._____() will not work

str(nigeria_state_GDP_clean)

nigeria_state_GDP_clean$gdp_tril <- parse_number(nigeria_state_GDP_clean$gdp_tril)
nigeria_state_GDP_clean$gdp_bil_us <- parse_number(nigeria_state_GDP_clean$gdp_bil_us)
nigeria_state_GDP_clean$gdp_ppp_bil_int <- parse_number(nigeria_state_GDP_clean$gdp_ppp_bil_int)
nigeria_state_GDP_clean$gdp_per_capita_us <- parse_number(nigeria_state_GDP_clean$gdp_per_capita_us)
nigeria_state_GDP_clean$gdp_per_capita_ppp_int <- parse_number(nigeria_state_GDP_clean$gdp_per_capita_ppp_int)

str(nigeria_state_GDP_clean)

write.csv()

# Plot 1

ggplot(nigeria_state_GDP_clean, aes(reorder(state, +gdp_ppp_bil_int), gdp_ppp_bil_int, fill = state)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_ppp_bil_int) > 40) + 
  scale_fill_brewer(palette="Blues") +
  labs(x = "County",
       y = "GDP (billions of US$)",
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 

# Plot 2 - County GDP per Capita

ggplot(nigeria_state_GDP_clean, aes(reorder(state, +gdp_per_capita_ppp_int), gdp_per_capita_ppp_int, fill = state)) +
  geom_bar(stat = "identity") + coord_flip() +
  gghighlight(max(gdp_per_capita_ppp_int) > 6000) +
  scale_fill_brewer(palette="Blues") +
  labs(x = "Region",
       y = "GDP per capita (US$)",
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
        plot.background = element_rect(fill = "bisque", colour = "bisque"),
        panel.background = element_rect(fill = "bisque", colour = "bisque"),
        legend.title = element_blank(),
        legend.position = "none") 


##########SOUTH AFRICA###################

# (B) Get the data from Wikipedia

link_kenya <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP"
kenya_county <- link_kenya %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

kenya_county_GDP <- kenya_county[[1]]
kenya_county_GDP_capita <- kenya_county[[2]]

##########EGYPT##################

# (B) Get the data from Wikipedia

link_kenya <- "https://en.wikipedia.org/wiki/List_of_counties_of_Kenya_by_GDP"
kenya_county <- link_kenya %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(fill = TRUE)

kenya_county_GDP <- kenya_county[[1]]
kenya_county_GDP_capita <- kenya_county[[2]]