# Hass Consult Nairobi Land Price Analysis

library(tidyverse)
library(tidyr)
library(janitor)
library(readxl)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)
library(treemapify)
# install.packages("ggrepel")
library(ggrepel)
library(janitor)
library(zoo)

# 1) Load the required data

suburbs <- read_excel("Hass Suburbs Combined 2015-2022.xlsx")
satellite <- read_excel("Hass Satellite Combined 2015-2022.xlsx")


# 2) Clean the data

suburbs <- suburbs %>%
  clean_names()

satellite <- satellite %>%
  clean_names()

all_data <- rbind(suburbs, satellite)

all_data <- all_data %>%
  mutate(quarter_double = 2 * quarter) %>%
  mutate(quarter_year = paste(year, quarter_double, sep = "."))

all_data$average_price <- as.numeric(all_data$average_price)
all_data$x25th_percentile <- as.numeric(all_data$x25th_percentile)
all_data$x75th_percentile <- as.numeric(all_data$x75th_percentile)
all_data$quarter_year <- as.yearqtr(as.numeric(all_data$quarter_year))

all_data_avg_price <- all_data %>%
  select(location, quarter_year, average_price)

all_data_percentile_price <- all_data %>%
  select(location, quarter_year, x25th_percentile, x75th_percentile)

str(all_data_avg_price)
str(all_data_percentile_price)

# 3) Plot  the data / EDA

max <- all_data_avg_price %>% 
  group_by(location) %>%
  summarize(max = max(average_price, na.rm = TRUE))

location_1 <- c("Kiserian", "Kitengela", "Athi River", "Juja", "Thika")
location_2 <- c("Limuru", "Ongata Rongai", "Syokimau", "Ruiru", "Ngong")

location_3 <- c("Tigoni", "Mlolongo", "Kiambu", "Karen", "Langata")
location_4 <- c("Donholm", "Ridgeways", "Runda", "Loresho", "Kitisuru", "Ruaka")

location_5 <- c("Nyari", "Muthaiga", "Spring Valley", "Lavington", "Gigiri", "Eastleigh")
location_6 <- c("Kileleshwa", "Riverside", "Parklands", "Kilimani", "Westlands", "Upper Hill")

all_data_avg_price %>%
  filter(location %in% location_1) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 
  
all_data_avg_price %>%
  filter(location %in% location_2) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price %>%
  filter(location %in% location_3) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price %>%
  filter(location %in% location_4) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price %>%
  filter(location %in% location_5) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 

all_data_avg_price %>%
  filter(location %in% location_6) %>%
  ggplot(aes(quarter_year, average_price, color = location)) +
  geom_line() +
  theme_classic() + 
  scale_y_continuous(labels = scales::comma) 


