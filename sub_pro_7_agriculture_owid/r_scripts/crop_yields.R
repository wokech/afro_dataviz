# Crop Yields

# 1) Load the Required Libraries

# Solve package loading issues with options(timeout = 600) 
# increase download length time

library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(tidyverse)
library(tidyr)
library(tidyr)
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
# install.packages("ggrepel")
library(ggrepel)
library(patchwork)
library(stringr)
library(magick)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)

# 2) Data Cleaning and Organization

# Load and clean the required data sets

crop_yields <- read_csv("sub_pro_13_agriculture_owid/processed_tables/global-food.csv")

# Clean the column headings

crop_yields_clean <- crop_yields %>%
  clean_names()

# Change the column title names

fish_seafood_1_clean <- fish_seafood_1_clean %>%
  rename("country" = "entity",
         "fish_and_seafood_production_tonnes" = "fish_and_seafood_00002960_production_005511_tonnes") 
