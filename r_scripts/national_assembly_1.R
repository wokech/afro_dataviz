## Kenyan parliament composition

# 1) Libraries and packages

library(tidyverse)
library(tidyr)
library(janitor)
library(tidyr)
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

# 2) Load the required datasets

parl_composition <- read_excel("Party_Membership_Sep_2022.xlsx")
parl_mps <- read_excel("Parliament_Sep_2022.xlsx")

# 3) Clean data filter out the relevant datasets

parl_composition <- parl_composition %>%
  clean_names()
parl_mps <- parl_mps %>%
  clean_names()

# Filter out the relevant datasets

parl_mps_regular <- parl_mps  %>%
  filter(!str_detect(constituency, "CWR|Nominated"))

parl_mps_nom <- parl_mps %>%
  filter(constituency == "Nominated")

parl_mps_women_rep <- parl_mps  %>%
  filter(str_detect(constituency, "CWR"))
