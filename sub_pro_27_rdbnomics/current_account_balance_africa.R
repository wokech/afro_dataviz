# Benjamin Braun Lessons
# https://benjaminbraun.org/teaching/financial-data/dbnomics_r#/title-slide

# Current Account Balance

# install.packages("rdbnomics")
# install.packages("thematic")

library(tidyverse) #https://www.tidyverse.org/
library(rdbnomics) #https://git.nomics.world/dbnomics/rdbnomics
library(janitor) #https://github.com/sfirke/janitor
library(thematic) #For styling plots: https://rstudio.github.io/thematic/index.html

# 1) Current account
#    Exports of goods and services + Receipts of income on ....-owned assets abroad Minus
#    Imports of goods and services + Payments of income on foreign-owned assets in the 
#    United States + Unilateral current transfers

# EAC 

bop_imf_eac <- rdb("IMF", "BOP", mask = "A.TZ+KE+UG+SS+RW+BI+CD.BCA_BP6_USD") 

bop_imf_eac <- as_tibble(bop_imf_eac) |>
  clean_names()

# EAC Plot 

ggplot(
  data = bop_imf_eac,
  aes(x = period, 
      y = value)) +
  geom_col() +
  labs(title = 'CA balance ...') + 
  facet_wrap("ref_area")

# Top Economies Plot

bop_imf_top_econ_africa <- rdb("IMF", "BOP", mask = "A.ZA+EG+DZ+NG+MA+KE+AO+GH+ET.BCA_BP6_USD") 

bop_imf_top_econ_africa <- as_tibble(bop_imf_top_econ) |>
  clean_names() |>
  filter(period > 1980)

# EAC Plot 

ggplot(
  data = bop_imf_top_econ_africa,
  aes(x = period, 
      y = value)) +
  geom_col() +
  labs(title = 'CA balance ...') + 
  facet_wrap("ref_area")
