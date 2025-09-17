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

bop_imf <- rdb("IMF", "BOP", mask = "A.TZ.BCA_BP6_USD") 

bop_imf <- as_tibble(bop_imf)

bop_imf |> 
  select(7, 12, 14, 15, 18)


bop_imf <- clean_names(bop_imf)

#Inspect variable names again
bop_imf |> 
  select(7, 12, 14, 15, 18)


ggplot(
  data = bop_imf,
  aes(x = period, 
      y = value)
) +
  geom_col() +
  labs(title = 'CA balance ...')

