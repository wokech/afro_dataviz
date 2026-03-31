# Accessing and Analyzing Economic Data in R with “fredr”

# https://worldpoliticsdatalab.org/tutorials/accessing-and-analyzing-economic-data-in-r-with-fredr/

# Working with the fredr Package:

# Install packages

install.packages ("fredr")
install.packages ("tidyverse")
install.packages ("scales")

# Load libraries

library (fredr)
library (tidyverse)
library (scales)

# Load the API key

fredr_set_key ("FRED_API_KEY")

pr1 <- fredr_series_search_text("Puerto Rico and manufacturing")

