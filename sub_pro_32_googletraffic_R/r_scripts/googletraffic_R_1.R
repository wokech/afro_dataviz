# Google Traffic R

# Website: https://blogs.worldbank.org/en/opendata/introducing-googletraffic-r-package-new-tool-measure-congestion-across-large-spatial-areas

# install.packages("googletraffic")

# install.packages("devtools")
# devtools::install_github("dime-worldbank/googletraffic")

# Quickstart 
# Setup

## Load package
library(googletraffic)

## Load additional packages to run below examples
library(ggplot2)
library(dplyr)
library(raster)

## Set API key
google_key <- "GOOGLE-KEY-HERE"