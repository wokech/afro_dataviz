# FRED data

# https://fredblog.stlouisfed.org/2024/12/leveraging-r-for-powerful-data-analysis/

# Using the fredo package for obtaining FRED data

# Install devtools if you haven't already
install.packages("devtools")

# Install fredo package from GitHub
devtools::install_github("manutzn/fredo")

library(fredo)
library(jsonlite)
library(dplyr)

api_key <- Sys.getenv("FRED_API_KEY")

# Function Signature

# fredo(api_key, series_ids, start_date, end_date)

# Parameters
# api_key: Your FRED API key as a string.
# series_ids: A character vector of FRED series IDs to retrieve (e.g., c(“GNPCA”, “UNRATE”)).
# start_date: The start date for data retrieval in ‘YYYY-MM-DD’ format.
# end_date: The end date for data retrieval in ‘YYYY-MM-DD’ format.

# Basic usage

# Define the series IDs and date range
series_ids <- c("GNPCA", "UNRATE")
start_date <- "1950-01-01"
end_date <- "2024-12-31"

fredo(api_key, series_ids, start_date, end_date)
