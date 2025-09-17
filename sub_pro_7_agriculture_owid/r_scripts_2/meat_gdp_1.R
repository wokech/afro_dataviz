# Meat Supply vs GDP per Capita

# Data Sources: OWID/FAO/WB

library(jsonlite)

# Fetch the data
meat_gdp <- read.csv("https://ourworldindata.org/grapher/meat-consumption-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
                     na.strings = "")

# Save the data
write.csv(meat_gdp, "sub_pro_7_agriculture_owid/datasets/meat-consumption-vs-gdp-per-capita.csv",
          row.names = FALSE)

# Read the data
meat_gdp <- read.csv("sub_pro_7_agriculture_owid/datasets/meat-consumption-vs-gdp-per-capita.csv")

# Filter out Africa