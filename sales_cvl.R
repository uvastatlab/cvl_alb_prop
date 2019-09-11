# Cville Open Data Portal: http://opendata.charlottesville.org/
# Real Estate Sales and Base
# Question: Typical price of residential property in Charlottesville over time


# Load libraries
library(tidyverse)
library(jsonlite)
library(lubridate)


# Read sales and base data from open data portal API
sales_json <- fromJSON("https://opendata.arcgis.com/datasets/489adf140c174534a544136dc3e4cb90_3.geojson")
base_json <- fromJSON ("https://opendata.arcgis.com/datasets/75512c3ce3ef4e3188c163241032edb3_20.geojson")

# Extract data frame from list
sales <- sales_json$features$properties
head(sales)
base <- base_json$features$properties
head(base)


# To Do
# Join by ParcelNumber to get StateCode and filter for residential properties (1,2,3)

# Summarize/visualize price/sales amount by time
#   maybe by 5 year periods (rather than year), depends on count (or maybe rolling window?)
#   since 1990 (maybe later)
#   probably by StateCode (urban, suburban, multifamily)
#   may need to bring in Real Estate (Residential Details) to segment by type (UseCode) instead?

# Will get income data over appropriate time to look at distribution of sales over "affordable" (percent of income) limits 
#   probably from census, ideally via tidycensus/api (depends on time)
