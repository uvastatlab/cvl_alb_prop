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


# Join by ParcelNumber to get StateCode and filter for residential properties (1,2,3)
sales2 <- sales %>% 
  left_join(base, by = "ParcelNumber")

res_code <- c("1.0 Residential (Urban)", "2.0 Residential (Suburban)", "3.0 Multi-Family")
resid <- sales2 %>% 
  filter(StateCode %in% res_code)

table(resid$StateCode) # hmmm...


# Summarize/visualize price/sales amount by time
#   by 5 year increments from 1990-2019
#   by StateCode (urban, suburban, multifamily)
#     may need to bring in Real Estate (Residential Details) to segment by type (UseCode) instead?
resid <- resid %>% 
  mutate(date = ymd_hms(SaleDate),
         year = year(date)) %>% 
  filter(year > 1989) %>% 
  mutate(yr5 = cut(year, b = 6),
         yr5mid = as.numeric(yr5),
         yr5mid = recode(yr5mid, 
                         `1` = 1992,
                         `2` = 1997,
                         `3` = 2002,
                         `4` = 2007,
                         `5` = 2012,
                         `6` = 2017))

resid %>% filter(SaleAmount == 0) %>% count() # 14,367 zeros!

resid %>% filter(SaleAmount > 0) %>% 
  summarize(mean(SaleAmount), median(SaleAmount), min(SaleAmount), max(SaleAmount))

# density of sale price
p <- ggplot(filter(resid, SaleAmount > 0), aes(x = SaleAmount, fill = StateCode)) + 
  scale_x_continuous(trans = "log10") +
  geom_density(alpha = 1/5) +
  facet_wrap(~yr5)
p

# get medians, means by yr5 and StateCode to add to plot
sale_summ <- resid %>% 
  group_by(yr5mid, StateCode) %>% 
  summarize(mean_sale = mean(SaleAmount), med_sale = median(SaleAmount))

p + geom_point(aes(x = med_sale, y = 1, color = StateCode), sale_summ)

ggplot(sale_summ, aes(x = yr5mid, y = mean_sale, color = StateCode)) + 
  geom_line() +
  geom_line(aes(x = yr5mid, y = med_sale, color = StateCode), linetype = 2)

# probably need to transform price into constant dollars?
# not sure how to treat the under-valued sales (friendly transfers?)


# Still to do
# Will get income data over appropriate time to look at distribution of sales over "affordable" (percent of income) limits 
#   probably from census, ideally via tidycensus/api (depends on time)
