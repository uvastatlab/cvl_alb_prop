# Albemarle Data: http://www.albemarle.org/department.asp?department=gds&relpage=3914
# Transfer History and Card
# Question: Typical price of residential property in Albemarle over time


# Load libraries
library(tidyverse)
library(lubridate)


# Read sales and card data downloaded with get_alb_data.R
card <- read_tsv("albco_realestate_website/GIS_CardLevelData_new.txt")
cardR <- card %>% filter(CardType == "R") # preety sure this is residential
# table(card$CommSectNum, card$CardType) # confirms

transfer <- read_tsv("albco_realestate_website/VISION_sales.txt")

transfer <- transfer %>% 
  mutate(date = mdy(saledate1),
         year = year(date))
table(transfer$year)


# Join by Parcel Number to get StateCode and filter fo
sum(transfer$mapblolot %in% card$TMP) # 115,121
sum(transfer$mapblolot %in% cardR$TMP) # 111,204

resid <- transfer %>% 
  inner_join(cardR, by = c("mapblolot" = "TMP")) 


# Summarize/visualize price/sales amount by time
#   by 5 year increments from 1990-2019
#   by truncated UseCode (urban, suburban, multifamily)
#     may need to bring in Real Estate (Residential Details) to segment by type (UseCode) instead?
usemulti <- c("3-4 Family", "Apartments", "Condo-Res-Garden", "Condo-Res-TH", "Duplex", "Multi-Family")
usesingle <- c("Doublewide", "Mobile Home", "Single Family", "Single Family-Rental", "Small Apartment")
resid <- resid %>% 
  filter(year > 1989) %>% 
  mutate(use = if_else(UseCode %in% usemulti, "multi", 
                       if_else(UseCode %in% usesingle, "single", "other"))) %>% 
  mutate(yr5 = cut(year, b = 6),
         yr5mid = as.numeric(yr5),
         yr5mid = recode(yr5mid, 
                         `1` = 1992,
                         `2` = 1997,
                         `3` = 2002,
                         `4` = 2007,
                         `5` = 2012,
                         `6` = 2017))
table(resid$use)

resid %>% filter(saleprice == 0) %>% count() # 32,589 zeros!

resid %>% filter(saleprice > 0) %>% 
  summarize(mean(saleprice), median(saleprice), min(saleprice), max(saleprice))

# density of sale price
p <- ggplot(filter(resid, saleprice > 0), aes(x = saleprice, fill = use)) + 
  scale_x_continuous(trans = "log10") +
  geom_density(alpha = 1/5) +
  facet_wrap(~yr5)
p

# get medians, means by yr5 and StateCode to add to plot
sale_summ <- resid %>% 
  group_by(yr5mid, use) %>% 
  summarize(mean_sale = mean(saleprice), med_sale = median(saleprice))

p + geom_point(aes(x = med_sale, y = 1, color = use), sale_summ)

ggplot(sale_summ, aes(x = yr5mid, y = mean_sale, color = use)) + 
  geom_line() +
  geom_line(aes(x = yr5mid, y = med_sale, color = use), linetype = 2)

# probably need to transform price into constant dollars?
# not sure how to treat the under-valued sales (friendly transfers?)


# Still to do
# Will get income data over appropriate time to look at distribution of sales over "affordable" (percent of income) limits 
#   probably from census, ideally via tidycensus/api (depends on time)

