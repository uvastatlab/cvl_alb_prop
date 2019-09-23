# Pull data from county website
# September 11, 2019

# Albemarle County Real Estate Data
# http://www.albemarle.org/department.asp?department=gds&relpage=3914

library(tidyverse)
library(lubridate)
# dir.create("albco_realestate_website") # first time
setwd("albco_realestate_website")

# card data
link <- "http://www.albemarle.org/gds/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link), list = TRUE) # list files, but don't extract
unzip(basename(link), "GIS_CardLevelData_new.txt") # extract file to working directory
card_level <- read_tsv("GIS_CardLevelData_new.txt")
names(card_level)

# parcel data
link2 <- "http://www.albemarle.org/gds/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2), list = TRUE) # list files, but don't extract
unzip(basename(link2), "GIS_View_Redacted_ParcelInfo.txt") # extract file to working directory
parcel_level <- read_tsv("GIS_View_Redacted_ParcelInfo.txt")
names(parcel_level)

# improvements data
link3 <- "http://www.albemarle.org/gds/gisdata/CAMA/GIS_CardLevelData_Improvements_TXT.zip"
download.file(link3, destfile = basename(link3))
unzip(basename(link3), list = TRUE) # list files, but don't extract
unzip(basename(link3), "GIS_CardLevelData_Improvements.txt") # extract file to working directory
improve <- read_tsv("GIS_CardLevelData_Improvements.txt")
names(improve)

# transfer history
link4 <- "http://www.albemarle.org/gds/gisdata/CAMA/VISION_sales_TXT.zip"
download.file(link4, destfile = basename(link4))
unzip(basename(link4), list = TRUE) # list files, but don't extract
unzip(basename(link4), "VISION_sales.txt") # extract file to working directory
sales <- read_tsv("VISION_sales.txt")
names(sales)
sales <- sales %>% 
  mutate(date = mdy(saledate1),
         year = year(date))
table(sales$year)

# other parcel characteristics
link5 <- "http://www.albemarle.org/gds/gisdata/CAMA/CityView_View_OtherParcelCharacteristics_TXT.zip"
download.file(link5, destfile = basename(link5))
unzip(basename(link5), list = TRUE) # list files, but don't extract
unzip(basename(link5), "CityView_View_OtherParcelCharacteristics.txt") # extract file to working directory
other <- read_tsv("CityView_View_OtherParcelCharacteristics.txt")
names(other)

# physical address file for coordinates
link6 <- "http://www.albemarle.org/gds/gisdata/Addresses/GIS_Addressing_TXT.zip"
download.file(link6, destfile = basename(link6))
unzip(basename(link6), list = TRUE) # list files, but don't extract
unzip(basename(link6), "GIS_Addressing.txt") # extract file to working directory
address <- read_csv("GIS_Addressing.txt")
names(address)


# Virginia address file
setwd("..")
link7 <- "https://ftp.vgingis.com/Download/BaseMapData/TXT/VirginiaSiteAddressPoint.txt.zip"
download.file(link7, destfile = basename(link7))
unzip(basename(link7), list = TRUE) # list files, but don't extract
unzip(basename(link7), "VirginiaSiteAddressPoint.txt") # extract file to working directory
vgin <- read_csv("VirginiaSiteAddressPoint.txt")
names(vgin)





