# Homestays

# Load libraries
library(tidyverse)
library(scales)
library(leaflet)

# 1. Read ParcelInfo, CardLevel, OtherParcel
# data downloaded with get_alb_data.R
parcel <- read_tsv("albco_realestate_website/GIS_View_Redacted_ParcelInfo.txt")
card <- read_tsv("albco_realestate_website/GIS_CardLevelData_new.txt")
other <- read_tsv("albco_realestate_website/CityView_View_OtherParcelCharacteristics.txt")


# 2. Select key variables and join
parcel <- parcel %>% 
  select(ParcelID, Owner, OwnerJan1, PropStreet, City:LotSizeJan1, LandValue:TotalValue, OwnerAddress:Status)
card <- card %>% 
  select(TMP:CardType, YearBuilt, UseCode:FinSqFt, Bedroom, TotalRooms)
other <- other %>% 
  select(ParcelID, CensusBlockGroup:CensusTract, ZoningPrimary:ZoningSecondary)

prop <- parcel %>% 
  left_join(other, by = "ParcelID") %>% 
  left_join(card, by = c("ParcelID" = "TMP"))


# 3a. Filter for residential (full list) - propres (property residential)
#     uncertain of HouseStyle or UseCode will be more accurate here... starting with UseCode
#  b. filter for owner occupied (PropStreet and OwnerAddress don't use consistent naming of things like DR/DRIVE, LN/LANE, PL/PLACE... sigh)
#     can implement a more rigorous fuzzy matching, but will shortcut it here 
#     partial string match (if one string is shorter, m < n, match for m-1 characters only)

# a. Filter for residential (full list) - propres
#   define residential use from UseCode - propres
resuse <- c("3-4 Family", "Apartments", "Condo-Res-Garden", "Condo-Res-TH",
            "Doublewide", "Duplex", "Mobile Home", "Multi-Family", "Single Family",
            "Single Family-Rental", "Small Apartment")

propres <- prop %>% 
  filter(CardType == "R" & UseCode %in% resuse) 
# R = residential (vs. commercial) 
# resuse selected from available UseCodes -- probably not correct just yet

# b. identify owner-occupied (PropStreet = OwnerAddress) - propres
#    find minimum address length and detect matches up to min - 1 characters
propres <- propres %>% 
  mutate(addlen = pmin(str_length(propres$PropStreet), str_length(propres$OwnerAddress))) %>% 
  mutate(ownocc = str_detect(str_sub(PropStreet, end = (addlen-1)), str_sub(OwnerAddress, end= (addlen-1)))) %>% 
  filter(ownocc == TRUE)

# again, this is almost certainly imperfect for reasons I haven't yet thought through


# 4. filter further for homestay qualified - prophs (property homestay)
#    a. identify residential zoning and extract qualified homes
#    b. identify rural zoning and extract qualified homes
#    c. bind these back together for prophs

# a. identify residential zoning and extract qualified homes - hs_res
#    using ZoningPrimary as below -- check if this is correct
#    e.g., Neighborhood Model District? Planned Unit Development? Town of Scottsville
reszone <- c("Planned Residential Development", "R1 Residential", "R10 Residential",
             "R15 Residential", "R2 Residential", "R4 Residential", "R6 Residential",
             "Neighborhood Model District", "Planned Unit Development", "Town of Scottsville")

# filter for owner occupied, residential zoning, 3 or more bedrooms, single family detached
# is UseCode = Single Family equivalent to SFD? Is there a code somewhere that identifies this?
# test <- hs_res %>% filter(UseCode == "Single Family") %>% count(HouseStyle)
# No, this includes town homes, SFAs; can't locate code, so in meantime
# Use HouseStyle to identify SFD (thss doesn't yet exclude doublewide)

sfd <- propres %>% # get all HouseStyle values
  filter(UseCode == "Single Family") %>% 
  select(HouseStyle)
sfd <- distinct(sfd) # keep a observation of each value
sfd <- sfd %>% mutate(not = str_detect(sfd$HouseStyle, c("TH|SFA|NULL"))) %>% # identify obs if TH or SFA or NULL appears in string
  filter(not == FALSE) %>% select(HouseStyle) # and remove them
sfd <- as.vector(sfd$HouseStyle) # turn it into a vector for comparison

hs_res <- propres %>% 
  filter(ZoningPrimary %in% reszone, Bedroom > 2, 
         HouseStyle %in% sfd, CardNum == 1) 
# elected to keep only primary card

# b. identify rural zoning and extract qualified homes - hs_rur1, hs_rur2
#    using ZoningPrimary as below -- check if this is correct
#    e.g., Village Residential (indicated rural in GIS web)?
rurzone <- c("Village Residential", "Rural Areas")

# filter for owner occupied, rural zoning, acreage < 5
#   then 3 or more bedrooms, single family detached
hs_rur1 <- propres %>% 
  filter(ZoningPrimary %in% rurzone, LotSize < 5, 
         Bedroom > 2, HouseStyle %in% sfd, CardNum == 1) 
# elected to keep only primary card

# filter for owner occupied, rural zoning, acreage >= 5
#   then 6 or more bedrooms, single family detached
hs_rur2 <- propres %>% 
  filter(ZoningPrimary %in% rurzone, LotSize >= 5, 
         Bedroom > 5, HouseStyle %in% sfd, CardNum == 1) 
# elected to keep only primary card


# c. bind these back together for prophs
prophs <- bind_rows(hs_res, hs_rur1, hs_rur2)


# 5. Join coordinates to qualified propeties 
#    a. Load VGIN address file, select key variables, join to prophs by address
#    b. and map

vgin <- read_csv("VirginiaSiteAddressPoint.txt")
albvgin <- vgin %>% 
  filter(FIPS == 51003) %>% 
  select(ADDRNUM, STREET_NAME, STREET_TYPE, FULLADDR, LAT, LONG, STATE, ZIP_5)
rm(vgin)

prophsgeo <- prophs %>% 
 left_join(albvgin, by = c("PropStreet" = "FULLADDR")) 


# b. map
# test <- sample_n(prophsgeo, 10)
m1 <- leaflet(prophsgeo) %>%
  addProviderTiles("CartoDB.Positron") %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~LONG, lat = ~LAT,
                   radius = 3, fillOpacity = 0.5)
m1 # Print the map
# needs work 

m2 <- leaflet(prophsgeo) %>%
  addProviderTiles("CartoDB.Positron") %>% # Add default OpenStreetMap map tiles
  # addCircleMarkers(lng = ~LONG, lat = ~LAT, 
  #                  radius = 3, fillOpacity = 0.5) %>% 
  addMarkers(lng = ~LONG, lat = ~LAT,
             clusterOptions = markerClusterOptions())
m2 # Print the map



# 6. Add indicator to complete residential property data
propres <- propres %>% 
  mutate(hsqual = if_else(ParcelID %in% prophs$ParcelID, 1, 0))

propres %>% count(hsqual) %>% mutate(pct = percent(n/sum(n)))

# Generate summary statistics (e.g., mean/median/distribution of assessments as proxy for wealth) among homestay qualified/not
propres %>% group_by(hsqual) %>% 
  summarize(meanvalue = mean(TotalValue, na.rm = T), 
            medvalue = median(TotalValue, na.rm = T))

propres %>% 
  ggplot(aes(x = log(TotalValue), group = as.factor(hsqual),
             color = as.factor(hsqual))) + geom_density()

# Maybe get proportion qualified within census tracts? 
# Get ACS tract data and compare to median incomes within tract? Population data within tract? (can map these as well)


# Save work so far
saveRDS(propres, "albemarle_resid.rds")
saveRDS(prophs, "albemalre_hsqual.rds")
saveRDS(prophsgeo, "albemarle_hsqual_geo.rds")
