# Permitting Script
# 2019: UPDATE GOOGLE SHEET SECTION. 

library(tidyr)
library(dplyr)
library(lubridate)
library(tidyverse)


curr_yr <- 2020
load(here::here("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2020_Version13.Rdata"))

#Set some default parameters
Datum = "WGS84"
CoordDeterm = "GPS"
WaterBody = "Prudhoe Bay"
permitholder = "Duncan G Green"
CollectionMeth = "Fyke Net"
LifeStage = "juvenile/adult"


# Set up the location so that we can join this to catches
Locations <- data.frame(Station = c("214", "218", "220", "230"),
                       StnName = c("Niakuk", "West Beach", "West Dock", "Endicott"),
                       Lat = c(70.34699, 70.358301, 70.38262, 70.309083),
                       Long = c(-148.209819, -148.473126, -148.55571, -147.884585))


permit.leng <- all.len %>% filter(Year == curr_yr) %>% 
  mutate(Net = paste0(Station, Side),
         Station = as.character(Station)) %>%
  rename(LG = Group, Mort_indiv = Code)
  

permitcatch <- allcatch %>% filter(Year == curr_yr) %>% 
  #group_by(EndDate, Net, Species, LG) %>% summarise(totcount_catch = sum(totcount)) %>%
  mutate(Station = substr(Net, 1, 3)) %>%
  rename(totcount_catch = totcount) 
  #left_join(Locations %>% dplyr::select(-StnName), by = c("Station" = "Station")) 

leng.totals <- permit.leng %>% group_by(EndDate, Net, Species, LG) %>%
  summarise(totcount_leng = sum(totcount, na.rm = TRUE), morts_leng = sum(Mort_indiv, na.rm = TRUE))


#This takes the total catches and subtracts out the measured fish
permit_unmeas <- permitcatch %>% full_join(leng.totals, by = c("EndDate" = "EndDate", "Net" = "Net", 
                                                            "Species" = "Species", "LG" = "LG")) %>%
  replace_na(list(totcount_leng = 0, morts_leng = 0)) %>% # replace the NAs in mort cols with zeros
  mutate(unmeasured = totcount_catch - totcount_leng, # calc total unmeasured fish
         unmeasured_morts = Morts - morts_leng) %>%   # calc total unmeasured morts
  filter(unmeasured != 0 | is.na(unmeasured)) %>% # omit zeros (already all accounted for) & inc NAs (unmeasured species)
  mutate(unmeasured = if_else(is.na(unmeasured), totcount_catch, unmeasured)) %>% # replace NAs with total catch
  group_by(EndDate, Net, Station, Species) %>% 
  summarise(unmeasured = sum(unmeasured, na.rm = TRUE), 
            Morts_total = sum(unmeasured_morts, na.rm = TRUE)) #add all LGs together




permit_all <- bind_rows(permit.leng, permit_unmeas) %>% 
   dplyr::select(EndDate, Station, Net, Species, LG, Length, unmeasured, Mort_indiv, Morts_total) %>% 
   arrange(EndDate, Net, Species, LG) %>%
   left_join(Locations, by = c("Station" = "Station")) %>%
   mutate(LocationID = paste0(Net, " - ", StnName)) %>%
   dplyr::select(-StnName, LG) #drop some unneed vars
 
#permit_all %>% View()

# Now conditionally add disposition 1, depending on if there were morts
permit_all <- permit_all %>% mutate(Disp1 = if_else(!is.na(Length), 
                                                    if_else(is.na(Mort_indiv), "Measured and Released", "Unitended Mortality"), 
                                                    "ID'ed and Released"),
                                    Disp2 = if_else(Morts_total > 0, "Unitended Mortality", "")) %>%
  rename(AddlCount1 = unmeasured, AddlCount2 = Morts_total)





###############
# Now because previous years of the database didn't have otolith/frozen/etc columns, the 2018 year doesn't have
# that info saved either. So let's reimport that info from the data entry google sheet
# I use the googlesheet just in case the CSV files had columns that were excluded
library(googlesheets)

gs_import <- gs_url("https://docs.google.com/spreadsheets/d/1mFaWJ752bZMYe3eBtjzbCXrrcOHYr-84_Dvf-E91lxg/pub?gid=0&single=true&output=csv")
Pru_leng <- gs_read(gs_import, ws = "Fish.LengthData")

names(Pru_leng)[5] <- "EndMonth"
names(Pru_leng)[6] <- "EndDay"
names(Pru_leng)[19] <- "CollectedLive"

Pru_leng$EndDate <- as.Date(ymd(as.Date(paste0(Pru_leng$Year,  "/", Pru_leng$EndMonth, "/", Pru_leng$EndDay )), 
                           tz = "US/Alaska"))

Pru_leng <- Pru_leng %>% mutate(Net = paste0(Net, Side)) %>%
  dplyr::select(EndDate, Net, Species, Length, Otolith, CollectedLive, Frozen) %>%
  filter(!is.na(Otolith) | !is.na(CollectedLive) | !is.na(Frozen))  

###############

permit_all <- permit_all %>% left_join(Pru_leng, by = c("EndDate" = "EndDate", "Net" = "Net", 
                                                        "Species" = "Species", "Length" = "Length"))







# Be careful if you measured other species in the future
permit_all <- permit_all %>% left_join(spplookup, by = c("Species" = "Species")) %>% 
  mutate(LengthMeth = if_else(Species == "ARCD" | Species == "SFCD", "Total", "Fork")) 


# Now finally add in new cols and drop unneeded cols
permit_all <- permit_all %>% add_column(Datum, CoordDeterm, WaterBody, 
                                        permitholder, CollectionMeth, LifeStage) %>% 
  dplyr::select(LocationID, Lat, Long, Datum, CoordDeterm, WaterBody, EndDate, 
                permitholder, CollectionMeth, commonname, LifeStage, Length, 
                LengthMeth, AddlCount1, Disp1, AddlCount2, Disp2, Otolith, CollectedLive, Frozen)


write.csv(permit_all, file = paste0("../../Permitting/ADFG Collections Report/", curr_yr, "-FRP_Reporting_ScriptOutput.csv"))

# You still need to manually adjust the last 3 columns. This is because there are too many combos 
# (a "frozen" fish might have been inadvertent mort before harvesting or not). I recommend filtering
# in excel to find all fish and pasting over to blanks, that way you avoid edge cases. 

# There's quite a bit that will still need to be done in Excel but this will get you within 10 minutes of finishing!

