library(shiny)
library(waiter)
library(ggplot2)
#library(rvest)
library(dplyr)
library(googlesheets)
library(readxl)
library(lubridate)
library(stringr)
library(tidyr)
#library(corrgram)
library(corrplot)
library(shinythemes)
library(rgdal)
library(leaflet)
#library(leaflet.extras)
library(gstat)
library(raster)
library(leaflet.minicharts)


load("../Data/Database/prudhoecatch&lengthdataset_2001-2021_version10.Rdata") #must be lower case for ShinyApps
currentyear <- 2021 #Update in future versions

###First Pull in the data and clean it up###

#Set Google sheet as Public AND publish it to the web! The first time run the following:
#gs_auth(new_user = T)
#install.packages("openssl")
#gs_ls() to see all of the sheets and get the true key.
#gs_ls()[[6]] to get the specific key, then paste into the url below

#For future years, to get the url below, find the spreadsheet url, e.g.,:
#https://docs.google.com/spreadsheets/d/1BhZPaxobt55MY2Sa66FqdJKFo9LVMSBAJN-3lXq9K30/edit
# then paste the part between the d/ and /edit. (e.g., the part that starts with "1BhZ"). 
# This is the "key". Replace the key in following url

#gs_import <- gs_url("https://docs.google.com/spreadsheets/d/1BhZPaxobt55MY2Sa66FqdJKFo9LVMSBAJN-3lXq9K30/pub?gid=0&single=true&output=csv")
# 
# 
# Pru_leng <- gs_read(gs_import, ws = "Fish.LengthData")
# Pru_tot <- gs_read(gs_import, ws = "Fish.TotalCount", range = cell_cols(1:5))
# Env_tempsalin <- gs_read(gs_import, ws = "Env.TempSalin")
# Pru_locations <- gs_read(gs_import, ws = "Locations", range = cell_cols(1:4))


Pru_tot <- read_excel("../Data/Database/PrudhoeBay_DataEntry2021.xlsx", sheet = "2021_FishTotalCatch") %>%
  dplyr::select(LINK:`Otolith Total`) %>% 
  mutate(Station = str_sub(LINK, start =  10, end = 12),
                   Side = str_sub(LINK, start =  13, end = 13),
                   SampleDate = ymd(as.Date(paste0(str_sub(LINK, 5, 8), "/", str_sub(LINK, 1, 2), 
                                         "/", str_sub(LINK, 3, 4)))),
                   doy = yday(SampleDate))


Pru_leng <- read_excel("../Data/Database/PrudhoeBay_DataEntry2021.xlsx", 
                       sheet = "2021_FishLengths", guess_max = 50000) %>%
  dplyr::select(LINK:Frozen)%>%
  rename("StartMonth" = `S Mon`,
         "StartDay" = `S Day`,
         "EndMonth" = `E Mon`,
         "EndDay" = `E Day`,
         "StartTime" = `S Time`,
         "EndTime" = `E Time`) %>%
  mutate(StartTime = seconds_to_period(StartTime),
         EndTime = seconds_to_period(EndTime),
         SampleDate = ymd(as.Date(paste0(Year,  "/", EndMonth, "/", EndDay )), 
                          tz = "America/Anchorage"),
         StartDate = ymd(as.Date(paste0(Year,  "/", StartMonth, "/", StartDay )), 
                         tz = "America/Anchorage"), 
         StartDateTime = as.POSIXct(paste0(StartDate, " " ,'00:00:00'), format = "%Y-%m-%d") + StartTime,
         StartDateTime = as.POSIXct(StartDateTime),
         EndDateTime = as.POSIXct(paste0(SampleDate, " " ,'00:00:00'), format = "%Y-%m-%d") + EndTime,
         EndDateTime = as.POSIXct(EndDateTime),
         LG = as.numeric(LG),
         Net = str_sub(LINK, start =  10, end = 12),
         Side = str_sub(LINK, start =  13, end = 13),
         #didn't need to fix SampleDate? if issues, look here first
         doy = yday(SampleDate),
         Station = paste0(Net, Side))

  
Env_tempsalin <- read_excel("../Data/Database/PrudhoeBay_DataEntry2021.xlsx", sheet = "2021_Temp_Salinity") %>%
  dplyr::select(Site:TempBot)
Pru_locations <- read_excel("../Data/Database/PrudhoeBay_DataEntry2021.xlsx", sheet = "Locations")









Env_tempsalin   <- Env_tempsalin %>%
  mutate(Date = ymd(as.Date(Date, format = "%m/%d/%Y"), tz = "America/Anchorage"),
         Site = as.factor(Site)) %>%
  rename("SampleDate" = "Date")

Pru_locations <- Pru_locations %>%
  mutate(Site = as.factor(Site))













# 
# Pru_leng$StartTime <- seconds_to_period(Pru_leng$StartTime)
# Pru_leng$EndTime <- seconds_to_period(Pru_leng$EndTime)
# Pru_leng$SampleDate <- ymd(as.Date(paste0(Pru_leng$Year,  "/", Pru_leng$EndMonth, "/", Pru_leng$EndDay )), 
#                            tz = "US/Alaska")
# 
# Pru_leng$StartDate <- ymd(as.Date(paste0(Pru_leng$Year,  "/", Pru_leng$StartMonth, "/", Pru_leng$StartDay )), 
#                           tz = "US/Alaska")
# 
# Pru_leng$StartDateTime <- as.POSIXct(paste0(Pru_leng$StartDate, " " ,'00:00:00'), format = "%Y-%m-%d") + 
#   Pru_leng$StartTime
# Pru_leng$StartDateTime <- as.POSIXct(Pru_leng$StartDateTime)
# 
# Pru_leng$EndDateTime <- as.POSIXct(paste0(Pru_leng$SampleDate, " " ,'00:00:00'), format = "%Y-%m-%d") + 
#   Pru_leng$EndTime
# Pru_leng$EndDateTime <- as.POSIXct(Pru_leng$EndDateTime)
# 
# #format(as.POSIXct(paste0(Pru_leng$SampleDate, '00:00:00')) + Pru_leng$StartTime, "%I:%M:%S %p") 
# 
# Pru_leng$Station <- paste0(Pru_leng$Net, Pru_leng$Side)
# Pru_leng$doy <- yday(Pru_leng$EndDateTime)
# 

# Pru_tot$Net <- str_sub(Pru_tot$LINK, start =  10, end = 12)
# Pru_tot$Side <- str_sub(Pru_tot$LINK, start =  13, end = 13)
# Pru_tot$SampleDate <- ymd(as.Date(paste0(str_sub(Pru_tot$LINK, 5, 8), "/", str_sub(Pru_tot$LINK, 1, 2), 
#                                          "/", str_sub(Pru_tot$LINK, 3, 4))))
# Pru_tot$doy <- yday(Pru_tot$SampleDate)

# Env_tempsalin$Date <- ymd(as.Date(Env_tempsalin$Date, format = "%m/%d/%Y"), tz = "US/Alaska")
# names(Env_tempsalin)[2] <- "SampleDate"
# Env_tempsalin$Site <- as.factor(Env_tempsalin$Site)
# 
# Pru_locations$Site <- as.factor(Pru_locations$Site)
# 

catchsummary <- Pru_tot %>% group_by(Species) %>% summarise(Total = sum(Count)) %>%
  mutate(Total = as.integer(Total)) %>%
  arrange(desc(Total))

lengthsummary <- Pru_leng %>% group_by(Species) %>% summarise(Total = length(LINK))
lengthsummary <- arrange(lengthsummary, desc(Total))


##### Add in common names, etc
spplookup <- tibble(Species = unique(Pru_tot$Species), CommName ="")
spplookup$CommName[spplookup$Species == "ARCD"] <- "Arctic Cod"
spplookup$CommName[spplookup$Species == "BDWF"] <- "Broad Whitefish"
spplookup$CommName[spplookup$Species == "ARCS"] <- "Arctic Cisco"
spplookup$CommName[spplookup$Species == "LSCS"] <- "Least Cisco"
spplookup$CommName[spplookup$Species == "DLVN"] <- "Dolly Varden"
spplookup$CommName[spplookup$Species == "SFCD"] <- "Saffron Cod"
spplookup$CommName[spplookup$Species == "ARFL"] <- "Arctic flounder"
spplookup$CommName[spplookup$Species == "HBWF"] <- "Humpback Whitefish"
spplookup$CommName[spplookup$Species == "FHSC"] <- "Fourhorn Sculpin"
spplookup$CommName[spplookup$Species == "RBSM"] <- "Rainbow Smelt"
spplookup$CommName[spplookup$Species == "RDWF"] <- "Round Whitefish"
spplookup$CommName[spplookup$Species == "NNSB"] <- "Ninespine Stickleback"
spplookup$CommName[spplookup$Species == "GRAY"] <- "Arctic Grayling"
spplookup$CommName[spplookup$Species == "HYCS"] <- "Hybrid Cisco"
spplookup$CommName[spplookup$Species == "CHUM"] <- "Chum Salmon"
spplookup$CommName[spplookup$Species == "BRBT"] <- "Burbot"
spplookup$CommName[spplookup$Species == "PCHR"] <- "Pacific Herring"
# Could have passed all of the previous as a list but since this is a dynamic list, the order might change in season
# as we catch new species

spplookup$Family <- "Other Family"
spplookup$Family[spplookup$Species %in% c("ARCS", "BDWF", "LSCS", "HBWF", "RDWF", "HYCS")] <- "Whitefish"
spplookup$Family[spplookup$Species %in% c("ARCD", "SFCD")] <- "Cods"
spplookup$Family[spplookup$Species %in% c("FHSC", "SHSC")] <- "Sculpin"
spplookup$Family[spplookup$Species %in% c("RBSM")] <- "Smelt"
spplookup$Family[spplookup$Species %in% c("ARFL")] <- "Flatfish"

#spplookup$Species %in% top_n(catchsummary, 7, Total)$Species
spplookup$topspp <- ifelse(spplookup$Species %in% top_n(catchsummary, 7, Total)$Species, 
                           spplookup$CommName, "Other_spp")


#temp <- left_join(spplookup, top_n(catchsummary, 7, Total), by="Species")


Pru_tot <- left_join(Pru_tot, spplookup, by="Species") %>%
  mutate(Net = paste0(Station, Side))


############
tempcorr <- spread((Pru_tot %>% group_by(SampleDate, Species, Net) %>% summarise(Tot=sum(Count))), Species, Tot)
tempcorr[is.na(tempcorr)] <- 0
tempcorr1 <- tempcorr[,-c(1:2)]
tempcorr <- cbind.data.frame(tempcorr %>% dplyr::select(SampleDate, Net), 
                             tempcorr %>% ungroup %>% dplyr::select(-SampleDate, -Net) %>% 
                               .[,colSums(tempcorr1[1:length(tempcorr1)])>100] )
rm(tempcorr1)
# Previous lines take data and turn it into 'wide' Species catch by date/site matrix, filtered to only species
# with over 100 fish caught


#####
catch2017_2016 <- Pru_tot %>% group_by(SampleDate, Species) %>% summarize(totcount = sum(Count))
catch2017_2016$Year <- currentyear
#catch2017_2016$day.of.year <- yday(catch2017_2016$SampleDate)

#common_cols <- intersect(colnames(catch2017_2016), colnames(temp3))
#catch2017_2016 <- rbind.data.frame(subset(catch2017_2016, select = common_cols), subset(temp3, select = common_cols))

#####



Pru_lengtemp <- Pru_leng
Pru_lengtemp$num <- 1
length2001_end <- all.len

for(j in c("SFCD", "ARCD")){
  for(i in 2001:currentyear){
    length2001_end[nrow(length2001_end)+1,] <- NA #Add a blank line to the end, put NAs in it
    length2001_end[nrow(length2001_end),c(1,2,5,6,12,18)] <- c("dummy_data", i, 7, 2, j, 
                                                               paste0(as.Date(paste0("7/2/", i), format="%m/%d/%Y")))
    # These for loops are because SFCD/ARCD weren't measured before 2017, so we add dummy data for these spp
  }
}
length2001_end$EndDate <- as.Date(length2001_end$EndDate)
length2001_end$num <- 1
length2001_end <- length2001_end %>% 
  mutate(std_date = as.Date(paste0(currentyear, "-", month(EndDate), "-", 
                                   day(EndDate))))

#write.csv(allcatch, file="allcatch.csv")

