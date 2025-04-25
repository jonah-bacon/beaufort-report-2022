## OVERVIEW ##
# This is the script to import the 2022 data into the all years datasets for both counts and lengths
# First, import the 2022 data that are saved as CSV files. These CSV files were created based on the 
# PrudhoeBay_DataEntry2022.xlsx file. If there are errors found in the future, fix in that 
# database, re-export the CSVs & re-run this script, then put the .RDATA file in the correct folder. 
# You will probably need to change directories and/or names


## STEP 1 - PRELIM QAQC ##
# QAQC what you can in the Google doc. This will save you time later as it is very easy to do
# and fast to view. Set a filter on different columns to see if everything is as it should be
# or if there are NAs where there shouldn't be. Run quick sums (to see if theres NAs), even 
# do some quick plotting to find outliers. Delete filters and plots before finishing. 
# Leaving in filters and plots could cause massive issues (haven't noticed any yet though).
# Make sure that headers (row 1) are the exact same as previous years. 
# Make sure that species codes are correct, e.g., "THSB" NOT "TSSB".

## STEP 2 - FILE EXPORT ##
# Once you are satisfied with the data, export the google doc as an .xlsx file
# Again, view everything to make sure that it looks good.
# From each of the tabs, save these each as a .CSV file (do not save as a Windows CSV or Mac CSV).
# Name these as "2022_FishLengths.csv", "2022_FishTotalCatch.csv".
# There is no need to import the "2022_Temp_Salinity.csv" as the YSI was broken this year (2022) 
# and so no temp/salinity data was taken.
# Since the locations don't change, you don't have to export these. 
# Create a 2022 analysis folder, preferably using a new RProject. 
# Save the CSV files to a "data" folder. 
# Copy the 2001-2022 .Rdata file into the data folder too. Make sure it's the final version!
# Copy over the environmental data, which currently exists as multiple files. 
# If I get some time one day, I'll streamline all the environ data into a single file. 

# STEP 3 - MERGE 2022 WITH 2001-2022
# Run script below. There are a few places to stop and check to make sure that everything works.
# So run it line-by-line the first few times to make sure that everything works. 
# After you confirm that everything runs fine, you can usually just run it all at once. 

## STEP 4 - I FOUND AN ERROR! ##
# If you find a small error in the data, correct it in the google doc, XLSX, and CSVs. 
# Then re-run here; things should run automatically with no problems.
# Yes it's a pain to have 3 "final" versions of everything and we should probably delete the 
# google doc and the CSVs but they are handy to have around. 
# I prefer to change the version names after correcting (so that someone doesn't mistakenly 
# use the old dataframe), but I can see the value in just using one updated file.
# If you find an error in the 2001-2016 database (it still happens!), whooo-boy. 
# First, talk to Justin. The steps are similar: Fix in the main file, run scripts. 
# There are individual import scripts for 2001-2016, 2017, and 2018.

# JTP 24/09/2019. justinpriest.ak@gmail.com
# Updated JAB 25/08/2022. jonahbacon37@gmail.com


library(dplyr)
library(here)
library(lubridate)
library(stringr)
library(readxl)
library(forcats)


Pru_leng <- read.csv(here::here("../../Data/Database/2022_FishLengths.csv"))
Pru_tot <- read.csv("../../Data/Database/2022_FishTotalCatch.csv")
Pru_tot <- Pru_tot[,1:5]
# check over the these two files. Sometimes you get 1000s of blank extra rows


names(Pru_leng)[3] <- "StartMonth"
names(Pru_leng)[4] <- "StartDay"
names(Pru_leng)[5] <- "EndMonth"
names(Pru_leng)[6] <- "EndDay"
names(Pru_leng)[7] <- "StartTime"
names(Pru_leng)[8] <- "EndTime"
Pru_leng$StartTime <- seconds_to_period(hm(Pru_leng$StartTime)) # a warning is normal here
Pru_leng$EndTime <- seconds_to_period(hm(Pru_leng$EndTime))
Pru_leng$SampleDate <- ymd(as.Date(paste0(Pru_leng$Year,  "/", Pru_leng$EndMonth, "/", Pru_leng$EndDay )), 
                           tz = "US/Alaska")

Pru_leng$StartDate <- ymd(as.Date(paste0(Pru_leng$Year,  "/", Pru_leng$StartMonth, "/", Pru_leng$StartDay )), 
                          tz = "US/Alaska")

Pru_leng$StartDateTime <- as.POSIXct(paste0(Pru_leng$StartDate, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  Pru_leng$StartTime 
Pru_leng$StartDateTime <- as.POSIXct(Pru_leng$StartDateTime)

Pru_leng$EndDateTime <- as.POSIXct(paste0(Pru_leng$SampleDate, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  Pru_leng$EndTime
Pru_leng$EndDateTime <- as.POSIXct(Pru_leng$EndDateTime)

Pru_leng$Station <- paste0(Pru_leng$Net, Pru_leng$Side)

Pru_tot$Net <- str_sub(Pru_tot$LINK, start =  10, end = 12)
Pru_tot$Side <- str_sub(Pru_tot$LINK, start =  13, end = 13)
Pru_tot$SampleDate <- ymd(as.Date(paste0(str_sub(Pru_tot$LINK, 5, 8), "/", str_sub(Pru_tot$LINK, 1, 2), 
                                         "/", str_sub(Pru_tot$LINK, 3, 4))))



#############
# load in the 2001-2021 data. MAKE SURE YOU USE FINAL VERSIONS!
load("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2021_Version10.Rdata")

all.trans <- all.len

pru_transfer <- Pru_leng %>% dplyr::select(LINK, Year, StartMonth, StartDay, EndMonth, EndDay,
                                           Net, Side, Code, Species, Length, Count, LG, StartDateTime, EndDateTime)
# above line removes StartTime and EndTime columns (unnecessary)
pru_transfer$day.of.year <- yday(pru_transfer$EndDateTime)
pru_transfer$totcount <- 1
pru_transfer$EndDate <- Pru_leng$SampleDate

names(pru_transfer)[1] <- "Line"
names(pru_transfer)[7] <- "Station"
names(pru_transfer)[13] <- "Group"


names(all.trans)
names(pru_transfer) # Review prev two lines of output to make sure they're the same!
isTRUE(names(pru_transfer)[17] == names(all.len)[17])  # stop if this is not "TRUE"


# If there's an issue, check that the structures are the same (or at least convertable)
str(all.trans)
str(pru_transfer)


pru_transfer$Side <- as.factor(pru_transfer$Side)
#all.trans$StartTime <- hm(all.trans$StartTime)
#all.trans$EndTime <- hm(all.trans$EndTime)

#check time zones
tz(all.trans$StartDateTime) #if UTC, uncomment following code:
#tz(all.trans$StartDateTime) <- "US/Alaska"
#tz(all.trans$EndDateTime) <- "US/Alaska"


bothsets_length <- rbind(all.trans, pru_transfer)

#remove the -9999 lengths which were just NAs
bothsets_length <- bothsets_length[bothsets_length$Length!=-9999,]



###########
# Now work on catch

sum(is.na(Pru_tot$LINK)) # If this is anything other than 0, you have blank lines being imported

allcatch.trans <- allcatch
pru_tot_trans <- Pru_tot

allcatch.trans$LG <- as.numeric(allcatch.trans$LG) #changed from "Group"
#allcatch.trans <- allcatch.trans %>% select(-Group)
allcatch.trans$Morts <- as.numeric(NA)



pru_tot_trans$Net <- paste0(pru_tot_trans$Net, pru_tot_trans$Side)
pru_tot_trans <- pru_tot_trans %>% dplyr::select(-Side, -LINK)
pru_tot_trans$Year <- 2022
pru_tot_trans$day.of.year <- yday(pru_tot_trans$SampleDate)


pru_tot_trans <- pru_tot_trans %>% dplyr::select(Year, EndDate=SampleDate, day.of.year, Net, 
                                                 Species, totcount = Count, LG, Morts)
names(allcatch.trans) 
names(pru_tot_trans)
bothsets_catch <- rbind(allcatch.trans, pru_tot_trans )


###########
# Before running the next few lines, inspect the code and make sure that everything is good
# NOTE!!! The years before 2017 do not have length groups added yet, so it's all NAs <2017

all.len <- bothsets_length
allcatch <- bothsets_catch

all.len$Species[all.len$Species == "NNST"] <- "NNSB"
all.len$Species[all.len$Species == "TSST"] <- "THSB"
all.len$Species[all.len$Species == "THSB "] <- "THSB"
all.len$Species[all.len$Species == "SUCK"] <- "LNSK"
all.len$Species[all.len$Species == "WSGR"] <- "WSGL"
allcatch$Species[allcatch$Species == "NNST"] <- "NNSB"
allcatch$Species[allcatch$Species == "TSST"] <- "THSB"
allcatch$Species[allcatch$Species == "THSB "] <- "THSB"
allcatch$Species[allcatch$Species == "SUCK"] <- "LNSK"
allcatch$Species[allcatch$Species == "WSGR"] <- "WSGL"

#Update Oct 2018: Lubridate "periods" are discontinued so I removed these columns
#all.len <- all.len %>% dplyr::select(-EndTime, -StartTime)


all.len <- all.len %>% mutate(Group = replace(Group, Group==1 & Species =="HBWF", NA), 
                              Group = replace(Group, Group==2 & Species =="HBWF", NA),
                              Group = replace(Group, Group==3 & Species =="HBWF", NA))


##############################
# ENV DATA
##############################

salinity <- read_excel("../../Data/Database/PB_Salinity_2001-2021_onesheet.xlsx", col_names = TRUE)
salinity$Date <- as_date(salinity$Date)
salinity$Station <- as.character(salinity$Station)
#note: I manually changed 7/7/2015 Station 218, from 47.0 to 4.7 ppt

temperature <- read_excel("../../Data/Database/PB_Temperature_2001-2021_onesheet.xlsx", col_names = TRUE)
temperature$Date <- as_date(temperature$Date)
temperature$Station <- as.character(temperature$Station)


# NOTE: -------------------------------------------------------------------
# In 2022, we did not collect environmental data because of a broken YSI.
# If copying this script in 2023, you will need to uncomment (CTRL+Shift+C) lines 213-226 to
# process environmental data that you collect

# # Dates will probably be the biggest problem right here!
# env2022 <- read.csv("../../Data/Database/2022_Temp_Salinity.csv")
# env2022$Date <- ymd(as_date(as.POSIXct(env2022$Date, format = "%m/%d/%Y")))
# env2022$Site <- as.character(env2022$Site)
# names(env2022) <- c("Station", "Date", "Salin_Top", "Salin_Mid", "Salin_Bot", "Temp_Top", "Temp_Mid", "Temp_Bot")
# 
# 
# tempdf <- left_join(salinity, temperature %>% dplyr::select(-Year), by = c("Date", "Station")) 
# 
# env_allyears <- bind_rows(tempdf, env2022)
# env_allyears$Year <- year(env_allyears$Date)
# 
# # replace the value on 8/9/2011 Stn 218. It was entered as 23 C, double the usual values
# env_allyears <- env_allyears %>% mutate(Temp_Mid=replace(Temp_Mid, Temp_Mid==23 & Year == 2011, NA)) 


# Finally, check if there are new species that you caught! 
data.frame(Species = unique(allcatch$Species)) %>% filter(!Species %in% unique(spplookup$Species))
# If the above line returns anything, add the new species to spplookup

#NOTE! Comment out this next part in 2021! This is only relevant to 2019 when STFL was new
# do not run this line twice 
#spplookup <- spplookup %>% 
  #add_row(Species = "STFL", commonname = "Starry Flounder", familygroup = "Pleuronectidae") %>% 
  #mutate(Species = forcats::fct_reorder(as.character(Species), as.character(Species))) %>%
  #arrange(Species)
# This adds a new species to spplookup, reorganizes the factor levels, then sorts by Species



########
# Check that it's all good and save it


save(all.len, allcatch, spplookup, env_allyears, file = 
       "../../Data/Database/PrudhoeCatch&LengthDataset_2001-2022_Version5.Rdata")
write.csv(allcatch, file = "allcatch2001-2022.csv")


