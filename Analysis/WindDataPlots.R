############################################################
# WIND PLOTS SCRIPT
# Last Updated: 2020/09/28

# This is one of the script files for the 2020 Annual Report
# Created by Justin Priest, October 2018; updated JTP Sept 2020


# Optional: restrict wind dates to only dates we sampled


# This script creates the figures for the 2020 Prudhoe Bay report.
# In the 2017 report these were figures 3 & 4
# To adapt this for future years, add the most recent data to the csv 
# then update the most recent year
# Everything should run automatically except the final figure.
# For this, it is easiest to touch it up manually.
# If possible, (use library(export) to export and move labels for readability)
############################################################


library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)
library(ggrepel)
library(CircStats)
library(extrafont)
library(CircStats)
#font_import()  #this only needs to be run once
loadfonts(device="win") #change "win" to "mac" for mac users

load(here::here("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2022_Version5.Rdata"))
curr_yr <- 2022

startsampling <- all.len %>% filter(Year == curr_yr) %>% summarise(date(min(StartDateTime)))
endsampling <- all.len %>% filter(Year == curr_yr) %>% summarise(date(max(EndDateTime)))

######################################
########## WIND DATA IMPORT ##########
######################################


# JTP Sept 2020: Wind and air temp data from NOAA NCEI
# https://www.ncdc.noaa.gov/cdo-web/datasets/LCD/stations/WBAN:27406/detail
# Documented here: https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# Station WBAN:27406
# These data were downloaded in 10-year increments (LCD CSV). 
# Once you have selected dataset on the NOAA website, they'll email you a link to download
# First, you must make sure that ALL columns are the same between files before adding. 
# This is best done in Excel, possibly with =IF(A1=A2, 1, 0) and comparing sum to total cells
# Once you're sure the cells will line up, paste into the appropriate document.
# In 2020, instead of adding a new file with just 2020 wind data, I made 2010-2020 one file. 
# For consistency, in future, you might want to revert to 2010-2019 & 2020-2022. 



# Note that the 2019 code was different than the 2018 code because of slight NOAA format changes. 
#If for whatever you find your data is in a different format, check the 2018 code and maybe that'll work.
wind_allyears <- read.csv(here::here("../../Data/Database/NOAA_wind_data_1980-2022.csv"), stringsAsFactors=FALSE)
windhourly <- wind_allyears %>% dplyr::select(DATE, REPORT_TYPE, HourlyWindSpeed, 
                                              HourlyWindDirection, HourlyWindGustSpeed) %>%
  mutate(DATE = ymd_hms(str_replace(DATE, pattern = "T", " "), tz = "US/Alaska"),
  #mutate(DATE = parse_date_time(DATE, c("%m/%d/%Y %H:%M"), exact = TRUE, tz = "US/Alaska"),
         HourlyWindSpeed = as.numeric(HourlyWindSpeed),
         HourlyWindDirection = as.numeric(HourlyWindDirection)) %>%
  rename(EndDate = DATE)


# JTP 2020: I apologize for not annotating earlier code. FM-15 & SAO mean the hourly observations 
# First, filter to remove daily summaries and other report types (only use hrly)
# Then calc the daily mean speed and direction based on circular means. 
# This took me a solid week to figure out at first and I think that it's correct but who knows! 
# The circular mean has to be taken because if you average 350 and 10 degree winds, it should be 0, not 180. 
windsummary <- windhourly %>% filter(REPORT_TYPE == "FM-15" | REPORT_TYPE == "SAO  ") %>% # Note the spaces after SAO
  mutate(EndDate = as.Date(EndDate,  tz = "US/Alaska"), Year = year(EndDate), Month = month(EndDate)) %>%
  group_by(EndDate, Year, Month) %>% 
  summarise(Dailymeanspeed = mean(HourlyWindSpeed, na.rm = TRUE), 
            Dailymeandir = ((circ.mean(2*pi*na.omit(HourlyWindDirection)/360))*(360 / (2*pi))) %%360 )
# NOTE: You have to use circular means and not just a mean

# Create categorical assignment of dominant wind directions
windsummary$eastwest <- ifelse(windsummary$Dailymeandir >= 45 & windsummary$Dailymeandir <135, "East", 
                            ifelse(windsummary$Dailymeandir >= 225 & windsummary$Dailymeandir <315, "West", 
                                   ifelse(windsummary$Dailymeandir >= 315 | windsummary$Dailymeandir <45, "North","South")))


# Below code multiplies out the east/west wind components by speed, then converts it to km/hr
windsummary$dirspeed_eastwest_kmh <- sin(windsummary$Dailymeandir*pi/180) * windsummary$Dailymeanspeed * 1.609344
windsummary$dirspeed_northsouth_kmh <- cos(windsummary$Dailymeandir*pi/180) * windsummary$Dailymeanspeed * 1.609344
# In the above code, positive numbers are east or north, negative are west or south


#seasonal summary of years
windsummary %>% filter(Month == 7 | Month ==8) %>% group_by(Year, eastwest) %>%
  tally() %>% spread(eastwest, value = n) %>% View("summary")

windsummary %>% filter(Month == 7 | Month ==8) %>% group_by(Year, eastwest) %>%
  tally() %>% group_by(eastwest) %>% summarise(meann = mean(n), mediann = median(n)) 

windsummary %>% filter( Month == 7 | Month ==8) %>% group_by(Year) %>%
  summarise(meanspeed_kph = mean(Dailymeanspeed)* 1.609344) %>% tail(n=20)

# what is the previous average
windsummary %>% filter(Month == 7 | Month ==8, Year >= 2001 & Year < curr_yr) %>% group_by(Year, eastwest) %>%
  tally() %>% group_by(eastwest) %>%
  summarise(mean(n, na.rm = TRUE), median(n, na.rm = TRUE))


#now just look at current year results, restricted to only sampling days
windsummary %>% filter(Year == curr_yr, EndDate >= startsampling, EndDate <= endsampling) %>% group_by(Year, eastwest) %>%
       tally() %>% spread(eastwest, value = n)

#Seasonal average
windsummary %>% filter(EndDate >= startsampling & EndDate <= endsampling) %>% 
  group_by(Year) %>% #run this to get the annual average
  summarise(meanspeed_kph = mean(Dailymeanspeed, na.rm = TRUE) * 1.609344,
            meandir = ((circ.mean(2*pi*na.omit(Dailymeandir)/360))*(360 / (2*pi))) %% 360 )



#############################
####### WIND FIGURES ########
#############################


##################################
#### 2018 E-W & S-N Area Plot ####

# This sets the breaks for the plot below at 7/1, 7/15, 8/1, etc
break.vec <- ymd(paste0(curr_yr,"-06-26"))
break.vec <- c(break.vec, break.vec +days(7), break.vec + days(14), 
               break.vec + days(21), break.vec + days(28), break.vec + days(35), break.vec + days(42), 
               break.vec + days(49), break.vec + days(56), break.vec + days(63))

ggplot(windsummary %>% filter(EndDate >= paste0(curr_yr,"-06-26") & EndDate < paste0(curr_yr,"-08-28")), 
       aes(x=as.Date(EndDate), y= dirspeed_eastwest_kmh)) +
  geom_area(fill="grey70", color = "black", position = "identity") +
  #theme(text=element_text(family="Times New Roman", size=16)) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(angle = 35, hjust = 1, size = 12), axis.text.y = element_text(size=12)) + 
  scale_y_continuous(breaks = seq(from = -30, to=30, by=10)) +
  scale_x_date(labels = date_format("%m/%d/%Y"), 
               limits = c(as.Date(paste0(curr_yr, "-06-26")), as.Date(paste0(curr_yr, "-08-28"))),
               breaks= break.vec) +
  labs(y=paste0(sprintf('\u2190'), "West          Wind velocity (km/h)          East", sprintf('\u2192')), x="") 
ggsave(file="../Figures/windsummary_EW.png", width = 6.5, height = 4, units = "in", dpi = 1000)


ggplot(windsummary %>% filter(EndDate >= paste0(curr_yr,"-06-26") & EndDate < paste0(curr_yr,"-08-28")), 
       aes(x=as.Date(EndDate), y= dirspeed_northsouth_kmh)) +
  geom_area(fill="grey70", color = "black", position = "identity") +
  #theme(text=element_text(family="Times New Roman", size=16)) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(angle = 35, hjust = 1, size = 12), axis.text.y = element_text(size=12)) + 
  scale_y_continuous(limits = c(-30,30), breaks = seq(from = -30, to=30, by=10)) +
  scale_x_date(labels = date_format("%m/%d/%Y"), 
               limits = c(as.Date(paste0(curr_yr, "-06-26")), as.Date(paste0(curr_yr, "-08-28"))),
               breaks= break.vec) +
  labs(y=paste0(sprintf('\u2190'), "South          Wind velocity (km/h)      North", sprintf('\u2192')), x="") 
ggsave(file="../Figures/windsummary_NS.png", width = 6.5, height = 4, units = "in", dpi = 1000)



##############################
#### Year Comparison Plot ####
summersummary <- windsummary %>% filter(Month == 7 | Month == 8) %>% group_by(Year) %>% 
  summarise(dirspeed_eastwest_kmh = mean(dirspeed_eastwest_kmh, na.rm = TRUE),
            dirspeed_northsouth_kmh = mean(dirspeed_northsouth_kmh, na.rm = TRUE))


# geom_text_repel almost gets it right, but still needs manual adjustment
x <- ggplot(summersummary %>% filter(Year > 1984), aes(x = dirspeed_eastwest_kmh, y = dirspeed_northsouth_kmh)) +
  geom_vline(xintercept = 0, col="gray") + geom_hline(yintercept= 0, col="gray") + 
  geom_point(data = summersummary %>% filter(Year < curr_yr), aes(x = dirspeed_eastwest_kmh, y = dirspeed_northsouth_kmh), col="gray") + 
  geom_text_repel(data = summersummary %>% filter(Year < curr_yr), aes(label=Year),family="Times New Roman", size=3) + 
  scale_y_continuous(limits = c(0, 9), breaks = c(0,2,4,6,8)) + scale_x_continuous(limits = c(-5, 16)) + 
  geom_point(data = summersummary %>% filter(Year == curr_yr), 
             aes(x = dirspeed_eastwest_kmh, y = dirspeed_northsouth_kmh), col="red") +
  geom_text_repel(data = summersummary %>% filter(Year == curr_yr), 
            aes(label=Year), family="Times New Roman", size=3, hjust=2, col="red") + 
  labs(y=paste0(sprintf('\u2190'), "South                Wind velocity (km/h)                North", sprintf('\u2192')), 
       x=paste0(sprintf('\u2190'), "West                Wind velocity (km/h)                East", sprintf('\u2192'))) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(text=element_text(family="Times New Roman", size=12))
print(x)
ggsave(file="../Figures/windplot_export.png", width=7, height=5)
# This is the main plot. But note that the point labels overlap. To fix this, run the code below


# This will export the plot as a .pptx file and you can manually adjust the labels to not overlap
# If you don't have package export, email Justin to get it! (Cannot load from CRAN)
library(datasets)
graph2ppt(x=x, "../Figures/windplot_export.pptx", width=7, height=5)


