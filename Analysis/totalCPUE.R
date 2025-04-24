##Effort and CPUE with total CPUE and SD

##########################################
# CPUE and Effort for 2019 Annual Report 
# Created by Justin Priest, Oct 2018

# Last edits: Oct 8, 2018

# note that the error bars are large 
# with the way I've done it. I might 
# come back and look at alt methods. 
##########################################

#For CPUE data there are many ways to define what "effort" is. 
# We chose to measure it in a combined manner: first we analyzed 
# on per hour basis, scaled to per day. Then both sides of the net
# were added together to get the CPUE for that station. 

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)

curr_yr <- 2022
load(here::here("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2022_Version5.Rdata"))



all.len <- all.len %>% mutate(Net = paste0(Station, Side))

effort <- full_join(all.len %>%
                      distinct(EndDate, Net), 
                    allcatch %>% distinct(EndDate, Net), 
                    by = c("EndDate", "Net")) %>% 
  left_join(all.len %>%
              dplyr::select(EndDate, Net, StartDateTime, EndDateTime), 
            by = c("EndDate", "Net")) %>% 
  distinct(EndDate, Net, StartDateTime, EndDateTime) %>%
  #this first mutate MANUALLY adds in skipped dates
  mutate(
    # Add start/end date/time for fishing when we caught fish but didn't have any lengths:
    StartDateTime=replace(StartDateTime, EndDate=="2022-07-16" & Net == "220E", 
                          as.POSIXct("2022-07-15 07:20")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-07-16" & Net == "220E", 
                        as.POSIXct("2022-07-16 6:57")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-07-31" & Net == "220E", 
                          as.POSIXct("2022-07-30 07:22")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-07-31" & Net == "220E", 
                        as.POSIXct("2022-07-31 7:30")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-08-02" & Net == "220E", 
                          as.POSIXct("2022-08-01 06:36")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-08-02" & Net == "220E", 
                        as.POSIXct("2022-08-02 06:48")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-08-05" & Net == "220W", 
                          as.POSIXct("2022-08-04 6:20")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-08-05" & Net == "220W", 
                        as.POSIXct("2022-08-05 6:37")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-08-10" & Net == "220W", 
                          as.POSIXct("2022-08-09 06:54")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-08-10" & Net == "220W", 
                        as.POSIXct("2022-08-10 06:17")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-08-18" & Net == "220W", 
                          as.POSIXct("2022-08-17 07:54")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-08-18" & Net == "220W", 
                        as.POSIXct("2022-08-18 06:50")),
    
    StartDateTime=replace(StartDateTime, EndDate=="2022-08-20" & Net == "220E", 
                          as.POSIXct("2022-08-19 06:10")), 
    EndDateTime=replace(EndDateTime, EndDate=="2022-08-20" & Net == "220E", 
                        as.POSIXct("2022-08-20 06:57"))) %>%
  
  # Add start date/times for fishing when we caught 0 fish:
  add_row(
    EndDate = as.Date("2022-07-30"),
    Net = "220E",
    StartDateTime=as.POSIXct("2022-07-29 7:32"),
    EndDateTime = as.POSIXct("2022-07-30 07:22")) %>%
  
  add_row(
    EndDate = as.Date("2022-08-06"),
    Net = "220E",
    StartDateTime = as.POSIXct("2022-08-05 06:44"),
    EndDateTime = as.POSIXct("2022-08-06 07:41")) %>%
  
  add_row(
    EndDate = as.Date("2022-08-07"),
    Net = "220E",
    StartDateTime = as.POSIXct("2022-08-06 07:41"),
    EndDateTime = as.POSIXct("2022-08-07 06:23")) %>%
  
  add_row(
    EndDate = as.Date("2022-08-15"),
    Net = "220W",
    StartDateTime = as.POSIXct("2022-08-14 06:37"),
    EndDateTime = as.POSIXct("2022-08-15 06:42")) %>%
  
  add_row(
    EndDate = as.Date("2022-08-15"),
    Net = "220E",
    StartDateTime = as.POSIXct("2022-08-14 06:46"),
    EndDateTime = as.POSIXct("2022-08-15 06:52")) %>%
  
  add_row(
    EndDate = as.Date("2022-08-18"),
    Net = "220E",
    StartDateTime = as.POSIXct("2022-08-17 08:07"),
    EndDateTime = as.POSIXct("2022-08-18 06:55")) %>%
  
  mutate(Year = year(EndDate),
         Effort = as.numeric(EndDateTime - StartDateTime)) %>%
  arrange(EndDate, Net)
# A few NAs occur when we have catch but no lengths for that day 
# (because the times are recorded in the length dataframe!)
# So run this and manually check for any NAs in the current year



CPUEallyears <- allcatch %>% left_join(effort %>% dplyr::select(-Year), by = c("EndDate", "Net")) %>%
  mutate(CPUE_day = 24 * (totcount / as.numeric(Effort)), 
         Station = substr(Net, 1, 3)) #Catch per day

allcatch2022 <- subset(allcatch, Year == 2022, select=c(Year:Morts))
effort2022 <- subset(effort, Year == 2022, select=c(EndDate:Effort))
CPUE_2022 <- allcatch2022 %>% left_join(effort2022 %>% dplyr::select(-Year), by = c("EndDate", "Net")) %>%
  mutate(CPUE_day = 24 * (totcount / as.numeric(Effort)), 
         Station = substr(Net, 1, 3)) #Catch per day
CPUE_by_station <- CPUE_2022 %>% group_by(Year, EndDate, Net, Species) %>% 
  summarise(CPUE_stn = sum(CPUE_day, na.rm = TRUE)) %>%  
  group_by(Net, Species) %>% summarise(CPUE_stat = mean(CPUE_stn, na.rm=T), CPUE_stat_sd = sd(CPUE_stn, na.rm=T)) %>% replace(., is.na(.), 0)
CPUE_by_LG <- CPUE_2022 %>% group_by(Year, EndDate, Net, Species, LG) %>% 
  summarise(CPUE_stn = sum(CPUE_day, na.rm = TRUE), CPUE_SD = sd(CPUE_day, na.rm=T)) %>% group_by(Net, Species, LG) %>% summarise(CPUE_stat = mean(CPUE_stn, na.rm=T),
                                                                       CPUE_stat_sd = sd(CPUE_stn, na.rm=T)) %>%  
  replace(., is.na(.), 0)

#visualize CPUE distribution
ggplot(CPUEallyears, aes(x=CPUEallyears$Effort))+
  geom_histogram(bins = 50) + scale_x_continuous(limits = c(10,60))


annualCPUE <- CPUEallyears %>% group_by(Year, EndDate, Station, Species) %>% 
  summarise(CPUE_stn = sum(CPUE_day, na.rm = TRUE)) %>%
  #run above part alone to see daily breakdown by station
  spread(Species, value = CPUE_stn) %>%
  replace(., is.na(.), 0) %>% 
  gather(Species, CPUE_stn, -Year, -EndDate, -Station) %>%
  # the above part is very important! CPUE calcs in 2017 were taking
  # the mean of all days where catch was present. Now this is set up
  # to create a list of ALL sample days/station & species, then to
  # replace all of the NA values with zeros. 
  group_by(Year, Species) %>%
  summarise(CPUE_ann = mean(CPUE_stn, na.rm = TRUE),
            CPUE_ann_sd = sd(CPUE_stn, na.rm = TRUE))
#note that this is combined nets (per station)
#left_join(effort %>% group_by(Year) %>% 
#summarise(anneffort = sum(Effort, na.rm = TRUE)/24), by = "Year") %>%
#group_by(Year, Species) %>%
#mutate(annCPUE_stnnights = 2*(anncatch / as.numeric(anneffort)))



effort %>% group_by(Year, Net) %>% 
  summarise(anneffort = sum(Effort, na.rm = TRUE)/24) %>%
  group_by(Year) %>% summarise(anneffort_nethrs = sum(anneffort, na.rm = TRUE)) %>% View(.) 
# overall in 2022 we had 353 net nights among 8 codends
# easier to use "station nights" (aka "net nights") and combine codends (divide by 2)







