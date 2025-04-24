## QA/QC ##

# This doesn't check everything for you. The worst kind of mistakes are those where a 
# datasheet might have been forgotten to be entered. Check for those first! 

# For any errors that occur below, compare the Excel/Google Doc database against the datasheet


library(tidyr)
library(dplyr)


curr_yr <- 2022
load(here::here("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2022_Version5.Rdata"))


# First check for any duplicated days. This works by testing for if you have 
# doubles of Species/LGs at that day/net
allcatch %>% filter(Year == curr_yr) %>% group_by(EndDate, Net, Species, LG) %>% filter(n()>1) %>% View()

# And the same for environmental data
env_allyears %>% filter(Year == curr_yr) %>% group_by(Date, Station) %>% filter(n()>1) %>% View()


#####
all.len <- all.len %>% mutate(Net = paste0(Station, Side))

length_tots <- all.len %>% filter(Year == curr_yr) %>% 
  group_by(EndDate, Net, Species, Group) %>%
  summarise(totals = sum(totcount)) %>%
  rename(LG = Group)

length_tots %>% group_by(Net) %>% summarise(sum(totals)) #Check that there aren't any extra sites

# this checks for when we have more lengths than fish caught
qaqc <- allcatch %>% 
  filter(Year == curr_yr) %>% dplyr::select(-Year, -day.of.year, -Morts) %>% 
  left_join(length_tots, by = 
            c("EndDate" = "EndDate", "Net" = "Net", 
              "Species" = "Species", "LG" = "LG")) %>%
  rename(catchtotals = totcount, lengthtotals = totals) %>% 
  replace_na(list(catchtotals = 0, lengthtotals = 0)) %>%
  mutate(diff = catchtotals - lengthtotals)

View(qaqc %>% filter(diff < 0))

sum(qaqc$catchtotals)


# Another way of looking at it
all.len %>%
  mutate(Net = paste0(Station, Side)) %>%
  filter(Year == curr_yr) %>% group_by(Year, EndDate, Species, Net, Group) %>% 
  summarise(lengthcount=sum(totcount, na.rm = TRUE)) %>% 
  left_join(allcatch %>% filter(Year == curr_yr) %>% group_by(Year, EndDate, Net, LG) %>% 
              summarise(totcount=sum(totcount, na.rm = TRUE)), 
            by = c("Year" = "Year", "EndDate" = "EndDate", "Net" = "Net", "Group" = "LG")) %>%
  filter(is.na(totcount))


# This will show all LGs that were not entered for the species it should have been
allcatch %>% filter(Year == curr_yr, Species == "ARCS" | Species == "BDWF"| Species == "DLVN" |Species == "LSCS", is.na(LG))



##########################
##### MIN/MAX LENGTH #####
##########################
head(all.len %>% filter(Year == curr_yr) %>% arrange(-Length), n = 20) #max length
head(all.len %>% filter(Year == curr_yr) %>% arrange(Length), n = 20)  #min length
# manually check (cross ref against paper datasheets) any lengths that are suspicious!            



