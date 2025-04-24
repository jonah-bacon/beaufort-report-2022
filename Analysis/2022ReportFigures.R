############################################################
# This is the script file for 2019 Annual Report Figures
# Created by Justin Priest, Oct 2018. Modified Sept 2019

# Most figures created here; two other scripts deal with 
# specific analyses: WindDataPlots.R and Effort&CPUE.R

# There is also 2019Tables.Rmd which re-creates the catch
# by station & diversity summary table. 
############################################################

library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(extrafont)
# font_import()  #this only needs to be run once
# loadfonts() #change for you mac lovers
library(scales)


# The .RData file was created by importing the main Excel doc
# from LGL ("PB_DataImport2001-2016.R"), then running  
# "2017 import.R", "2018 import.R", and "2019_import.R". This should 
# produce a complete 2001-2019 dataset. See those script files
# for documented fixes to the dataset. 
# BEFORE USING THE .RDATA FILE TALK TO JUSTIN ABOUT ISSUES
load(here::here("../../Data/Database/PrudhoeCatch&LengthDataset_2001-2022_Version5.Rdata"))

curr_yr <- 2022



###################################
##### Scatterplots by species #####
###################################

# This sets the breaks for the plot below at 7/1, 7/15, 8/1, etc
break.vec <- ymd(paste0(curr_yr,"-06-26"))
break.vec <- c(break.vec, break.vec +days(7), break.vec + days(14), 
               break.vec + days(21), break.vec + days(28), break.vec + days(35), break.vec + days(42), 
               break.vec + days(49), break.vec + days(56), break.vec + days(63))

bdwfscatter <- ggplot(all.len %>% filter(Species=="BDWF", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Broad Whitefish Lengths, all sites combined"), y="Length (mm)", x="") 

arcsscatter <- ggplot(all.len %>% filter(Species=="ARCS", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Arctic Cisco Lengths, all sites combined"), y="Length (mm)", x="") 

lscsscatter <- ggplot(all.len %>% filter(Species=="LSCS", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Least Cisco Lengths, all sites combined"), y="Length (mm)", x="")  

dlvnscatter <- ggplot(all.len %>% filter(Year == curr_yr, Species=="DLVN", Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Dolly Varden Lengths, all sites combined"), y="Length (mm)", x="") 

hbwfscatter <- ggplot(all.len %>% filter(Species=="HBWF", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Humpback Whitefish Lengths, all sites combined"), y="Length (mm)", x="") 

sfcdscatter <- ggplot(all.len %>% filter(Species=="SFCD", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Saffron Cod Lengths, all sites combined"), y="Length (mm)", x="") 

arcdscatter <- ggplot(all.len %>% filter(Species=="ARCD", Year==curr_yr, Length > 35), aes(x=EndDate,y=Length)) + 
  geom_point(colour = "darkblue", alpha = 0.4, size = 1.5) +
  labs(title = paste0(curr_yr, " Arctic Cod Lengths, all sites combined"), y="Length (mm)", x="") 


format.figures <- function(x)
{ x <- x + theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(text=element_text(family="Times New Roman", size=12), 
        axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_x_date(labels = date_format("%m/%d/%Y"), limits = c(as.Date(paste0(curr_yr, "-06-26")), as.Date(paste0(curr_yr, "-08-26"))),
               breaks= break.vec) +
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y = c(0, 1.1 * max(x$data$Length, na.rm = TRUE)))

print(x)
}

format.figures(bdwfscatter)
format.figures(arcsscatter)
format.figures(lscsscatter)
format.figures(hbwfscatter)
format.figures(dlvnscatter)
format.figures(sfcdscatter)
format.figures(arcdscatter)
# ggsave(format.figures(bdwfscatter), file="../Figures/scatterplot_bdwf.png", width = 6, height = 4, units = "in", dpi = 200)
# ggsave(format.figures(arcsscatter), file="../Figures/scatterplot_arcs.png", width = 6, height = 4, units = "in", dpi = 600)
# ggsave(format.figures(lscsscatter), file="../Figures/scatterplot_lscs.png", width = 6, height = 4, units = "in", dpi = 600)
# ggsave(format.figures(hbwfscatter), file="../Figures/scatterplot_hbwf.png", width = 6, height = 4, units = "in", dpi = 600)
# ggsave(format.figures(dlvnscatter), file="../Figures/scatterplot_dlvn.png", width = 6, height = 4, units = "in", dpi = 600)
# ggsave(format.figures(sfcdscatter), file="../Figures/scatterplot_sfcd.png", width = 6, height = 4, units = "in", dpi = 600)
# ggsave(format.figures(arcdscatter), file="../Figures/scatterplot_arcd.png", width = 6, height = 4, units = "in", dpi = 600)




##################################
##### Species catch boxplot ######
##################################

allcatch %>% group_by(Species, Year) %>% summarise(total=sum(totcount)) %>%
spread(Species , value = total) # just to visualize everything
# 2018 was very middle of the road for most species catches


main_species <- list("ARCS", "BDWF", "LSCS", "DLVN", "HBWF", "RDWF", "ARFL", "FHSC", "SFCD")



annualcatches <- allcatch %>% group_by(Species, Year) %>% summarise(total=sum(totcount)) %>%
  left_join(spplookup, by = "Species") %>% 
  mutate(total = replace(total, Year == 2002 & Species == "ARCD", NA)) 
# This removes the ARCD 2002 catch which was MASSIVE and made it unable to compare catches
# Note in analysis that this outlier catch was removed!



ggplot(annualcatches %>% filter(Species %in% main_species), aes(x=commonname, y=total)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 50000), labels = comma) +
  geom_point(data=annualcatches %>% filter(Species %in% main_species, Year==curr_yr), 
             aes(x=commonname, y=total), col="red") +
  theme_bw() + theme(panel.grid.minor = element_blank()) + 
  theme(text=element_text(family="Times New Roman", size=8), 
        axis.text.x = element_text(angle = 35, hjust = 1)) +
  ylab("Total Annual Catch") + xlab("")
# ggsave(file="../Figures/speciescatch_boxplot.png", width = 6, height = 4, units = "in", dpi = 600)
#NOTE! The 400K ARCD catch was removed just so everything would fit




#########################
#### Temp & SALINITY ####
#########################

env2022 <- env_allyears %>% filter(Year == curr_yr) %>%
  mutate(meansalin = rowMeans(dplyr::select(., Salin_Top:Salin_Bot_1.5), na.rm = TRUE)) %>%
  mutate(meantemp = rowMeans(dplyr::select(., Temp_Top:Temp_Bot_1.5), na.rm = TRUE))

# Need to create blank rows for days/stations that weren't sampled
# This is so that the line will have "blanks" for these skipped days
env2022 <- left_join(expand.grid(Date=seq(from=min(env2022$Date), to=max(env2022$Date), by = "days"),
            Station = as.character(c(230, 220, 218, 214))), env2022, by = c("Date" = "Date", "Station" = "Station")) %>% 
  arrange(Date)


envlabels <- env2022 %>% group_by(Station) %>% 
  summarise(stnmeansal = mean(meansalin, na.rm = TRUE),
            stnmeantemp = mean(meantemp, na.rm = TRUE)) %>% 
  mutate(temp_labels = paste0(Station, "; Mean = ",format(round(stnmeantemp, 1), nsmall = 1), "°C"),
         sal_labels = paste0(Station, "; Mean = ", format(round(stnmeansal, 1), nsmall = 1), " ppt"))


stn_colors <- c("214" = "black", "218" = "#898989", "220" = "#898989", "230" = "black")
stn_lines  <- c("214" = "twodash", "218" = "dotted", "220" = "solid", "230" = "solid")


format.figures.env <- function(x)
{ x <- x + theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(text=element_text(family="Times New Roman", size=12), legend.title=element_blank(), 
        axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_x_date(labels = date_format("%m/%d/%Y"), limits = c(as.Date(paste0(curr_yr, "-06-26")), as.Date(paste0(curr_yr, "-09-01"))),
               breaks= break.vec) 
}


salinplot <- ggplot(data = env2022, 
       aes(x = Date, y = meansalin, color = Station, linetype = Station)) + 
  geom_line(cex = 1) + ylab("Salinity (ppt)") + xlab("") +
  scale_y_continuous(limits = c(0,35), breaks = seq(0, 35, by = 5))

print(format.figures.env(salinplot) + 
        scale_color_manual(values = stn_colors, labels = envlabels$sal_labels) + 
        scale_linetype_manual(values = stn_lines, labels = envlabels$sal_labels) + 
        theme(legend.position=c(.3, .84), legend.text = element_text(color = "black", size = 8), 
              legend.background = element_rect(fill = F)))
# Manually move the legend! 
#ggsave(file="../Figures/salinity_by_station.png", width = 6, height = 4, units = "in", dpi = 600)


tempplot <- ggplot(data = env2022, 
                    aes(x = Date, y = meantemp, color = Station, linetype = Station)) + 
  geom_line(cex = 1.05) + ylab("Temperature (°C)") + xlab("") +
  scale_y_continuous(limits = c(0,16), breaks = seq(0, 16, by = 2)) 

print(format.figures.env(tempplot) +  
        scale_color_manual(values = stn_colors, labels = envlabels$temp_labels) + 
        scale_linetype_manual(values = stn_lines, labels = envlabels$temp_labels) + 
        theme(legend.position=c(.3, .3), legend.text = element_text(color = "black", size = 8), 
              legend.background = element_rect(fill = F)))
#ggsave(file="../Figures/temp_by_station.png", width = 6, height = 4, units = "in", dpi = 600)





###################################
##### ANNUAL FAMILY CATCHES  #####
###################################

library(forcats)
allcatch.fam <- allcatch %>% left_join(spplookup %>% dplyr::select(-commonname)) %>%
  mutate(familygroup = fct_recode(familygroup, "Other Spp." = "Other"))
  # add species family groups, change "other" to "other spp."

report_theme <- function() {
  theme_bw() + 
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
          text=element_text(family="Times New Roman", size=12), 
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 35, hjust = 1),  
          panel.border = element_blank(), 
          axis.line = element_line())
}


fam_colors <- c("Coregoninae" = "#7fd8ff", "Pleuronectidae" = "#e28f28", 
                "Osmeridae" = "grey", "Gadidae" = "#fff175", "Salmoninae" = "#006ece", 
                "Cottidae" ="#007c12", "Other Spp." = "pink")


allcatch.fam %>% group_by(Year, familygroup) %>%
  summarise(famsum = sum(totcount, na.rm = TRUE)) %>%
  group_by(Year, familygroup) %>%
  summarise (totcount = sum(famsum)) %>%
  mutate(freq = totcount / sum(totcount),
         familygroup = factor(familygroup, # this reorders corrects
                              levels = c("Coregoninae", "Pleuronectidae", 
                                         "Osmeridae", "Gadidae", "Salmoninae", 
                                         "Cottidae", "Other Spp."))) %>%
  #above creates a df w/ proportion by family per year, then orders them
  ggplot(aes(x=Year, y=freq, fill=familygroup)) + geom_bar(stat="identity") +
         scale_fill_manual(values = fam_colors) + labs(y="", x = "") + 
         scale_x_continuous(expand=c(0,0), breaks= seq(2001, curr_yr)) + #expand removes whitespace
         scale_y_continuous(labels=percent, expand=c(0,0)) +
         report_theme()
# if there is a family that is NA, you have a new species (or incorrectly named species)
# ggsave(file="../Figures/annualfamilyproportions.png", width = 6, height = 4, units = "in", dpi = 600)





###################################
##### DAILY FAMILY CATCHES  #####
###################################


allcatch.fam %>% filter(Year == curr_yr) %>% group_by(EndDate, familygroup) %>%
  summarise(famsum = sum(totcount, na.rm = TRUE)) %>%
  group_by(EndDate, familygroup) %>%
  summarise (totcount = sum(famsum)) %>%
  mutate(freq = totcount / sum(totcount),
         familygroup = factor(familygroup, 
                              levels = c("Coregoninae", "Pleuronectidae", 
                                         "Osmeridae", "Gadidae", "Salmoninae", 
                                         "Cottidae", "Other Spp."))) %>%
  ggplot(aes(x=EndDate, y=freq, fill=familygroup)) + geom_bar(stat="identity") +
         scale_fill_manual(values = fam_colors) + labs(y="", x = "") + 
         #scale_x_continuous(expand=c(0,0), breaks= seq(2001, curr_yr)) + 
         scale_y_continuous(labels=percent, expand=c(0,0)) + #expand removes whitespace
         scale_x_date(expand=c(0,0), breaks = break.vec) + #UPDATE BREAKS
         report_theme()
# ggsave(file="../Figures/2022familyproportions.png", width = 6, height = 4, units = "in", dpi = 600)

