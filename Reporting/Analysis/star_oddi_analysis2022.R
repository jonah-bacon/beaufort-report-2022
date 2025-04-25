#Graphs and analyzes all Star-Oddi data 


# Load libraries ----------------------------------------------------------


library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(gtable)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(ggpubr)


# Import data -------------------------------------------------------------

niakuk <- read.csv(here::here("../../Star_Oddi/214_niakuk.csv"))
niakuk <- niakuk %>% 
  separate(datetime, into = c("date", "time"), sep = " ")
head(niakuk)
niakuk$time <- seconds_to_period(hm(niakuk$time))
niakuk$date <- mdy(niakuk$date, tz = "US/Alaska")
niakuk$datetime <- as.POSIXct(paste0(niakuk$date, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  niakuk$time
niakuk$depth <- scale(niakuk$depth, scale=F)
niakuk$site <- "214"

west_beach <- read.csv(here::here("../../Star_Oddi/218_wbeach.csv"))
west_beach <- west_beach %>% 
  separate(datetime, into = c("date", "time"), sep = " ")
west_beach$time <- hm(west_beach$time)
west_beach$date <- mdy(west_beach$date, tz = "US/Alaska")
west_beach$datetime <- as.POSIXct(paste0(west_beach$date, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  west_beach$time
west_beach$depth <- scale(west_beach$depth, scale=F)
west_beach$site <- "218"

west_dock <- read.csv(here::here("../../Star_Oddi/220_wdock.csv"))
west_dock <- west_dock %>% 
  separate(datetime, into = c("date", "time"), sep = " ")
west_dock$time <- hm(west_dock$time)
west_dock$date <- mdy(west_dock$date, tz = "US/Alaska")
west_dock$datetime <- as.POSIXct(paste0(west_dock$date, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  west_dock$time
west_dock$depth <- scale(west_dock$depth, scale=F)
west_dock$site <- "220"

endi <- read.csv(here::here("../../Star_Oddi/230_endicott.csv"))
endi <- endi %>% 
  separate(datetime, into = c("date", "time"), sep = " ")
endi$time <- hm(endi$time)
endi$date <- mdy(endi$date, tz = "US/Alaska")
endi$datetime <- as.POSIXct(paste0(endi$date, " " ,'00:00:00'), format = "%Y-%m-%d") + 
  endi$time
endi$depth <- scale(endi$depth, scale=F)
endi$site <- "230"

all_data <- rbind(endi, niakuk, west_beach, west_dock)


#This plots the average temperature by averaging the temperature at each site at each 
#time point. I needed this plot for a presentation, but kept the script. 
#avg_temp_data <- all_data %>% group_by(datetime) %>% summarise(mean_temp=mean(temp)) %>%
  #mutate(diff = mean_temp - lag(mean_temp,1, na.rm=T))

#ggplot(data=avg_temp_data, aes(x=datetime, y=mean_temp)) + geom_line() + 
  #scale_x_datetime(name="Date", labels = date_format("%m/%d/%Y", tz = "US/Alaska"), 
  #breaks = seq(as.POSIXct("2021-07-01"),as.POSIXct("2021-08-26"), "14 days")) +
  #scale_y_continuous(name="Average Temperature (°C)", breaks=seq(0,16,2)) + 
  #theme(axis.text.x = element_text(angle=30, hjust=1), 
        #text = element_text(family="Times New Roman", size=12))


# Temperature figure ------------------------------------------------------

cols <-  c("214" = "orange","218" = "darkred","220" = "blue", "230" = "darkgreen")
temp_plot <- ggplot(data=all_data, aes(x = datetime, y=temp, color = site)) + 
  geom_line(position = position_dodge(0.2), size=0.3) + 
  scale_x_datetime(name="", labels = date_format("%m/%d/%Y", tz = "US/Alaska"), 
                   breaks = seq(as.POSIXct("2021-07-01"),as.POSIXct("2021-08-26"), "14 days")) +
  scale_y_continuous(name="Temperature (°C)", breaks=seq(0,21,3)) + 
  scale_color_manual(name="Site", values=cols) + 
  theme(text=element_text(family = "Times New Roman", size=12))


# Salinity figure ---------------------------------------------------------


salinity_plot <- ggplot(data=all_data, aes(x = datetime, y=salinity, color = site)) + 
  geom_line(position = position_dodge(0.2), size=0.3) + 
  scale_x_datetime(name="", labels = date_format("%m/%d/%Y", tz = "US/Alaska"), 
                   breaks = seq(as.POSIXct("2021-07-01"),as.POSIXct("2021-08-26"), "14 days")) +
  ylab("Salinity (ppt)") + 
  scale_color_manual(name="Site", values=cols) + 
  theme(text=element_text(family = "Times New Roman", size=12))


# Depth figure ------------------------------------------------------------


depth_plot <- ggplot(data=all_data, aes(x = datetime, y=depth, color = site)) + 
  geom_line(position = position_dodge(0.2), size=0.3) + 
  scale_x_datetime(name="Time", labels = date_format("%m/%d/%Y", tz = "US/Alaska"), 
                   breaks = seq(as.POSIXct("2021-07-01"),as.POSIXct("2021-08-26"), "14 days")) +
  ylab("Depth (m)") + 
  scale_color_manual(name="Site", values=cols) + 
  theme(text=element_text(family = "Times New Roman", size=12))


# Save Star-Oddi Plot -----------------------------------------------------

ggarrange(temp_plot, salinity_plot, depth_plot, ncol=1, nrow=3, widths=c(2),
          heights = c(4), common.legend=T, legend = "right")
ggsave(file="../Figures/star_oddi_plots.png", width = 8, height = 10, units = "in", dpi = 600)

