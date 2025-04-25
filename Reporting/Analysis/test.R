allcatch %>% 
  filter(Year == curr_yr) %>% 
  group_by(Net) %>% 
  summarize("n.lengths" = sum(unique(LG)))
  
str(allcatch$LG)  
  distinct()
group_by(LINK) %>% 
  filter(LG !%in% c(1,2,3))

test.df <- allcatch
test.df$LG <- as.factor(test.df$LG)
sum(unique(test.df$LG), na.rm = T)

f <- test.df %>% 
  filter(Year == curr_yr) %>%
  group_by(EndDate, Net) %>% 
  summarize("unique" = length(unique(LG))) %>% 
  filter(unique == 1)
str(test.df$LG)

all.len %>% 
  filter(Year == curr_yr) %>% 
  filter(Station == 220) %>% 
  filter(EndDate %in% c("2022-07-16", "2022-07-31", "2022-08-02", "2022-08-05", "2022-08-10", "2022-08-18", "2022-08-20")) %>% 
  summarize(StartTime = StartDateTime, EndDateTime = EndDateTime)


g <- effort %>% 
  filter(Year == curr_yr) %>%
  group_by(EndDate) %>% 
  summarise("test" = length(Net))
View(g)

View(filter(effort, Year == curr_yr))
View(filter(CPUEallyears, Year == curr_yr & CPUE_day < 0.5))
View(filter(annualCPUE, Year == curr_yr))


curr_yr <- 2022
load(here::here("../../Data/Database/2022_FishLengths.csv"))

fishlengths <-read.csv(here::here("../../Data/Database/2022_FishLengths.csv"))
fishlengths1 <- fishlengths %>% 
  filter(Otolith == 1) %>% 
  group_by(Species) %>% 
  summarize("50-74" = length(Otolith[Length <= 74]),
            "75-99" = length(Otolith[Length >= 75 & Length <= 99]),
            "100-124" = length(Otolith[Length >= 100 & Length <= 124]),
            "125-149" = length(Otolith[Length >= 125 & Length <= 149]),
            "150-174" = length(Otolith[Length >= 150 & Length <= 174]),
            "175-199" = length(Otolith[Length >= 175 & Length <= 199]),
            "200-224" = length(Otolith[Length >= 200 & Length <= 224]),
            "225-249" = length(Otolith[Length >= 225 & Length <= 249]),
            "250-274" = length(Otolith[Length >= 250 & Length <= 274]),
            "275-299" = length(Otolith[Length >= 275 & Length <= 299]),
            "300-324" = length(Otolith[Length >= 300 & Length <= 324]),
            "325-349" = length(Otolith[Length >= 325 & Length <= 349]),
            "350-374" = length(Otolith[Length >= 350 & Length <= 374]),
            "375-399" = length(Otolith[Length >= 375 & Length <= 399]),
            "400-424" = length(Otolith[Length >= 400 & Length <= 424]),
            "425-449" = length(Otolith[Length >= 425 & Length <= 449]),
            "450-474" = length(Otolith[Length >= 450 & Length <= 474]),
            "475-499" = length(Otolith[Length >= 475 & Length <= 499]),
            "500-524" = length(Otolith[Length >= 500 & Length <= 524]),
            "525-549" = length(Otolith[Length >= 525 & Length <= 549]),
            "600-624" = length(Otolith[Length >= 600 & Length <= 624])
            )
            
fishlengths %>% 
  filter(Species == "SFCD") %>% 
  summarize("50-74" = length(Otolith[Length <= 74]),
            "75-99" = length(Otolith[Length >= 75 & Length <= 99]),
            "100-124" = length(Otolith[Length >= 100 & Length <= 124]),
            "125-149" = length(Otolith[Length >= 125 & Length <= 149]),
            "150-174" = length(Otolith[Length >= 150 & Length <= 174]),
            "175-199" = length(Otolith[Length >= 175 & Length <= 199]),
            "200-224" = length(Otolith[Length >= 200 & Length <= 224]),
            "225-249" = length(Otolith[Length >= 225 & Length <= 249]),
            "250-274" = length(Otolith[Length >= 250 & Length <= 274]),
            "275-299" = length(Otolith[Length >= 275 & Length <= 299]),
            "300-324" = length(Otolith[Length >= 300 & Length <= 324]),
            "325-349" = length(Otolith[Length >= 325 & Length <= 349]),
            "350-374" = length(Otolith[Length >= 350 & Length <= 374]),
            "375-399" = length(Otolith[Length >= 375 & Length <= 399]),
            "400-424" = length(Otolith[Length >= 400 & Length <= 424]),
            "425-449" = length(Otolith[Length >= 425 & Length <= 449]),
            "450-474" = length(Otolith[Length >= 450 & Length <= 474]),
            "475-499" = length(Otolith[Length >= 475 & Length <= 499]),
            "500-524" = length(Otolith[Length >= 500 & Length <= 524]),
            "525-549" = length(Otolith[Length >= 525 & Length <= 549]),
            "600-624" = length(Otolith[Length >= 600 & Length <= 624])
  )

view(filter(CPUE_by_station, Species == "SFCD"))
view(filter(CPUE_by_LG, 
            Species == "DLVN" & LG == "2"))
