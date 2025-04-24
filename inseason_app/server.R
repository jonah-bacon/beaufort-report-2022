# July 30 2021 version



shinyServer(
  # Function takes input from ui.R and returns output objects.
  
  function(input, output) {
    
    dataset.new <- reactive({
      if(input$sites != "All") {Pru_leng %>% filter(Species == input$sppchoice, Net == input$sites, 
                                                    LG == input$lengths | is.na(LG))
      } else {Pru_leng %>% filter(Species == input$sppchoice, LG == input$lengths | is.na(LG)) }
    })
    
    
    dataset.cor <- reactive({
      
      if(input$logtrans) { log(subset(tempcorr, select=c(input$corr_spp))+1) } 
      else {subset(tempcorr, select=c(input$corr_spp))}
      
    })
    
    
    
    output$lengthplot <- renderPlot({
      
      t <- ggplot(dataset.new(), aes(x=SampleDate, y=Length)) + 
        geom_point(colour = "dark blue", size = 1.5, alpha = 0.3) + labs(title = input$sppchoice) 
      if(input$adv_op){ t <- t + geom_smooth(aes(group=LG), method = input$smoothMethod)}
      t <- t + annotate(geom="text",x=as.POSIXct(as.Date(paste0(currentyear, "-7-20"))), 
                        y=max((Pru_leng %>% filter(Species == input$sppchoice))$Length)/1.4,
                        label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.8)
      
      #facet_wrap(~Net)
      print(t)
      
    })
    
    
    output$catchbydatelines <- renderPlot({
      #ggplot(Pru_tot, aes(x=SampleDate, y=Count)) + geom_bar(stat = "identity", aes(fill=Net))
      
      if(input$chartdatelines) {
        t <- ggplot(Pru_tot %>% group_by(SampleDate, Net, Species) %>% filter(Species == input$sppchoice) %>%
                      summarize(TotCount = sum(Count)), aes(x=SampleDate, y=TotCount, color=Net)) + 
          geom_line(stat = "identity", cex=1.5) + labs(title = input$sppchoice) } else{
            t <- ggplot(Pru_tot %>% group_by(SampleDate, Species) %>% filter(Species == input$sppchoice) %>%
                          summarize(TotCount = sum(Count)), aes(x=SampleDate, y=TotCount)) + 
              geom_line(stat = "identity", cex=1.5) + labs(title = input$sppchoice) 
            
          }
      t <- t + annotate(geom="text",x=as.Date(paste0(currentyear, "-7-17")), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
                        y=max((Pru_tot %>% filter(Species == input$sppchoice))$Count)/1.4,
                        label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)
      print(t)
    })
    
    output$catchbydatespp <- renderPlot({
      t <- ggplot(Pru_tot, aes(x=SampleDate, y=Count))
      if(input$catchbydate_inp == "Net"){t <- t + geom_bar(stat = "identity", aes(fill = Net)) 
      } else { t <- t + geom_bar(stat = "identity", aes(fill = topspp))}
      t <- t + annotate(geom="text",x=as.Date(paste0(currentyear, "-7-17")), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
                        y=max(Pru_tot$Count)/1.4,
                        label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)
      print(t)
    })
    
    
    
    output$totalcatch <- renderPlot({
      if(input$netgroups == "None"){
        ggplot(Pru_tot, aes(x=Net, y=Count)) + geom_histogram(stat = "identity")}
      else{  
        ggplot(Pru_tot, aes(x=Net, y=Count)) + geom_histogram(aes_string(fill = input$netgroups), stat = "identity")
        
      }
    })
    
    output$catch_corr <- renderPlot({
      #corrgram(dataset.cor(),
      #         main="Correlation between log transformed species catches",
      #         lower.panel=panel.pts, upper.panel=panel.conf,
      #         diag.panel=panel.density)
      
      corrplot.mixed(cor(dataset.cor()), upper = "circle", lower = "number")
      
    })
    
    
    output$cumcatch <- renderPlot({
      if(input$sppchoice_all != "ALL") {
        ggplot() +
          geom_line(data = allcatch %>% group_by(Year, day.of.year) %>% filter(Species == input$sppchoice_all) %>% 
                      summarize(totcount = sum(totcount)) %>% group_by(Year) %>% arrange(day.of.year) %>% 
                      mutate(cummsum = cumsum(totcount), 
                             currentyeardate = as.Date(day.of.year, origin = paste0(currentyear, "-01-01"))),
                    #Previous call to data, takes in all years' catch, filters it correctly, then summarizes
                    #the cumm catch by day of year, then puts that day of year back into the current year's date
                    #that way it will be on the same date scale as the current catches
                    aes(x = currentyeardate, y = cummsum, group = Year, color = Year), cex = 1) + 
          geom_line(data = Pru_tot %>% group_by(SampleDate) %>% filter(Species == input$sppchoice_all) %>% 
                      summarize(totcount = sum(Count)), 
                    aes(x=SampleDate, y = cumsum(totcount)), cex=2, color = "red")+
          ggtitle(paste0("Cumulative ", input$sppchoice_all, " counted")) + 
          #cale_colour_manual("", breaks = c("2016", "2017"), values = c("orange", "dark blue")) +
          xlab("Date") + ylab("Cumulative number of fish counted") 
        # annotate(geom="text",x=as.Date("2018-7-30"), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
        #          y=max(cumsum(filter(catch2016, Species == input$sppchoice_all)$totcount))/2.1,
        #          #had to use different filter method here....
        #          label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)
      }
      
      else {
        ggplot() +
          geom_line(data = allcatch %>% group_by(Year, day.of.year) %>% 
                      summarize(totcount = sum(totcount)) %>% group_by(Year) %>% arrange(day.of.year) %>% 
                      mutate(cummsum = cumsum(totcount), 
                             currentyeardate = as.Date(day.of.year, origin = paste0(currentyear, "-01-01"))), 
                    aes(x=currentyeardate, y = cummsum, group = Year, color = Year), cex=1) +
          geom_line(data = Pru_tot %>% group_by(SampleDate) %>% summarize(totcount = sum(Count)), 
                    aes(x=SampleDate, y = cumsum(totcount)), color = "red", cex=2) +
          ggtitle("Cumulative fish counted, all species") +
          # scale_colour_manual("", breaks = c("2016", "2017"), values = c("orange", "dark blue")) +
          xlab("Date") + ylab("Cumulative number of fish counted")
        # annotate(geom="text",x=as.Date("2018-7-30"), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
        #          y=max(cumsum(catch2016$totcount))/1.4, ## Just to take max of previous year
        #          label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)
      }
    })
    
    
    output$cummeasure <- renderPlot({
      if(input$sppchoice_all != "ALL") {
        ggplot() +
          geom_line(data = length2001_end %>% group_by(EndDate=(as.Date(EndDate, "%Y-%m-%d"))+years(1)) %>%
                      filter(Species == input$sppchoice_all) %>% summarise(cumnum = sum(num)), 
                    aes(x=as.Date(EndDate, "%Y-%m-%d"), y = cumsum((cumnum)), color = "2016"), cex=2) +
          geom_line(data = Pru_lengtemp  %>% group_by(SampleDate) %>% filter(Species == input$sppchoice_all) %>%
                      summarise(cumnum = sum(num)), 
                    aes(x=as.Date(SampleDate , "%Y-%m-%d"), y = cumsum((cumnum)), color = "2017"), cex=2) +
          ggtitle("Cumulative fish measured") + 
          scale_colour_manual("", breaks = c("2016", "2017"), values = c("orange", "dark blue")) +
          xlab("Date") + ylab("Cumulative number of fish measured") +
          annotate(geom="text",x=as.Date(paste0(currentyear, "-8-4")), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
                   y=max(cumsum(filter(length2001_end, Species == input$sppchoice_all)$totcount))/2.3,
                   #had to use different filter method here....
                   label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)}
      
      
      else {
        ggplot() +
          geom_line(data = length2001_end %>% 
                      group_by(EndDate=(as.Date(EndDate, "%Y-%m-%d")) + years(1)) %>% 
                      summarise(cumnum = sum(num)), 
                    aes(x=as.Date(EndDate, "%Y-%m-%d"), y = cumsum((cumnum)), color = as.character(currentyear)), cex=2) +
          geom_line(data = Pru_lengtemp %>% group_by(SampleDate) %>% summarise(cumnum = sum(num)), 
                    aes(x=as.Date(SampleDate , "%Y-%m-%d"), y = cumsum((cumnum)), color = "2017"), cex=2)+
          ggtitle("Cumulative fish measured") +
          scale_colour_manual("", breaks = c("2016", "2017"), values = c("orange", "dark blue")) +
          xlab("Date") + ylab("Cumulative number of fish measured") +
          annotate(geom="text",x=as.Date(paste0(currentyear, "-7-30")), ###NOTE IF USING IN FUTURE YEARS, CHANGE THIS!!!###
                   y=max(cumsum(length2001_end$num))/1.4, ## Just to take max of previous year
                   label="Preliminary", angle = 30, size = 20, fontface="bold", color="white", alpha = 0.5)}
      
    })
    
    
    output$speciescount <- renderPlot({
      if(input$bysite == "bysite"){
        ggplot(Pru_tot %>% group_by(SampleDate, Net) %>% summarise(numspp = n_distinct(Species)), 
               aes(SampleDate, numspp, color = Net)) +
          geom_point() + xlab("Date") + ylab("Number of fish species")}
      else{
        ggplot(Pru_tot %>% group_by(SampleDate) %>% summarise(numspp = n_distinct(Species)), 
               aes(SampleDate, numspp)) +
          geom_point() + geom_smooth(method = "lm") + xlab("Date") + ylab("Number of fish species")
        
      }
      
    })
    
    output$tempsalin <- renderPlot({
      ggplot(Env_tempsalin, aes_string(Env_tempsalin$SalinTop, Env_tempsalin$TempTop, color = input$tempcontrols)) + 
        geom_point(cex=5) + xlab("Water Salinity, Top") + ylab("Water Temperature, Top")
      
      
    })
    
    output$text1 <- renderText({ 
      paste0("Selected dataset consists of ", input$chart_type)
    })
    
    
    output$catchsumm = renderTable(catchsummary)
    
    output$lensumm = renderTable(lengthsummary)
    
    
    
    ######### Mapping
    
    #########COMMENTING OUT NEXT 65 lines
    
    # output$Prudinterpmap <- renderLeaflet({
    #   
    #   
    #   leaflet() %>% addTiles() %>% setView(-148.2, 70.35, zoom = 10)    })
    # 
    # 
    # #reactive({
    # #   if(input$mapsalintemp == "Temp"){
    # #     jtpcolors <- colorNumeric("RdYlBu", domain=5:15, alpha = FALSE, reverse = FALSE) }
    # #   else{
    # #     jtpcolors <- colorNumeric("Spectral", domain=0:40, alpha = FALSE,reverse = FALSE) }
    # #  })
    # 
    # 
    # observe({
    #   #color palette
    #   if(input$mapsalintemp == "Temp"){
    #     jtpcolors <- colorNumeric("RdYlBu", domain=5:17, na.color = "transparent", alpha = FALSE, reverse = TRUE) }
    #   else{
    #     jtpcolors <- colorNumeric("Greens", domain=0:32, na.color = "transparent", alpha = FALSE, reverse = FALSE) }
    #   
    #   
    #   temp1 <- left_join(Pru_locations, Env_tempsalin %>% filter(SampleDate == as.Date(input$mapslider)) %>% 
    #                        group_by(Site)) 
    #   temp <- temp1[complete.cases(temp1), ]
    #   
    #   
    #   interp_values <- as.numeric(unlist(temp[,paste0(input$mapsalintemp,input$maptopbot)]))
    # 
    #   frame.xy <- as.data.frame(cbind(x=as.numeric(temp$Long),y=as.numeric(temp$Lat)))
    #   coordinates(frame.xy) <- ~x+y # This turns it into a 'SpatialPoints' class
    #   is.na(interp_values)
    # 
    #   #Create a data frame grid from all combinations of the supplied vectors or factors.
    #   grd1 <- expand.grid(x = seq(from = -148.63, to = -147.8, by = 0.0012), # this is the range of interpolation
    #                       y = seq(from = 70.28, to = 70.425, by = 0.0007))  # and the resolution
    # 
    #   coordinates(grd1) <- ~x + y
    #   gridded(grd1) <- TRUE
    # 
    #   # Now interpolate surface using the idw model
    #   idw <- idw(formula = interp_values ~ 1, locations = frame.xy, newdata = grd1, idp = .4, na.action = na.omit) 
    #   # Chose IDP=.4 by trial/error
    #   interp_raster = raster(idw, "var1.pred") # turn this IDW into a raster of predicted values 
    #   
    #   # Finally, we mask the raster IDW grid by the polygon of the coast (crops to just water)
    #   testraster <- mask(interp_raster, Prud_coast2, inverse = TRUE)
    #   crs(testraster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    #   
    #   
    #   leafletProxy("Prudinterpmap") %>% clearImages() %>% clearControls() %>% # removes prev raster and prev legend
    #     clearMarkers() %>%
    #     addRasterImage(testraster, colors = jtpcolors) %>%
    #     addLegend("bottomright", pal = jtpcolors, values = interp_values,
    #               title = input$mapsalintemp, opacity = 0.5) %>%
    #     addMarkers(data = Pru_locations, popup = ~Site, layerId = Pru_locations$Site)
    #   #leafletProxy("Prudinterpmap") clearMarkers() %>%
    #   
    #   #input$Prudinterpmap_marker_mouseover
    #   
    #   #addControl(radioButtons("testinput", "can you see this", c("Salinity" = "Salin", "Temperature" = "Temp")),
    #   #                      position="bottomleft")
    #   
    #   
    #   
    #   click<-input$Prudinterpmap_marker_click
    #   if(is.null(click)){
    #     return()  }
    #   
    # })
    
    
    #Maybe save to implement later
    # observeEvent(input$showminicharts == TRUE, {
    #   temp1 <- left_join(Pru_locations, Env_tempsalin %>% filter(SampleDate == as.Date(input$mapslider)) %>% 
    #                        group_by(Site)) 
    #   
    #   leafletProxy("Prudinterpmap") %>% clearPopups() %>% clearImages() %>% clearControls() %>%
    #     addMinicharts(temp1$Long, temp1$Lat, chartdata = temp1[, c("SalinTop", "SalinMid")],
    #                   width = 45, height = 45)
    # })
    
    # When circle is hovered over...show a popup
    #observeEvent(input$Prudinterpmap_marker_mouseover$id, {
    #pointId <- input$Prudinterpmap_marker_mouseover$id
    #lat = as.numeric(Pru_locations[Pru_locations$Site == pointId, "Lat"])
    #lng = as.numeric(Pru_locations[Pru_locations$Site == pointId, "Long"])
    #leafletProxy("Prudinterpmap") %>% addPopups(lat = lat + .03, lng = lng, as.character("testing "))
    
    #absolutePanel(top = 120, left = 120, 
    #              ggplot(Env_tempsalin, aes(x=SalinTop, y=TempTop)) + geom_line())
    
    #})
    
    
    #Later code to use 
    # if(Sys.Date() > max(Pru_leng$SampleDate)) 
    
  }
)


