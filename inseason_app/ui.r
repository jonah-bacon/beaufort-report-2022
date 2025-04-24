


#load("../Data/Database/PrudhoeCatch&LengthDataset_2001-2020_Version13.Rdata")
#source("code/helpers_prudhoe2021.R")
source("helpers_prudhoe2021.R")



shinyUI(fluidPage(
  useWaiter(), 
  waiterPreloader(), #optional loading screen that only shows briefly
  theme = shinytheme("sandstone"),
                  navbarPage("Prudhoe Bay 2021 In-Season Results", 
                             
                             #titlePanel("test"),
                             
                             tabPanel("Charts",
                                      sidebarLayout(
                                        sidebarPanel(
                                          tags$head(tags$style(HTML("hr {border-top: 1px solid #b3b3b3;}"))
                                          ),
                                          h2("Chart Options"),
                                          hr(),
                                          conditionalPanel(condition = "input.mainpanels_id == 'Catch by Date'", 
                                                           selectInput("chart_type", label = h3("Select chart type"), 
                                                                       choices = list("Bar chart" = "bar_dates", "Line chart" = "line_dates"), 
                                                                       selected = "bar_dates")), 
                                          conditionalPanel(condition = "input.mainpanels_id == 'Length Plot' || 
                     input.mainpanels_id == 'Catch by Date' & input.chart_type == 'line_dates'",
                                                           selectInput("sppchoice", label = h3("Select species"), 
                                                                       choices = list("Broad Whitefish" = "BDWF", "Least Cisco" = "LSCS", "Arctic Cisco" = "ARCS", 
                                                                                      "Dolly Varden" = "DLVN", "Humpback Whitefish" = "HBWF", "Arctic Cod" = "ARCD",
                                                                                      "Saffron Cod" = "SFCD"), selected = "BDWF")),
                                          conditionalPanel(condition = "input.mainpanels_id == 'Cumulative Counts'", 
                                                           selectInput("sppchoice_all", label = h3("Select species"), 
                                                                       choices = list("All species" = "ALL", "Arctic Cisco" = "ARCS", "Arctic Cod" = "ARCD",
                                                                                      "Arctic Flounder" = "ARFL", "Broad Whitefish" = "BDWF", "Dolly Varden" = "DLVN", 
                                                                                      "Fourhorn Sculpin" = "FHSC", "Humpback Whitefish" = "HBWF", "Least Cisco" = "LSCS", 
                                                                                      "Rainbow Smelt" = "RBSM", "Saffron Cod" = "SFCD"), 
                                                                       selected = "ALL")),
                                          
                                          #selectInput("sites", label = h3("Select sites"), 
                                          #            choices = list("Endicott" = "230", "Niakuk" = "214", "West Beach" = "218"), 
                                          #                            selected = "230", multiple = TRUE),
                                          
                                          #checkboxGroupInput("sites","Choose sites:", 
                                          #                   choices = list("All" = "All", "Endicott" = "230", 
                                          #                                   "Niakuk" = "214", "West Beach" = "218", "West Dock" = "220"), 
                                          #                   selected = c("230", "214", "218", "220")),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id == 'Catch Correlations'", 
                                                           checkboxGroupInput("corr_spp", "Select Species correlations",
                                                                              choices = list("Arctic cisco" = "ARCS",  "Arctic Cod" = "ARCD", "Arctic Flounder" = "ARFL",
                                                                                             "Broad Whitefish" = "BDWF", "Dolly Varden" = "DLVN", "Fourhorn Sculpin" = "FHSC",
                                                                                             "Least Cisco" = "LSCS", "Humpback Whitefish" = "HBWF", "Rainbow Smelt" = "RBSM",
                                                                                             "Round Whitefish" = "RDWF", "Saffron Cod" = "SFCD"), 
                                                                              selected = c("ARCS", "BDWF", "LSCS"))
                                          ),
                                          conditionalPanel(condition = "input.mainpanels_id == 'Species Richness'", 
                                                           radioButtons("bysite", "Select a grouping", 
                                                                        c("Group all sites" = "None", "Separate by site" = "bysite"))),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id == 'Catch by Site'", 
                                                           radioButtons("netgroups", "Select a grouping", 
                                                                        c("None" = "None", "By top species" = "topspp", "By Species Group" = "Family"))),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id == 'Length Plot'",
                                                           selectInput("sites","Choose sites:", choices = list("All" = "All", "Endicott" = "230", 
                                                                                                               "Niakuk" = "214", "West Beach" = "218", "West Dock" = "220"), 
                                                                       selected = c("All")),
                                                           checkboxGroupInput("lengths","Select length groups:", choices = list("Length Group 1" = "1", 
                                                                                                                                "Length Group 2" = "2", "Length Group 3" = "3"), selected = c("1", "2", "3")), 
                                                           
                                                           hr(),
                                                           checkboxInput("adv_op", em("Add smoother?"), value = FALSE),
                                                           
                                                           conditionalPanel(
                                                             condition = "input.adv_op",
                                                             selectInput("smoothMethod", "Smoothing Method", list("lm", "loess")))
                                          ),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id =='Catch Correlations' ",
                                                           hr(),
                                                           checkboxInput("logtrans", em("Log Transform?"), value = FALSE)),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id == 'Temperature and Salinity'", 
                                                           radioButtons("tempcontrols", "Color by:", 
                                                                        c("Sample Date" = "SampleDate", "Sample Location" = "Site"))),
                                          
                                          
                                          conditionalPanel(condition = "input.chart_type == 'line_dates' & input.mainpanels_id == 'Catch by Date'",
                                                           checkboxInput("chartdatelines", "Show by site?", value = FALSE) ),
                                          
                                          conditionalPanel(condition = "input.chart_type == 'bar_dates' & input.mainpanels_id == 'Catch by Date'",
                                                           radioButtons("catchbydate_inp", "select a choice", c("By top species"= "topspp", 
                                                                                                                "by site" = "Net")) ),
                                          
                                          conditionalPanel(condition = "input.mainpanels_id == 'Cumulative Counts'",
                                                           radioButtons("cumm_input", "Select Comparison", c("Number caught" = "catch", 
                                                                                                             "Number measured" = "meas")))
                                          
                                        ),
                                        
                                        
                                        mainPanel(
                                          tabsetPanel(
                                            
                                            tabPanel("Length Plot", plotOutput("lengthplot")),
                                            
                                            tabPanel("Catch by Date", conditionalPanel(condition = "input.chart_type == 'bar_dates'",
                                                                                       plotOutput("catchbydatespp")),
                                                     conditionalPanel(condition = "input.chart_type == 'line_dates'",
                                                                      plotOutput("catchbydatelines")
                                                     )
                                            ),
                                            #tabPanel("Catch by date 2", plotOutput("catchbydatelines"),
                                            #         checkboxInput("chartdatelines", "Show by site?", value = FALSE)
                                            #         ),
                                            tabPanel("Catch Correlations", plotOutput("catch_corr")),
                                            tabPanel("Catch by Site", plotOutput("totalcatch")),
                                            tabPanel("Cumulative Counts", conditionalPanel(condition = "input.cumm_input == 'meas'",
                                                                                           plotOutput("cummeasure")),
                                                     conditionalPanel(condition = "input.cumm_input == 'catch'",
                                                                      plotOutput("cumcatch")),
                                                     br(), em("Historical catch to be added at a future date")),
                                            tabPanel("Species Richness", plotOutput("speciescount")),
                                            tabPanel("Temperature and Salinity", plotOutput("tempsalin")),
                                            id="mainpanels_id"
                                            #tabPanel("About", h4("put text here"))
                                            
                                            
                                          )
                                        )
                                        
                                      )
                             ),#end charts tabpanel
                             tabPanel("Summary", h1("Summary of Results"),
                                      column(6, h2(paste0("Total catch so far is ", prettyNum(sum(Pru_tot$Count), big.mark=","))),
                                             h3("Top Species Caught"), tableOutput("catchsumm")),
                                      column(6, h2(paste0("Of which ", prettyNum(length(Pru_leng$LINK), big.mark=","), " were measured."))),
                                      br(), 
                                      h3("Top Species Measured"), tableOutput("lensumm")),
                             #####COMMENTING OUT MAPPING SECTION - JULY 2018
                             # tabPanel("Mapping", leafletOutput("Prudinterpmap", width="100%"),
                             #          #absolutePanel(top=90, right = 120, checkboxInput("showminicharts", "Show Mini charts?", value = FALSE)),
                             #          fluidRow(column(6, sliderInput("mapslider", "",
                             #                                         min = min(Env_tempsalin$SampleDate), max = max(Env_tempsalin$SampleDate), 
                             #                                         value = min(Env_tempsalin$SampleDate), timeFormat="%m-%d-%Y")), 
                             #                   column(6, radioButtons("mapsalintemp", "", c("Salinity" = "Salin", "Temperature" = "Temp"))),
                             #                   column(6, radioButtons("maptopbot", "Where in Water Column?", 
                             #                                          c("Top" = "Top", "Mid-water" = "Mid", "Bottom" = "Bot")))),
                             #          p("Please wait about 7 seconds for rasters to load")
                             # ),
                             
                             #absolutePanel(top=160, left = 50, sliderInput("mapslider", "",
                             #             min = min(Env_tempsalin$SampleDate), max = max(Env_tempsalin$SampleDate), 
                             #             value = min(Env_tempsalin$SampleDate), timeFormat="%m-%d-%Y"),
                             # radioButtons("mapsalintemp", "", c("Salinity" = "Salin", "Temperature" = "Temp")),
                             # radioButtons("maptopbot", "Where in Water Column?", c("Top" = "Top", "Mid-water" = "Mid", "Bottom" = "Bot"))
                             # )),
                             tabPanel("About", h3("About this application:"),
                                      h4("The Prudhoe Bay fisheries studies have been occurring since approximately 1982. 
                              The 2021 field season is the fifth year that the University of Alaska Fairbanks,
                              College of Fisheries and Ocean Sciences has taken over management of the project."), 
                                      h4("This application summarizes the in-season results from the 2021 season. As such,
                              all results herein are preliminary. Development of the app is ongoing, with new
                              features being added occasionally"), br(),
                                      p("If you have questions or suggestions, please contact Justin Priest,
                             justin.priest@alaska.gov"),
                                      br(),
                                      p("Application version 1.1")
                             )
                             
)))









