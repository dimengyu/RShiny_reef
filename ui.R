library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(colourpicker)
library(DT)
library(plyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(sf)
library(plotly)
library(ggExtra)
library(oce)
library(ggpubr)
library(mice)
library(ggcorrplot)
library(caret)
library(e1071)
source("plt_map.R")

#### prepare filename and dataset
fn_ssh <- Sys.glob("data/SSH/*.nc")
time_ssh <- substr(fn_ssh, 38, 41)

fn_uv <- Sys.glob("data/UV/*.he5")
time_uv <- substr(fn_uv, 28, 31)

fn_co2 <- Sys.glob("data/CO2/*.csv")
time_co2 <- substr(fn_co2, 15, 18)

fn_precipitation <- Sys.glob("data/PRECIPITATION/*.HDF5")
time_precipitation <- substr(fn_precipitation, 40, 43)

fn_st <- Sys.glob("data/TEMPERATURE_SALINITY/*.nc")
time_st <- substr(fn_st, 63, 66)

dashboardPage(
  # 1. Header
  dashboardHeader(title = "Reef Dashboard"),
  
  # 2. Sidebar
  dashboardSidebar(
    # 2.1 Sidebar Menu
    sidebarMenu(
      menuItem("Visualisation", tabName = "map", icon = icon("map")),
      menuItem("Model", tabName = "model", icon = icon("chart-line")),
      menuItem("Reference", tabName = "ref", icon = icon("file"))
      ) # end of 2.1 Sidebar Menu

  ), # end of 2 Sidebar
  
  # 3. Body
  dashboardBody(
    # 3.1 Tab Items
    tabItems(
      # 3.1.1 First tab - Map
      tabItem(tabName = "map",
              # 3.1.1.1 First Row: include selector and maps
              fluidRow(
                # selectors
                box(width = 3,
                    height = 470, 
                    # 1st selector - factor
                    pickerInput(
                      'factor',
                      'Factor',
                      c("SSH", "PRECIPITATION", "TEMPERATURE", "SALINITY", "UV"),
                      multiple = F,
                      selected = "SSH"
                    ), # end of 1st selector
                    
                    # 2nd selector - year (no end required)
                    # 2nd selector 1
                    conditionalPanel(
                      condition = "input.factor == 'CO2'",
                      pickerInput(
                        'time1',
                        'Year',
                        time_co2,
                        multiple = F
                      )
                    ), # end of 2nd selector 1 
                    
                    # 2nd selector 2
                    conditionalPanel(
                      condition = "input.factor == 'PRECIPITATION'",
                      pickerInput(
                        'time2',
                        'Year',
                        time_precipitation,
                        multiple = F
                      )
                    ),# end of 2nd selector 2
                    
                    # 2nd selector 3
                    conditionalPanel(
                      condition = "input.factor == 'SSH'",
                      pickerInput(
                        'time3',
                        'Year',
                        time_ssh,
                        multiple = F
                      )
                    ),# end of 2nd selector 3
                    
                    # 2nd selector 4
                    conditionalPanel(
                      condition = "input.factor == 'SALINITY'",
                      pickerInput(
                        'time4',
                        'Year',
                        time_st,
                        multiple = F
                      )
                    ),# end of 2nd selector 4
                    
                    # 2nd selector 5
                    conditionalPanel(
                      condition = "input.factor == 'TEMPERATURE'",
                      pickerInput(
                        'time5',
                        'Year',
                        time_st,
                        multiple = F
                      )
                    ),# end of 2nd selector 5
                    
                    # 2nd selector 6
                    conditionalPanel(
                      condition = "input.factor == 'UV'",
                      pickerInput(
                        'time6',
                        'Year',
                        time_uv,
                        multiple = F
                      )
                    ), # end of 2nd selector 6
                  
                    # 3rd selector - bleaching level
                    sliderInput(
                      "cutoff",
                      'Set a bleaching threshold (%)',
                      min = 0,
                      max = 100,
                      value = 5
                    ), # end of 3rd selector
                    
                    # # 4th selector - interactive or static
                    # radioButtons(
                    #   'view',
                    #   'Interactive Map',
                    #   c("No", "Yes"),
                    #   selected = "No",
                    #   inline = T
                    # ), # end of 4th selector
                    
                    # 5th selector - colorpicker
                    colourInput("low_col", "Pick a color for lowest value:", "#101f60"),
                    colourInput("high_col", "Pick a color for highest value:", "#d9aea7"),
                    
                    # 6th selector - action button
                    actionButton("create", "Generate Map", icon = icon("play"))
                    ), # end of selectors
                
                # map
                tabBox(width = 9,
                       height = 470,
                       title = "Map",
                       # The id lets us use input$view on the server to find the current tab
                       id = "view", 
                       tabPanel("Static", plotOutput('staticMap')),
                       tabPanel("Interactive", plotlyOutput("plotlyMap"))
                       ) # end of map
                
                ), # end of 3.1.1.1 First Row
              
              # 3.1.1.2 Second Row: distribution of bleaching and selected factor
              fluidRow(
                # table of selected factor by bleaching status
                box(width = 5,
                    title = "Data",
                    status = "primary",
                    solidHeader = T,
                    dataTableOutput('data_table')
                    ), # end of table
                
                # plot of relationship 
                box(width = 3,
                    title = "Distribution of selected factor by bleaching status",
                    status = "primary",
                    solidHeader = T,
                    plotOutput('relationship_plot')
                    ), # end of plot of relationship 
                
                # scatterplot of distribution - selected factor
                box(width = 4,
                    title = "Average bleaching vs. selected factor",
                    status = "warning",
                    solidHeader = T,
                    plotOutput('scatter_marginal')
                    ) # end of plot of distribution - selected factor
                ) # end of 3.1.1.2 Second Row
              
      ), # end of 3.1.1 First tab
      
      # 3.1.2 Second tab - Model
      tabItem(tabName = "model",
              h2("Predictive Model and Performance"),
              # 3.1.2.1 First Row: include corrplot, PCA and pre-validation cv accuracy
              fluidRow(
                # corplot
                box(width = 3,
                    title = "Corrplot",
                    status = "primary",
                    solidHeader = T,
                    plotOutput('corr_plot')
                    ), # end of corplot
                # PCA
                box(width = 5,
                    title = "PCA",
                    status = "primary",
                    solidHeader = T,
                    plotOutput('pca_plot')),

                # cv accuracy
                box(width = 4,
                    title = "CV Accuracy",
                    status = "primary",
                    solidHeader = T,
                    imageOutput("cv_plot"))
              ), # end of 3.1.2.1
              
              # 3.1.2.2 Second Row: include 
              fluidRow(
                box(width = 6, 
                    title = "Stability",
                    status = "primary",
                    solidHeader = T,
                    imageOutput("stability_plot")
                    ), 
                valueBox(
                  width = 3,
                  value = "78.08%",
                  subtitle = "Median Accuracy of Random Forest",  
                  icon = tags$i(class = "fas fa-thumbs-up", style="font-size: 50px; color: white"),
                  color = "aqua"
                ), 
                valueBox(
                  width = 3,
                  value = "77.94%",
                  subtitle = "Median Accuracy of Logistic Regression on RF",
                  color = "light-blue"
                ),
                valueBox(
                  width = 3,
                  value = "75.02%",
                  subtitle = "Median Accuracy of SVM",
                  color = "green"
                ),
                valueBox(
                  width = 3,
                  value = "69.64%",
                  subtitle = "Median Accuracy of KNN",
                  icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 50px; color: red"),
                  color = "yellow"
                ),
                valueBox(
                  width = 3,
                  value = "74.80%",
                  subtitle = "Median Accuracy of Logistic Regression on SVM",  
                  color = "red"
                  # , icon = tags$i(class = "fas fa-thumbs-down", style="font-size: 50px; color: white"),
                )
              ) # end of 3.1.2.2
                    
      ), # end of 3.1.2 Second tab
      
      # 3.1.3 Third tab - Reference
      tabItem(tabName = "ref",
              h2("Reference"),
              br(),
              p("1. Sully, S., Burkepile, D., Donovan, M., Hodgson, G. and van Woesik, R., 2019. A global analysis of coral bleaching over the past two decades. Nature Communications, 10(1).", a(href="https://www.nature.com/articles/s41467-019-09238-2", "[online]"), style = "font-size:16px;"),
              br(),
              p("2. Search.earthdata.nasa.gov. 2012. AIRS/Aqua L3 Monthly CO2 in the free troposphere (AIRS+AMSU) 2.5 degrees x 2 degrees V005 (AIRX3C2M) at GES DISC.", a(href="https://search.earthdata.nasa.gov/search/granules/collection-details?p=C1238517293-GES_DISC&pg[0][v]=f&pg[0][gsk]=-start_date&tl=1649489823!3!!&fs10=Carbon%20And%20Hydrocarbon%20Compounds&fsm0=Atmospheric%20Chemistry&fs20=Carbon%20Dioxide&fst0=Atmosphere&fs21=Atmospheric%20Carbon%20Dioxide&fs11=Carbon%20And%20Hydrocarbon%20Compounds&fsm1=Atmospheric%20Chemistry&fst1=Atmosphere&lat=-0.28125&zoom=0", "[online]"), style = "font-size:16px;"),
              br(),
              p("3. Search.earthdata.nasa.gov. 2018. ECCO Ocean Temperature and Salinity - Monthly Mean 0.5 Degree (Version 4 Release 4).", a(href="https://search.earthdata.nasa.gov/search/granules/collection-details?p=C1990404795-POCLOUD&pg[0][v]=f&pg[0][gsk]=-start_date&tl=1328111912!3!!&fs10=Salinity&fsm0=Salinity/Density&fst0=Oceans&lat=-0.28125&zoom=0", "[online]"), style = "font-size:16px;"),
              br(),
              p("4. Search.earthdata.nasa.gov. 2018. ECCO Sea Surface Height - Daily Mean 0.5 Degree (Version 4 Release 4).", a(href="https://search.earthdata.nasa.gov/search/granules/collection-details?p=C1990404813-POCLOUD&fs10=Sea%20Surface%20Height&fsm0=Sea%20Surface%20Topography&fst0=Oceans&lat=-0.28125&zoom=0", "[online]"), style = "font-size:16px;"),
              br(),
              p("5. Search.earthdata.nasa.gov. 2022. OMI/Aura Surface UVB Irradiance and Erythemal Dose Daily L3 Global Gridded 1.0 degree x 1.0 degree V3 (OMUVBd) at GES DISC.", a(href="https://search.earthdata.nasa.gov/search/granules/collection-details?p=C1266136072-GES_DISC&q=uv&tl=1649489823!3!!&lat=-0.28125&zoom=0", "[online]"), style = "font-size:16px;"),
              br(),
              p("6. Search.earthdata.nasa.gov. 2022. MERRA-2 tavg1_2d_flx_Nx: 2d,1-Hourly,Time-Averaged,Single-Level,Assimilation,Surface Flux Diagnostics 0.625 x 0.5 degree V5.12.4 (M2T1NXFLX) at GES DISC.", a(href="https://search.earthdata.nasa.gov/search/granules/collection-details?p=C1276812838-GES_DISC&fsm0=Atmospheric%20Winds&fst0=Atmosphere&lat=-0.28125&zoom=0", "[online]"), style = "font-size:16px;")
      ) # end of 3.1.3 Third tab
      
    ) # end of 3.1 Tab Items
  ) # end of 3 Body
) # end of all
