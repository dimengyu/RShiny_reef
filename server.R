shinyServer(function(input, output) {
  # prepare filename and dataset
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

# ------------------------------------------------------>  Instruction page
  showModal(
    modalDialog(
      title = tags$h2("Here is the instruction!"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Continue"),

      tags$ul(
        tags$li(
          #### tag1
          tags$h3("Visualisation Page:"),
          tags$p("Select the coral bleaching factor you want to analyze in 'Factor' and the corresponding year in 'Year'. Page also provides the option to set the coral bleaching threshold and set the customization options for the world map color, allowing users to freely choose the result they want to present.", style = "font-size:16px;text-align:justify;"),
          tags$p("The world map presented on the page can be rendered in static or interactive form.", style = "font-size:16px;text-align:justify;"),
          tags$p("If you move the mouse to the coral reef point in the interactive world map, the page will show the interactive effect: including the coordinates of the coral reef, the bleaching condition, name, country, ocean. If you move the mouse to another location, the coordinates of that location and the corresponding value of the selected environmental factor will be displayed.", style = "font-size:16px;text-align:justify;"),
          tags$p("The leftmost table at the bottom of the page is used to present more accurate information about the selected element and the value of the environmental factor; the middle violin plot shows the distribution of selected factor by bleaching status. The rightmost image combines the middle images and shows the relationship between bleaching and the selected factor in the form of a scatter plot.", style = "font-size:16px;text-align:justify;"),
          ),
        tags$li(
          #### tag2
          tags$h3("Model Page:"),
          tags$p("Corplot shows the correlation coefficient between multiple environmental factors. The CV accuracy and Density of the models are also shown, and the results of multiple models are displayed.", style = "font-size:16px;text-align:justify;"),
        ),
        tags$li(
          #### tag3
          tags$h3("Reference Page:"),
          tags$p("The data sources for the images and the literature references used for the modeling analysis in the model page.", style = "font-size:16px;text-align:justify;"),
        )
      ),
      tags$p("Have fun and enjoy yourself!", style = "font-size:16px; text-align:right; font-weight:bold;"),
      tags$p(HTML("&mdash; Team Reef P1"), style = "font-size:16px; text-align:right; font-weight:bold;")
    )
  )
  
  
# ------------------------------------------------------>  Maps and distribution
  # prepare reef data for distribution plot
  reef <- read_csv("data/Reef_Check_with_cortad_variables_with_annual_rate_of_SST_change_SSH2.csv",
                   show_col_types = F)  
  
  reef_bleach <- reef %>%
    dplyr::select(Reef.ID, Year, Depth, Average_bleaching) %>%
    group_by(Reef.ID, Year, Depth) %>%
    summarise(avg_bleaching = mean(Average_bleaching, na.rm = T)) %>%
    group_by(Reef.ID, Year) %>%
    slice_min(Depth, n = 1)
  
  factor_yr <- reactive({
    # extract year and factor from input
    if (input$factor == "CO2") {
      year <- input$time1
      factor <- "co2"
    } else if (input$factor == "PRECIPITATION") {
      year <- input$time2
      factor <- "Precipitation"
    } else if (input$factor == "SSH") {
      year <- input$time3
      factor <- "Sea surface height"
    } else if (input$factor == "SALINITY") {
      year <- input$time4
      factor <- "Salinity"
    } else if (input$factor == "TEMPERATURE") {
      year <- input$time5
      factor <- "Temperature"
    } else if (input$factor == "UV") {
      year <- input$time6
      factor <- "UVindex"
    }
      
    # return list of items
    list(year = year, factor = factor, cutoff = input$cutoff)
    ## use factor_yr()[["year"]] later
  })
  
  #### data table
  options(DT.options = list(pageLength = 5))
  rowCallback <- c(
    "function(row, data){",
    "  for(var i=0; i<data.length; i++){",
    "    if(data[i] === null){",
    "      $('td:eq('+i+')', row).html('NA')",
    "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
    "    }",
    "  }",
    "}"  
  )
  
  output$data_table <- renderDT(
    datatable(reef %>%
                mutate(bleach = case_when(Average_bleaching > factor_yr()[["cutoff"]] ~ "Bleached",
                                          Average_bleaching <= factor_yr()[["cutoff"]] ~ "Not Bleached")) %>%
                filter(Year == factor_yr()[["year"]]) %>%
                dplyr::select(Reef.ID, Reef.Name, bleach, Average_bleaching, factor_yr()[["factor"]]) %>% 
                arrange(factor_yr()[["factor"]]),
              colnames=c("Reef ID", "Name", "Bleach Status", "Average Bleaching", factor_yr()[["factor"]]),
              # caption = htmltools::tags$caption(
              #   style = 'font-size:16px; color:black;',
              #   htmltools::strong(paste0("Factor = ", factor_yr()[["factor"]], "; Year = ", factor_yr()[["year"]]))
              # ),
              filter = 'top', 
              options = list(pageLength = 5, 
                             autoWidth = T, 
                             rowCallback = JS(rowCallback)), 
              rownames = F)
    )
  
  #### scatterplot with marginal distribution
  output$scatter_marginal <- renderPlot({
    p <- reef %>% 
      mutate(bleach = case_when(Average_bleaching > input$cutoff ~ "Bleached",
                                Average_bleaching <= input$cutoff ~ "Not Bleached")) %>%
      filter(Year == factor_yr()[["year"]]) %>%
      ggplot(aes(x=get(factor_yr()[["factor"]]),
                 y=log(Average_bleaching), 
                 color = bleach)) +
      geom_point() + 
      labs(x = factor_yr()[["factor"]], y = "log(Average Bleaching)", color = 'bleach') +
      theme_bw() + 
      theme(legend.position="none")
    ggMarginal(p, type="densigram")
  })  
  
  #### violin plot: bleaching vs. selected factor
  output$relationship_plot <- renderPlot({
    reef %>%
      filter(Year == factor_yr()[["year"]]) %>%
      dplyr::select(Reef.ID, Average_bleaching, factor_yr()[["factor"]]) %>%
      distinct() %>%
      mutate(bleach = case_when(Average_bleaching > input$cutoff ~ "Bleached",
                                Average_bleaching <= input$cutoff ~ "Not Bleached")) %>%
      ggplot(aes(x = bleach, y = get(factor_yr()[["factor"]]), fill = bleach)) +
      geom_violin() +
      labs(y = factor_yr()[["factor"]], x = "")+ 
      theme_bw() + 
      theme(legend.position = "none") 
  })
  
  autoInvalidate <- reactiveTimer(29000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  #### Map  
  pp <- eventReactive(input$create, {
    view <- input$view == 'Interactive'
    # prepare reef data
    reef_bleach <- reef %>%
      dplyr::select(Reef.ID, Reef.Name, Year, Depth, Average_bleaching) %>%
      group_by(Reef.ID, Reef.Name, Year, Depth) %>%
      summarise(avg_bleaching = mean(Average_bleaching, na.rm = T)) %>%
      group_by(Reef.ID, Reef.Name, Year) %>%
      slice_min(Depth, n = 1)
    reef2 <- reef %>%
      dplyr::select(Reef.ID, Reef.Name, Ocean, Country, Longitude.Degrees, Latitude.Degrees) %>%
      distinct() %>%
      left_join(reef_bleach, by = c("Reef.ID", "Reef.Name"))
    
    # plot map
    if (input$factor == "SSH") {
      sub_fn <- substr(fn_ssh, 38, 41)
      fn <- fn_ssh[input$time3 == sub_fn]
      
      rf <- reef2 %>%
        mutate(
          bleach = case_when(
            avg_bleaching > input$cutoff ~ "Bleached",
            avg_bleaching <= input$cutoff ~ "Not Bleached"
          ),
          Year = as.character(Year)
        ) %>%
        filter(Year == input$time3)
      
      return(plot_ssh(
        fn,
        reef_dt = rf,
        view = view,
        lc = input$low_col,
        uc = input$high_col
      ))
    } else if (input$factor == "PRECIPITATION") {
      sub_fn <- substr(fn_precipitation, 40, 43)
      fn <- fn_precipitation[input$time2 == sub_fn]
      rf <- reef2 %>%
        mutate(
          bleach = case_when(
            avg_bleaching > input$cutoff ~ "Bleached",
            avg_bleaching <= input$cutoff ~ "Not Bleached"
          ),
          Year = as.character(Year)
        ) %>%
        filter(Year == input$time2)
      return(
        plot_precipitation(
          fn,
          reef_dt = rf,
          view = view,
          lc = input$low_col,
          uc = input$high_col
        )
      )
    } else if (input$factor == "TEMPERATURE") {
      sub_fn <- substr(fn_st, 63, 66)
      fn <- fn_st[input$time5 == sub_fn]
      rf <- reef2 %>%
        mutate(
          bleach = case_when(
            avg_bleaching > input$cutoff ~ "Bleached",
            avg_bleaching <= input$cutoff ~ "Not Bleached"
          ),
          Year = as.character(Year)
        ) %>%
        filter(Year == input$time5)
      return(
        plot_st(
          fn,
          reef_dt = rf,
          var = "THETA",
          view = view,
          lc = input$low_col,
          uc = input$high_col
        )
      )
    } else if (input$factor == "SALINITY") {
      sub_fn <- substr(fn_st, 63, 66)
      fn <- fn_st[input$time4 == sub_fn]
      rf <- reef2 %>%
        mutate(
          bleach = case_when(
            avg_bleaching > input$cutoff ~ "Bleached",
            avg_bleaching <= input$cutoff ~ "Not Bleached"
          ),
          Year = as.character(Year)
        ) %>%
        filter(Year == input$time4)
      return(
        plot_st(
          fn,
          reef_dt = rf,
          var = "SALT",
          view = view,
          lc = input$low_col,
          uc = input$high_col
        )
      )
    } else if (input$factor == "UV") {
      sub_fn <- substr(fn_uv, 28, 31)
      fn <- fn_uv[input$time6 == sub_fn]
      rf <- reef2 %>%
        mutate(
          bleach = case_when(
            avg_bleaching > input$cutoff ~ "Bleached",
            avg_bleaching <= input$cutoff ~ "Not Bleached"
          ),
          Year = as.character(Year)
        ) %>%
        filter(Year == input$time6)
      return(plot_uv(
        fn,
        reef_dt = rf,
        view = view,
        lc = input$low_col,
        uc = input$high_col
      ))
    }
  })
  
  output$plotlyMap <- renderPlotly({  
    autoInvalidate <- reactiveTimer(29000)
    observe({
      autoInvalidate()
      cat(".")
      })
    pp()
    })
  output$staticMap <- renderPlot({pp()})  
  
# ---------------------------------------------------------------->  Model plots
  reef_cor <- read_csv("data/final_clean_reef.csv", show_col_types = F) %>% 
    dplyr::select(Average_bleaching, Depth, ClimSST, Temperature_Mean, 
                  Windspeed,SSTA, SSTA_DHW, TSA_Mean, TSA_DHW, Diversity, 
                  rate_of_SST_change, UVindex, Sea.surface.height, 
                  Salinity, Precipitation, co2)
  
  #### corrplot
  output$corr_plot <- renderPlot({
    ggcorrplot(cor(reef_cor), 
               tl.cex = 8, 
               tl.srt = 90) + 
      theme(legend.position = "top", 
            legend.title = element_blank())
  })
  
  #### PCA
  output$pca_plot <- renderPlot({
    reef.pca <- prcomp(reef_cor, center=TRUE,scale=TRUE)
    pcaCharts(reef.pca)
  })
  
  reef_ml<- read_csv("data/final_clean_reef.csv", show_col_types = F) %>% 
    dplyr::select(-Reef.ID, -Reef.Name, -Ocean,-Country, -`State.Province.Island`, -City.Town, -Year, -Date, -Organism.Code, -Errors., -Region, -Longitude.Degrees, -Latitude.Degrees, -Average_bleaching, -S1, -S2, -S3, -S4, -Temperature_Kelvin, -SSTA_Mean) 
  
  combined_df <- reef_ml %>%
    mutate(Coral_bleaching = as.factor(Coral_bleaching))
  
  #### CV accuracy
  output$cv_plot <- renderImage({
    list(src = "data/cv_acc_plot.png",
         align = "center", 
         style = "display: block; width: 95%; margin-left: auto; margin-right: auto;")
    }, deleteFile = F
    )
  #### Stability
  output$stability_plot <- renderImage({
    list(src = "data/Stability.png",
         align = "center", 
         style = "display: block; height: 95%; margin-left: auto; margin-right: auto;")
  }, deleteFile = F
  )

})
