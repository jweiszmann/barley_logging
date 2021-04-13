#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
library(forecast)

#load data
logdat_merge <- read_csv("logdat_merge.csv")



ui <- fluidPage(
    
    # Application title
    titlePanel("Climate Data 2019"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Select date range to be plotted
            dateRangeInput("date", strong("Date range"), start = "2019-03-29", end = "2019-07-02",
                           min = "2019-03-29", max = "2019-07-09"),
            
            radioButtons("all_or_mean", "mean of selected loggers?",
                         choices = c( "Yes"= "Yes",
                                    "No" = "No"),
                         selected = "No"),
            
            radioButtons("smooth", "add a smoothing function?",
                         choices = c( "Yes"= "Yes",
                                      "No" = "No"),
                         selected = "No"),
            
            # Horizontal line ----
            tags$hr(),
            
            #selection of logger and variable; depending on choice of SoA
            checkboxGroupInput(inputId = "ecotech_logger", label = strong("Ecotech Logger"),
                               choices = unique(logdat_merge[logdat_merge$SoA == "Ecotech",]$logger),
                               selected = unique(logdat_merge[logdat_merge$SoA == "Ecotech",]$logger)[1]),
            
            selectInput(inputId = "ecotech_variable", label = strong("Ecotech Variable"),
                        choices = unique(logdat_merge[logdat_merge$SoA == "Ecotech",]$variable),
                        selected = unique(logdat_merge[logdat_merge$SoA ==  "Ecotech",]$variable[1])),
            helpText("s_temp: soil temperature | pF: soil water retention potential | PAR: Photosynthetically Active Radiation"),
            
            # Horizontal line ----
            tags$hr(),
            
            checkboxGroupInput(inputId = "lascar_logger", label = strong("Lascar Logger"),
                               choices = unique(logdat_merge[logdat_merge$SoA == "Lascar",]$logger),
                               selected = unique(logdat_merge[logdat_merge$SoA == "Lascar",]$logger)[1]),
            
            selectInput(inputId = "lascar_variable", label = strong("Lascar Variable"),
                        choices = unique(logdat_merge[logdat_merge$SoA == "Lascar",]$variable),
                        selected = unique(logdat_merge[logdat_merge$SoA ==  "Lascar",]$variable[1])),
            
            helpText("a_temp: air temperature, a_rh: air relative humidity, dew_point: dew point"),
       
        ),
        # Show the plots----
        mainPanel(
          fluidRow(
            column(12,
                   tabsetPanel(type = "tabs",
                               tabPanel("Data", plotOutput("mainplot_eco")),
                               tabPanel("Time Series Decomposition", plotOutput("decomp_plot_eco"),
                                        helpText("A mean of all selected loggers is taken for decomposition"))),
                   
                   helpText("A, B, C, G, F: -20 cm", "  |  D: -30 cm", "  |  E: -50 cm")),
            
            column(12,
                   tabsetPanel(type = "tabs",
                               tabPanel("Data", plotOutput("mainplot_lascar")),
                               tabPanel("Time Series Decomposition", plotOutput("decomp_plot_lascar"), 
                                        helpText("A mean of all selected loggers is taken for decomposition"))),
                   helpText("L1, L2, L3, L5, L9: in field  | L6: shaded")),
          )
        ))
)

# Define server logic---
server <- function(input, output) {
  logdat_merge <- logdat_merge  %>% 
    mutate(time = as.POSIXct(time))  %>% 
    mutate(time_r = as.POSIXct(trunc(time, units = "hours"))) %>% 
    mutate(daynight = ifelse(hour > 6 & hour <= 18, "day", "night"))
  
  
  
  #filter data and summarize if selected----
  #ecotech
  eco_dat_all <- reactive({
    req(input$ecotech_variable)
    logdat_merge  %>% 
      filter(time > as.POSIXct(input$date[1]), 
             time < as.POSIXct(input$date[2]),
             SoA == "Ecotech",
             logger %in% input$ecotech_logger,
             variable == input$ecotech_variable)
  })
  eco_dat_mean <- reactive({
    req(input$ecotech_variable)
    logdat_merge  %>%
      filter(time > as.POSIXct(input$date[1]),
             time < as.POSIXct(input$date[2]),
             SoA == "Ecotech",
             logger %in% input$ecotech_logger,
             variable == input$ecotech_variable) %>%
      group_by(SoA, variable, datehour)  %>%
      mutate(m_value = mean(m_value))  %>%
      mutate(logger = "mean")
  })
  
  eco_dat_dcmp <- reactive({
    eco_dat_mean() %>%  
      as.data.frame() %>% 
      select(date, hour, m_value)  %>% 
      group_by(date, hour)  %>% 
      summarise_all(mean)  %>% 
      mutate(time = as.POSIXct(paste0(date, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")) %>% 
      as.data.frame() %>% 
      select(m_value) %>% 
      msts(., seasonal.periods = c(24))  %>% decompose()
    })
  
  #Lascar
  lasc_dat_all <- reactive({
    req(input$lascar_variable)
    logdat_merge  %>% 
      filter(time > as.POSIXct(input$date[1]), 
             time < as.POSIXct(input$date[2]),
             SoA == "Lascar",
             logger %in% input$lascar_logger,
             variable == input$lascar_variable)
  })
  lasc_dat_mean <- reactive({
    req(input$lascar_variable)
    logdat_merge  %>%
      filter(time > as.POSIXct(input$date[1]),
             time < as.POSIXct(input$date[2]),
             SoA == "Lascar",
             logger %in% input$lascar_logger,
             variable == input$lascar_variable) %>%
      group_by(SoA, variable, datehour)  %>%
      mutate(m_value = mean(m_value))  %>%
      mutate(logger = "mean")
  })
  
  lasc_dat_dcmp <- reactive({
   lasc_dat_mean() %>%  
      as.data.frame() %>% 
      select(date, hour, m_value)  %>% 
      group_by(date, hour)  %>% 
      summarise_all(mean)  %>% 
      mutate(time = as.POSIXct(paste0(date, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")) %>% 
      as.data.frame() %>% 
      select(m_value) %>% 
      msts(., seasonal.periods = c(24))  %>% decompose()
    
  }) 
  #create main Plots----
  #Ecotech
  output$mainplot_eco <- renderPlot({
    if(input$all_or_mean == "No"){
      x <-eco_dat_all()
    }
    else{
      x <-eco_dat_mean()
    }
    g <- ggplot(x, aes(x=time_r, y=m_value, colour = logger))+
      geom_point() + 
      ylab(input$ecotech_variable)
    if(input$smooth == "No"){ 
      g + ggtitle("Ecotech [soil temperature | soil water potential | PAR]") + xlab("")+theme_light()
    }
    else{
      g + ggtitle("Ecotech [soil temperature | soil water potential | PAR]") + xlab("")+theme_light() + geom_smooth()
    }
  })
  #Ecotech Time Series Decomposition
  output$decomp_plot_eco <- renderPlot({
    eco_dat_dcmp() %>% autoplot() + theme_light() + ggtitle(paste0("Decomposition of additive time series: Ecotech: ", input$ectotech_variable))
  })
  
  #Lascar
  output$mainplot_lascar <- renderPlot({
    if(input$all_or_mean == "No"){
      y <-lasc_dat_all()
    }
    else{
      y <-lasc_dat_mean()
    }
    g <- ggplot(y, aes(x=time_r, y=m_value, colour = logger))+
      geom_point() + 
      ylab(input$lascar_variable)
    
    if(input$smooth == "No"){
      g + ggtitle("Lascar [air temperature | air relative humidity | dewpoint]")+ xlab("")+theme_light() + scale_color_brewer(palette = "Dark2")
    }
    else{
      g + ggtitle("Lascar [air temperature | air relative humidity | dewpoint]")+ xlab("")+theme_light() + geom_smooth() + scale_color_brewer(palette = "Dark2")
    }
  })
  #Lascar Time Series Decomposition
  output$decomp_plot_lascar <- renderPlot({
    lasc_dat_dcmp() %>% autoplot() + theme_light() + ggtitle(paste0("Decomposition of additive time series: Lascar: ", input$lascar_variable))
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
