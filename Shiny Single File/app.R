#Small Shiny app to predict ED demand using FB Prophet (https://facebook.github.io/prophet/)
#Requires data processed via NHS Demand and Capacity Team ED Model for correct formatting
#Outputs .CSV file with predicted attendances for the next week (by hour).

#To do:
#1. Improve UI of app - 04/10
#2. Ensure optimisation of code - 06/08
#3. Allow user input of some key parameters via a helper header row
#4. Create 'loading...' notification - completed 01/08
#5. Multiple file upload and processing

#Required libraries:
library(shiny)
library(prophet)
library(anytime)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(shinyjs)

#Define UI for data upload

ui <- fluidPage(
  useShinyjs(),
  titlePanel(span(style = "color: rgb(0,94,184)","NHS Demand and Capacity Forecasting Tool"), windowTitle = "NHS Demand and Capacity Forecasting Tool"),
  
  sidebarLayout(
    
    #Sidebar panel for user interaction
    sidebarPanel(h2("Data Upload"),
                 
                 fileInput(inputId = 'file1', label = "Upload model export here:",multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="border-color: black;"),
                 h6("Set the opening time for the department:"),
                 sliderInput("slider", label = "Department Open (24hr) :", min = 0, max = 24, value = c(0,24)),
                 tags$hr(style="border-color: black;"),
                 h6('Use the "Predict" button below to begin the forecast.'),
                 actionButton("go","Predict"),
                 tags$hr(style="border-color: black;"),
                 downloadButton("downloadData", "Download"),
                 br(),
                 br(),
                 em("Please do not click the download button until the graph on the right has been generated")
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      img(src = 'NHS.png', align = "right", height="15%", width="15%"),
      
      withSpinner(plotOutput("forecast"))
      
    )
    
  ))


#Define server logic to process uploaded file
server <- function(input, output) {

  #Process data through prophet
  fcst_short <- eventReactive(input$go, {
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ","
    )
    
    rows <- nrow(df)
    mrows <- min(8784,rows)
    
    df <- tail(df,mrows)
    
    df$ds <- anytime(df$ds)
    
    m <- prophet(yearly.seasonality = TRUE, interval.width = 0.85)
    m <- add_country_holidays(m,'England')
    m <- fit.prophet(m, df)
    
    future_short <- make_future_dataframe(m, periods = 169 + (23 - lubridate::hour(tail(df$ds,1))), freq = 60 * 60)
    future_full <- future_short
    future_short <- future_short %>% 
      filter(as.numeric(format(ds, "%H")) >= input$slider[1]) %>%
      filter(as.numeric(format(ds, "%H")) < input$slider[2])
    fcst <- predict(m, future_short)
    
    combined <- left_join(future_full, fcst)
    combined[is.na(combined)] <- 0
    fcst_short <- tail(combined, 168)
    fcst_short <- fcst_short[c('ds','yhat','yhat_lower','yhat_upper')]
    
    return(fcst_short)
  })
  
  #Process data to match capacity timescales
  fcst_cut <- reactive({
    fcst_cut <- fcst_short() %>% mutate(yhat_lower = replace(yhat_lower, yhat_lower < 0, 0), yhat_upper = replace(yhat_upper, yhat_upper < 0, 0), yhat = replace(yhat, yhat < 0, 0)) 
    return(fcst_cut)
  })
  
  #Process outputs to help chart display
  fcst_m <- reactive({
    fcst_m <- max(fcst_short()$yhat_upper)
    return(fcst_m)
  })
  
  #Process filename for chart

  file_name <- reactive({
    file_name <- substr(input$file1$name,7,nchar(input$file1$name)-14)
  })
  
  #Present outputs in line and area chart
  output$forecast <- renderPlot({
    
    req(input$file1)

    ggplot(data = fcst_cut(), aes(x=ds,y=yhat))+
      geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3)+
      geom_line()+
      ggtitle(file_name())+
      xlab("Date")+
      ylab("Attendances")+
      scale_x_datetime(date_breaks = "12 hours",expand = c(0,0))+
      theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
      coord_cartesian(ylim = c(0,fcst_m()))
    
  })
  
  #Define download parameters for outputs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Import",file_name(), Sys.Date(), ".csv",sep = "")
    },
    content = function(file) {
      write.csv(fcst_short(), file)
    }
    
  )
  
}
# Run the app ----
shinyApp(ui, server)
