#Small Shiny app to predict ED demand using FB Prophet (https://facebook.github.io/prophet/)
#Requires data processed via NHS Demand and Capacity Team ED Model for correct formatting
#Outputs .CSV file with predicted attendances for the next week (by hour).

#To do:
#1. Improve UI of app
#2. Ensure optimisation of code
#3. Allow user input of some key parameters via a helper header row
#4. Create 'loading...' notification
#5. Multiple file upload and processing

#Required libraries:
library(shiny)
library(prophet)
library(anytime)
library(ggplot2)

#Define UI for data upload

ui <- fluidPage(
  
  titlePanel("NHS Demand and Capacity Implementation of Prophet"),
  
  sidebarLayout(
  
  #Sidebar panel for user interaction
  sidebarPanel(h3("Data Upload"),
               
               fileInput(inputId = 'file1', label = "Upload dataset here:",multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               downloadButton("downloadData", "Download")
               ),
             

    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("forecast")
      
    )
    
  ))


#Define server logic to process uploaded file
server <- function(input, output) {
  
  #Process data through prophet
  fcst_short <- eventReactive(input$file1, {
    
  df <- read.csv(input$file1$datapath,
                 header = TRUE,
                 sep = ","
  )
  
  df <- tail(df,-8784)
  
  df$ds <- anytime(df$ds)
  
  m <- prophet(yearly.seasonality = TRUE)
  m <- add_country_holidays(m,'England')
  m <- fit.prophet(m, df)
  
  future <- make_future_dataframe(m, periods = 168, freq = 60 * 60)
  fcst <- predict(m, future)
  
  fcst_short <- tail(fcst, 168)
  
  return(fcst_short)
  })
  
  #Process outputs to help chart display
  fcst_m <- reactive({
    fcst_m <- max(fcst_short()$yhat_upper)
    return(fcst_m)
  })
  
  #Present outputs in line and area chart
  output$forecast <- renderPlot({
  
    req(input$file1)
    
  ggplot(data = fcst_short(), aes(x=ds,y=yhat))+
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3)+
    geom_line()+
    coord_cartesian(ylim = c(0,fcst_m()))

  })
  
  #Define download parameters for outputs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv",sep = "")
    },
    content = function(file) {
      write.csv(fcst_short(), file)
    }
    
  )
  
}
# Run the app ----
shinyApp(ui, server)
