library(shiny)
library(prophet)
library(anytime)
library(ggplot2)

# Define UI for data upload app ----

ui <- fluidPage(
  
  titlePanel("NHS Demand and Capacity Implementation of Prophet"), #main page title
  
  sidebarLayout( #configure as split page
  
  sidebarPanel(h3("Data Upload"), #sidebar main title
               fileInput(inputId = 'file1', label = "Upload dataset here:",multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               downloadButton("downloadData", "Download")
               ),#fileinput widget
             

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput("test")
      
    )
    
  ))


# Define server logic to read selected file ----
server <- function(input, output) {
  
  fcst_short <- eventReactive(input$file1, {
    
  df <- read.csv(input$file1$datapath,
                 header = TRUE,
                 sep = ","
  )
  
  df <- tail(df,-8000)
  
  df$ds <- anytime(df$ds)
  
  m <- prophet()
  m <- add_country_holidays(m,'England')
  m <- fit.prophet(m, df)
  
  future <- make_future_dataframe(m, periods = 168, freq = 60 * 60)
  fcst <- predict(m, future)
  
  fcst_short <- tail(fcst, 168)
  
  
  
  return(fcst_short)
  })
  
  fcst_m <- reactive({
    fcst_m <- max(fcst_short()$yhat_upper)
    return(fcst_m)
  })
  
  output$test <- renderPlot({
  
    req(input$file1)
    
  ggplot(data = fcst_short(), aes(x=ds,y=yhat))+
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3)+
    geom_line()+
    coord_cartesian(ylim = c(0,fcst_m()))

  })
  
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