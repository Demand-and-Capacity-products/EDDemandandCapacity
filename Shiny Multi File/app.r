#Small Shiny app to predict ED demand using FB Prophet (https://facebook.github.io/prophet/)
#Requires data processed via NHS Demand and Capacity Team ED Model for correct formatting
#Outputs .CSV file with predicted attendances for the next week (by hour).

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
    sidebarPanel(h4("Step 1: Data Upload"),
                 
                 fileInput(inputId = 'file1', label = "Upload model export here:",multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 tags$hr(style="border-color: black;"),
                 h4("Step 2: Opening Times"),
                 h6("Set the opening times for the department using the two sliders. For example, if the department is open from 8am to 8pm, set the left slider to 8 and the right slider to 20."),
                 sliderInput("slider", label = "Department Open (24hr) :", min = 0, max = 24, value = c(0,24)),
                 tags$hr(style="border-color: black;"),
                 h4("Step 3: Prediction"),
                 h6('Use the "Predict" button below to begin the forecast.'),
                 actionButton("go","Predict"),
                 tags$hr(style="border-color: black;"),
                 h4("Step 4: Download"),
                 h6("Download the completed forecast to be brought back in to the model."),
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
  
  #Prep Prophet
  m <- prophet(yearly.seasonality = TRUE, monthly.seasonality = TRUE, daily.seasonality = TRUE, interval.width = 0.85)
  m <- add_country_holidays(m,'England')
  
  
  #Process data through prophet
  fcst_short <- eventReactive(input$go, {
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ","
    )
    
    df$ds <- anytime(df$ds, tz = "GMT")
    
    rows <- nrow(df)
    ##############
    
    x <- NULL
    for (i in 2:ncol(df)) {
      x <- append(x,colnames(df[i]))
    }
    
    for (i in 1:length(x)) {
      assign(paste("df", x[i], sep = ""), data.frame(df[1], df[i+1]))
      d <-get(paste("df",x[i],sep=""))
      names(d) <- c("ds","y")
      assign(paste("df",x[i],sep=""),d)
      d<- NULL
      assign(paste("m",x[i],sep=""), fit.prophet(m, get(paste("df", x[i], sep = ""))))
    }
    
    future <- make_future_dataframe(get(paste("m",x[i],sep="")),
                                    periods = 168 + if (24-hour(tail(df$ds,1)) == 24) {0} else {24-hour(tail(df$ds,1))},
                                    freq = 60*60, 
                                    include_history = FALSE)
    future_base <- future
    
    future <- future %>% 
      filter(as.numeric(format(ds, "%H")) >= input$slider[1]) %>%
      filter(as.numeric(format(ds, "%H")) < input$slider[2])
    
    df <- data.frame()
    
    for (i in 1:length(x)) {
      assign(paste("forecast",x[i],sep=""),predict(get(paste("m",x[i],sep="")), future))
      d <-get(paste("forecast",x[i],sep=""))
      d <- d %>% 
        select("ds", "yhat", "yhat_lower", "yhat_upper")
      names(d) <- c("ds",paste("yhat",x[i],sep = ""),paste("yhat_lower",x[i],sep = ""),paste("yhat_upper",x[i],sep = ""))
      assign(paste("forecast",x[i],sep=""),d)
      if(is_empty(df)) {df <- get(paste("forecast",x[i],sep=""))} else {df <- cbind(df,get(paste("forecast",x[i],sep="")))}
    }
    
    combined <- data.frame()
    
    for (i in 1:length(x)) {
    if(is_empty(combined)) {combined <- left_join(future_base, get(paste("forecast",x[i],sep="")))} else {(combined <- left_join(combined, get(paste("forecast",x[i],sep=""))))}}
    combined[is.na(combined)] <- 0
    fcst_short <- tail(combined, 168)
    #fcst_short <- fcst_short[c('ds','yhat','yhat_lower','yhat_upper')]
    
    ##############
    # df$ds <- anytime(df$ds)
    # 
    # m <- prophet(yearly.seasonality = TRUE, interval.width = 0.85)
    # m <- add_country_holidays(m,'England')
    # m <- fit.prophet(m, df)
    # 
    # future_short <- make_future_dataframe(m, periods = 168 + (23 - lubridate::hour(tail(df$ds,1))), freq = 60 * 60, include_history = FALSE)
    # future_full <- future_short
    # future_short <- future_short %>% 
    #   filter(as.numeric(format(ds, "%H")) >= input$slider[1]) %>%
    #   filter(as.numeric(format(ds, "%H")) < input$slider[2])
    # fcst <- predict(m, future_short)
    # 
    # combined <- left_join(future_full, fcst)
    # combined[is.na(combined)] <- 0
    # fcst_short <- tail(combined, 168)
    # fcst_short <- fcst_short[c('ds','yhat','yhat_lower','yhat_upper')]
    ##############
    
    return(fcst_short)
  })
  
  #Process data to match capacity timescales
  fcst_cut <- reactive({
    fcst_cut <- fcst_short() %>% 
      for (i in 1:length(x)) {
       mutate(yhat_lower = replace(yhat_lower,yhat_lower < 0, 0)) 
              #paste(yhat_upper,x[i], sep = "") = replace(paste(yhat_upper,x[i], sep = ""), paste(yhat_upper,x[i],sep = "") < 0, 0), 
              #paste(yhat,x[i], sep = "") = replace(paste(yhat,x[i], sep = ""), paste(yhat,x[i], sep = "") < 0, 0)) 
    }
    return(fcst_cut)
  })
  
  #Process outputs to help chart display
  fcst_m <- reactive({
    fcst_m <- max(fcst_short()$yhat_upper)
    return(fcst_m)
  })
  
  #Process filename for chart
  
  file_name <- eventReactive(input$go, {
    file_name <- substr(input$file1$name,7,nchar(input$file1$name)-14)
    return(file_name)
  })
  
  #Present outputs in line and area chart
  output$forecast <- renderPlot({
    
    input$file1
    if(file_name() == substr(input$file1$name,7,nchar(input$file1$name)-14))
    {ggplot(data = fcst_cut(), aes(x=ds,y=yhat))+
        geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3)+
        geom_line()+
        ggtitle(file_name())+
        xlab("Date")+
        ylab("Attendances")+
        scale_x_datetime(date_breaks = "12 hours",expand = c(0,0))+
        theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))+
        coord_cartesian(ylim = c(0,fcst_m()))}
    
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
