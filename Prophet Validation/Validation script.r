#Relatively generic script to validate ED forecasting with Prophet.
#Subject to change to match functions of main ED forecasting app.
#Created by Paul Bullard.

#load packages, and install if required.

#Prophet - Automatic Forecasting Procedure
if(!require("prophet")) {
  install.packages("prophet")
  library(prophet)
}

#ggplot2 - Create elegant data visualisations using the grammar of graphics
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

#anytime - anything to 'POSIXct' or 'Date' converter
if(!require("anytime")) {
  install.packages("anytime")
  library(anytime)
}

#dplyr - a grammar of data maniupulation
if(!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}

#lubridate - make dealing with dates a little easier
if(!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}

#Read in the data.
#The data needs to be a csv with two columns.
#'ds' - a date/time column in the format yyyy-mm-dd hh:mm
#'y' - attendance counts per hour
df <- read.csv(filename<-file.choose())

#Extract the filename for later use
filename <- basename(filename)
filename <- tools::file_path_sans_ext(filename)

#Manipulate the data to correct date/time format for R. Cut in to two data sets - train and test.
#Test set to one week of data (168 hours)
df$ds <- anytime(df$ds)
dftrain <- head(df, -168)
dftest <- tail(df, 168)

#Initiate prophet, and start to build the model parameters.
m <- prophet()
m <- add_country_holidays(m, 'England')
m <- prophet(dftrain, 
             seasonality.mode = 'multiplicative', 
             weekly.seasonality = TRUE, 
             daily.seasonality = TRUE, 
             yearly.seasonality = TRUE, 
             interval.width = 0.85)

#Create a future data frame to predict in to
future <- make_future_dataframe(m, periods = 168, freq = 'hour')

#OPTIONAL
#If the department is closed at particular times, uncomment the below section.
#Set the start and end hour after the greater than and less than signs

#future_short <- future %>% 
#  filter(as.numeric(format(ds, "%H")) >= 8) %>%
#  filter(as.numeric(format(ds, "%H")) < 20)

#If using the department closed function, change the second variable below to 'future_short'
fcst <- predict(m, future)

#Plot and save trend components to working directory
png(paste(filename," components plot"),width = 600, height = 600)
prophet_plot_components(m, fcst)
dev.off()

#Cutting down the outputs to remove additional columns
fcst <- fcst[c('ds','yhat','yhat_lower','yhat_upper')]

#Combine the future and the forecast data 
#(although only needed if future_short is used, run to set variable names correctly)
#Also cuts down the dataset to the most recent week
combined <- left_join(future, fcst)
combined[is.na(combined)] <- 0
combined <- tail(combined, 168)

#Combine the forecasted data with the test data, for comparison.
combined <- left_join(combined, dftest)
combined[is.na(combined)] <- 0
short <- tail(combined, 168)

#Plot and save the comparison outputs
png(paste(filename," comparison plot"),width = 600, height = 600)

ggplot(data = short, aes(x=ds,y=yhat))+
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), fill = "blue", alpha=0.3)+
  geom_line()+
  geom_point(data= short, aes(x=ds, y=y), color = "red")+
  ggtitle(filename)

dev.off()

  #Save the outputs
write.csv(short,paste(filename, 'trainforecast.csv'))
  
