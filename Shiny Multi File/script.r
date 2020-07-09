#Non-shiny multiple file test
#Libraries:

library(dplyr)
library(anytime)
library(prophet)
library(lubridate)
library(ggplot2)

#Prep prophet

m <- prophet(yearly.seasonality = TRUE, interval.width = 0.85)
m <- add_country_holidays(m,'England')

#ReadCSV

dat <- read.csv("Data/ExportComb19-08-27.csv")
dat$ds <- anytime(dat$ds)

#Split into multiple dataframes, and fit prophet model
x <- NULL
for (i in 2:ncol(dat)) {
  x <- append(x,colnames(dat[i]))
}

for (i in 1:length(x)) {
  assign(paste("df", x[i], sep = ""), data.frame(dat[1], dat[i+1]))
  d <-get(paste("df",x[i],sep=""))
  names(d) <- c("ds","y")
  assign(paste("df",x[i],sep=""),d)
  d<- NULL
  assign(paste("m",x[i],sep=""), fit.prophet(m, get(paste("df", x[i], sep = ""))))
}

#for (i in 1:length(x)) {
#  assign(paste("m",x[i],sep=""), fit.prophet(m, get(paste("df", x[i], sep = ""))))
#}

#Needs fixing

future <- make_future_dataframe(get(paste("m",x[i],sep="")),
                                periods = 168 + if (24-hour(tail(dat$ds,1)) == 24) {0} else {24-hour(tail(dat$ds,1))},
                                freq = 60*60, 
                                include_history = FALSE)

 

#future <- make_future_dataframe(mAmb, periods = 168 + (23 - lubridate::hour(tail(dat$ds,1))), freq = 60 * 60, include_history = FALSE)

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

df <- df[-c(5)]



ggplot(data = df, aes(x=ds,y=df[,2]))+
  geom_ribbon(aes(ymin = yhat_lowerAmb, ymax = yhat_upperAmb), fill = "blue", alpha=0.3)+
  geom_line()+
  xlab("Date")+
  ylab("Attendances")+
  scale_x_datetime(date_breaks = "12 hours",expand = c(0,0))+
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data = df, aes(x=ds,y=yhatWal.In))+
  geom_ribbon(aes(ymin = yhat_lowerWal.In, ymax = yhat_upperWal.In), fill = "blue", alpha=0.3)+
  geom_line()+
  xlab("Date")+
  ylab("Attendances")+
  scale_x_datetime(date_breaks = "12 hours",expand = c(0,0))+
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))
