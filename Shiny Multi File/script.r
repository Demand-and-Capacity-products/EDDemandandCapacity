#Non-shiny multiple file test
#Libraries:

library(dplyr)
library(anytime)
library(prophet)
library(lubridate)
library(ggplot2)
library(tidyr)

#Prep prophet

m <- prophet(yearly.seasonality = TRUE, monthly.seasonality = TRUE, daily.seasonality = TRUE, interval.width = 0.85)
m <- add_country_holidays(m,'England')

#ReadCSV

dat <- read.csv("Data/Majors1.csv")
dat <- dat %>% 
  rename("ds" = "Date.Time", "y" = "Majors")
dat$ds <- anytime(dat$ds, tz = "GMT")
dat <- dat %>% 
  filter(ds < "2020-01-01")

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

#Currently bugged and logged on GH
#Make one future dataframe for all streams
future <- make_future_dataframe(get(paste("m",x[i],sep="")),
                                periods = 168 + if (24-hour(tail(dat$ds,1)) == 24) {0} else {24-hour(tail(dat$ds,1))},
                                freq = 60*60, 
                                include_history = FALSE)
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

#cut to one week
df <- df[-c(5)] %>% 
  head(-1) %>% 
  tail(-1)

ggplot(data = df, aes(x=ds,y=df[,2]))+
  geom_ribbon(aes(ymin = yhat_lowery, ymax = yhat_uppery), fill = "blue", alpha=0.3)+
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







#Testing
full_data <- full_join(dat,df)

cut_data <- as_tibble(full_data) %>% 
  filter(ds > "2019-01-01") %>% 
  replace(is.na(.), 0)

cut_data %>% 
  ggplot(aes(x=ds,y=y))+
  geom_ribbon(aes(ymin = yhat_lowery, ymax = yhat_uppery), fill = "blue", alpha=0.3)+
  geom_line()+
  xlab("Date")+
  ylab("Attendances")+
  scale_x_datetime(date_breaks = "1 month",expand = c(0,0))+
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(angle = 90, hjust = 1))
