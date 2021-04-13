library(readr)
library(rlist)
library(tidyverse)
library(lubridate)
library(plotly)
library(tidyquant)
##----load data (Ecotech)----
#set working directory (Ecotech):
setwd("E:/Uni/Ucloud_sync/Barley_Climate_Logging/data/Ecotech")

#find all csv files in it
file.list <- list.files(pattern='*.csv')

#read files in list
df.list <- lapply(file.list, function(i){
  x = read_csv(i, skip=1)
  x$logger <- strsplit(i, ".csv")
  x
})

#create dataframe from list; format dates as POSIXct
logdat<- list.rbind(df.list)
colnames(logdat)<-c("no", "time", "s_temp", "pF", "PAR", "hk_bat", "hk_temp", "hk_rh", "logger")

logdat <- logdat  %>% 
  mutate(time = as.POSIXct(time, format = "%d.%m.%Y %H:%M:%S"))  %>% 
  mutate(time = as.POSIXct(time, format = "%d.%m.%Y %H:%M:%S"))  %>% 
  mutate(date = as.Date(time, tz=""))  %>% 
  mutate(hour = hour(time))  %>% 
  mutate(logger = as.character(logger))  %>% 
  filter(time  > as.POSIXct("01.01.2018", format = "%d.%m.%Y"))
  
logdat <- logdat[!is.na(logdat$no),]
logdat$datehour <- interaction(logdat$date, logdat$hour)


##load data (Lascar)------
#set working directory (Lascar)
setwd("E:/Uni/Ucloud_sync/Barley_Climate_Logging/data/Lascar")

#find all txt files in it
file.list_lascar <- list.files(pattern='*.txt')

#read files to list
df.list_lascar <- lapply(file.list_lascar, function(i){
  x = read_csv(i)[,c(-1)]
  x$logger <- strsplit(i, "_")[[1]][1]
  x
})

for (U in 1:length(df.list_lascar)){
  df.list_lascar[[U]]$`Serial Number`<- df.list_lascar[[U]]$`Serial Number`[1] 
}

#create dataframe from list
logdat_lascar<- list.rbind(df.list_lascar)
colnames(logdat_lascar)<-c("time", "a_temp", "a_rh", "dew_point", "serial_no", "logger")


##----format dataframes and create merge dataframe for export
logdat_m <- pivot_longer(logdat, cols = c("s_temp", "pF", "PAR"), names_to = "variable")  %>% 
  mutate(time = as.POSIXct(time)) %>% 
  mutate(SoA = "Ecotech") %>% 
  group_by(date, hour, variable, logger)  %>% 
  mutate(m_value = mean(value))

logdat_lascar_m <-logdat_lascar  %>% 
  mutate(time = as.POSIXct(time)) %>% 
  pivot_longer(cols = c("a_temp", "a_rh", "dew_point"), names_to = "variable")  %>%
  mutate(SoA = "Lascar") %>% 
  mutate(date = as.Date(time, tz="")) %>% 
  mutate(hour = hour(time)) %>% 
  group_by(date, hour, variable, logger)  %>% 
  mutate(m_value = mean(value))  %>% 
  mutate(datehour =interaction(date, hour))

logdat_merge <- rbind(logdat_m[,c(2,6:13)], logdat_lascar_m[,c(1,3,7,8,10,4,5,6,9)]) %>% na.omit()

##----plots---
logdat  %>% 
  filter(logger %in% c("C", "D", "E"))  %>% 
  ggplot(., aes(x=time, y=s_temp, colour = logger)) + 
  geom_line()


ggplot(subset(logdat, logger %in% c("C", "D", "E")), aes(x=time, y=s_temp, colour = logger))+geom_line()

ggplot(logdat, aes(x=time, y=pF, colour = logger))+geom_line()
ggplot(logdat, aes(x=time, y=s_temp, colour = logger))+geom_line()


ggplotly(ggplot(logdat, aes(x=time, y=pF, colour = logger))+geom_line()+ coord_x_datetime(xlim=c(today()-weeks(1),today())))
ggplot(subset(logdat[logdat$time > as.POSIXct("2019-04-12 00:00:01", tz=""),], logger %in% c("C", "D", "E")), aes(x=time, y=pF, colour = logger))+geom_point()


a<-ggplot(logdat[logdat$time > as.POSIXct("2019-04-29 00:00:01", tz=""),], aes(x=time, y=pF, colour = logger))+geom_line()+ylab("pF")
b<-ggplot(logdat[logdat$time > as.POSIXct("2019-04-29 00:00:01", tz=""),], aes(x=time, y=s_temp, colour = logger))+geom_line()+ylab("soil temp")
subplot(a, b, nrows = 2, margin = 0.04, heights = c(0.5, 0.5), shareX = TRUE, titleY = TRUE)


ggplot(subset(logdat, time > as.POSIXct("2019-04-03 00:00:01", tz="")  & time < as.POSIXct("2019-04-05 00:00:01", tz="") & logger =="A"), aes(x=time, y=s_temp))+
  geom_point()+
  geom_point(data=subset(logdat, time > as.POSIXct("2019-04-03 00:00:01", tz="")  & time < as.POSIXct("2019-04-05 00:00:01", tz="") & logger =="A"), aes(x=time, y=hk_temp), colour = "red")+
  scale_y_log10()


logdat_dt<- logdat_dt[rowSums(is.na(logdat_dt[,-9])) !=ncol(logdat_dt[,-9])]
logdat_dt[, max(pF), by =logdate]


##Batterycheck----
subset(logdat, time > as.POSIXct("2019-08-05 00:00:01", tz="") & hk_bat<=12)$logger
ggplot(subset(logdat, time > as.POSIXct("2019-06-19 00:00:01", tz="")), aes(x=time, y=hk_bat, colour = logger))+geom_point()

logger <- subset(logdat, logger=="A")
logger <- logger[complete.cases(logger),]
logger$timevec<-c(0,cumsum(as.numeric(diff(logger$time)))/(60*24))
logger<-logger[c(-1,-2,-3),]
btm <- lm(hk_bat~timevec, data=logger)
pred<-data.frame(timevec=seq(1:300))
pred$p<- predict(btm, pred)
pred[pred$p<11.8,] #A: after 8 days prediction to fall below 11.8V --> 88 days #after 11 d pred --> 107 d

ggplot(pred, aes(x=timevec, y=p))+geom_line()+geom_point(data=logger, aes(x=timevec, y=hk_bat), colour="#CC0000")

##summaries---
library(data.table)
logdat_dt <- setDT(logdat)
logdat_dt$logdate <- interaction(logdat_dt$logger, logdat_dt$date)
logdat_dt<- logdat_dt[rowSums(is.na(logdat_dt[,-9])) !=ncol(logdat_dt[,-9])]
maxlog <- logdat_dt[, max(pF), by =logdate]
maxlog$logdate<- as.character(maxlog$logdate)
maxlog$logger <- sapply(strsplit(maxlog$logdate, "\\."),`[`,1)
maxlog$date <- sapply(strsplit(maxlog$logdate, "\\."),`[`,2)
maxlog<-maxlog[-3,] #----------hardcode!!?

library(dplyr)
library(magrittr)
logdat_summary <- logdat[!(is.na(logdat$time)),]
logdat_summary %<>% 
  #filter(time > as.POSIXct("2019-04-02 23:59:01", tz=""), time < as.POSIXct("2019-05-03 00:00:01", tz = ""))%>% 
  filter(time > as.POSIXct("2019-04-02 23:59:01", tz=""), time < as.POSIXct("2019-04-11 00:00:01", tz = ""))%>% 
  filter(logger %in% c("A", "B", "C", "F", "G"))%>%
  mutate(daynight = ifelse(hour > 6 & hour <= 18, "day", "night")) %>% 
  group_by(date, daynight)%>% 
  mutate(meantemp_DN = mean(s_temp))%>% 
  group_by(datehour)%>% 
  mutate(meantemp = mean(s_temp))

logdat_summary$datehourposix <- as.POSIXct(paste(logdat_summary$date, logdat_summary$hour, 0, 1), tz = "", format= "%Y-%m-%d %H %M%S")

ggplot(logdat_summary, aes(x=datehourposix, y=meantemp))+
  geom_point(aes(colour = daynight))+
 # geom_smooth(colour = "black", se = F)+
  scale_x_datetime(date_breaks = "2 days", date_minor_breaks = "1 day")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1))+
  ylab("soil temperature [°C]")+
  xlab("")+
  labs(colour = "")+
  geom_segment(aes(y = min(logdat_summary$meantemp), yend = min(logdat_summary$meantemp), x = mean(logdat_summary$time[logdat_summary$meantemp == min(logdat_summary$meantemp)])), xend = min(logdat_summary$time)-days(2), colour = "black")+
  geom_segment(aes(y = max(logdat_summary$meantemp), yend = max(logdat_summary$meantemp), xend = mean(logdat_summary$time[logdat_summary$meantemp == max(logdat_summary$meantemp)])), x = min(logdat_summary$time)-days(2), colour = "black")+
  scale_y_continuous(breaks = c(round(min(logdat_summary$meantemp), digits = 2), 12, 14, 16, 18, round(max(logdat_summary$meantemp), digits = 2)))


ggplot(maxlog[c(-1,-2,-3,-4,-5)], aes(x=date, y=V1, fill=logger))+geom_col(position=position_dodge())

##summaries for GIF --- 
logdat_dt <- setDT(logdat[logdat$time > as.POSIXct("2019-05-1 00:00:01", tz=""),])
logdat_dt$logdatehour <- interaction(logdat_dt$logger, logdat_dt$datehour)
logdat_dt<- logdat_dt[!(is.na(logdat_dt$date))]
maxlog <- logdat_dt[, mean(s_temp), by =logdatehour]
maxlog$logdatehour<- as.character(maxlog$logdatehour)
maxlog$logger <- sapply(strsplit(maxlog$logdatehour, "\\."),`[`,1)
maxlog$date <- sapply(strsplit(maxlog$logdatehour, "\\."),`[`,2)
maxlog$hour <- sapply(strsplit(maxlog$logdatehour, "\\."),`[`,3)
maxlog$datehour <- interaction(maxlog$date, maxlog$hour)
maxlog$day_time <- paste0(maxlog$date, " ",maxlog$hour, "h")

##sandbox------
maxlog$x<- 0
maxlog$y<- 0
maxlog$z<- 0

maxlog[maxlog$logger == "A"]$x <- 1
maxlog[maxlog$logger == "A"]$y <- 1
maxlog[maxlog$logger == "A"]$z <- -20

maxlog[maxlog$logger == "B"]$x <- 1
maxlog[maxlog$logger == "B"]$y <- 3
maxlog[maxlog$logger == "B"]$z <- -20

maxlog[maxlog$logger == "C"]$x <- 2
maxlog[maxlog$logger == "C"]$y <- 2
maxlog[maxlog$logger == "C"]$z <- -20

maxlog[maxlog$logger == "D"]$x <- 3
maxlog[maxlog$logger == "D"]$y <- 2
maxlog[maxlog$logger == "D"]$z <- -30

maxlog[maxlog$logger == "E"]$x <- 4
maxlog[maxlog$logger == "E"]$y <- 2
maxlog[maxlog$logger == "E"]$z <- -50

maxlog[maxlog$logger == "F"]$x <- 5
maxlog[maxlog$logger == "F"]$y <- 1
maxlog[maxlog$logger == "F"]$z <- -20

maxlog[maxlog$logger == "G"]$x <- 5
maxlog[maxlog$logger == "G"]$y <- 3
maxlog[maxlog$logger == "G"]$z <- -20

ggplot(subset(logdat[logdat_scale$time > as.POSIXct("2019-04-02 00:00:01", tz=""),], logger == "G"), aes(x=time, y=hk_rh, colour = logger))+geom_point()+geom_line()+geom_line(data=subset(logdat_lascar), aes(x=time, y=a_hum, colour=logger))

library(akima)
a <- interp(x=t1$x, y=t1$z, z=t1$va, xo = seq(min(t1$x), max(t1$x), by=0.1), yo = seq(min(t1$z), max(t1$z), by=0.1), duplicate = "mean")
filled.contour(a)

a <- interp(x=t1$x, y=t1$y, z=t1$va, xo = seq(min(t1$x), max(t1$x), by=0.01), yo = seq(min(t1$y), max(t1$y), by=0.01), duplicate = "mean")
filled.contour(a)
#xydiagram
maxlog1 <- maxlog
maxlog1$time <- as.POSIXct(maxlog1$datehour, format = "%Y-%m-%d.%H")
maxlog1 <- subset(maxlog1, time > as.POSIXct("2019-04-03 00:00:01", tz="") & time < as.POSIXct("2019-08-07 00:00:01", tz=""))
a <- interp(x=maxlog1$time, y=maxlog1$z, z=maxlog1$V1, xo = seq(min(maxlog1$time), max(maxlog1$time), by="hour"), yo = seq(min(maxlog1$z), max(maxlog1$z), by=1), duplicate = "mean")
a$x<-as.POSIXct(a$x, origin = "1970-01-01")
cols = rev(colorRampPalette(c('darkred','red','lightblue', 'blue'))(26))
filled.contour(a, col = cols, plot.title = title(main = paste0("soil temperature [°C]"), ylab="Soil Depth [cm]", xlab=""))


a <- interp(x=maxlog1$x, y=maxlog1$z, z=maxlog1$V1, xo = seq(min(maxlog1$x), max(maxlog1$x), by=0.1), yo = seq(min(maxlog1$z), max(maxlog1$z), by=0.1), duplicate = "mean")

ggplot(subset(maxlog, date == "2019-04-09" & !(logger == "A") & !(logger =="G")))+aes(x=y, y=z, z=V1)+geom_tile(aes(fill=V1))+stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005)

##gif sandbox-----
library(akima)

cols = rev(colorRampPalette(c('darkred','red','lightblue', 'blue'))(32))
maxlog1 <- subset(maxlog, datehour==unique(maxlog$datehour)[140])
a <- interp(x=maxlog1$x, y=maxlog1$z, z=maxlog1$V1, xo = seq(min(maxlog1$x), max(maxlog1$x), by=0.1), yo = seq(min(maxlog1$z), max(maxlog1$z), by=0.1), duplicate = "mean")
filled.contour(a,zlim = c(0.4,2.7), col = cols, plot.title = title(main = maxlog$datehour[140]))



library(animation)
frames = length(unique(maxlog$datehour))
saveGIF({
  for (i in 7:frames){
    maxlog1 <- subset(maxlog, date==maxlog$date[i])
    a <- interp(x=maxlog1$x, y=maxlog1$z, z=maxlog1$V1, xo = seq(min(maxlog1$x), max(maxlog1$x), by=0.1), yo = seq(min(maxlog1$z), max(maxlog1$z), by=0.1), duplicate = "mean")
    filled.contour(a, zlim = c(0.4,2.7), plot.title = title(main = maxlog$date[i]))
    }
  
},interval = .5, movie.name="test.gif")

cols = rev(colorRampPalette(c('darkred','red','lightblue', 'blue'))(21))
maxlog <- maxlog[maxlog$date > as.POSIXct("2019-04-30 00:00:01", tz=""),]
frames = length(unique(maxlog$datehour))
saveGIF({
  for (i in 1:frames){
    
    maxlog1 <- subset(maxlog, datehour==unique(maxlog$datehour)[i])
    a <- interp(x=maxlog1$x, y=maxlog1$z, z=maxlog1$V1, xo = seq(min(maxlog1$x), max(maxlog1$x), by=0.1), yo = seq(min(maxlog1$z), max(maxlog1$z), by=0.1), duplicate = "mean")
    filled.contour(a, zlim = c(0.4,2.7), col = cols, plot.title= title(main = paste0("pF     ", maxlog$day_time[i]), ylab="Soil Depth", xlab="Field width"))
    if (i == frames){
      replicate(60, filled.contour(a, zlim = c(0.4,2.7), col = cols,  plot.title = title(main = paste0("pF     ", maxlog$day_time[i]), ylab="Soil Depth", xlab="Field width")))
    }}

},interval = .05, movie.name="pF.gif")


#loesses
ggplot(subset(logdat, time > as.POSIXct("2019-04-03 00:00:01", tz="") & time < as.POSIXct("2019-04-19 23:59:01", tz="") & !(logger %in% c("D", "E"))), aes(x=time, y=s_temp))+geom_point()+geom_smooth(colour = "red", method = "loess") + ylab("soil temperature in 20 cm depth [°C]")
air <- subset(logdat_lascar, time > as.POSIXct("2019-04-03 00:00:01", tz=""))
ggplot(air, aes(x=time, y=a_temp))+geom_point(color="black")+geom_smooth(method = "loess", colour="red")+ylab("air temperature [°C]")+xlab("")

#-------lascar------
##summaries --- 
library(data.table)

logdat_lascar$date<-as.Date(logdat_lascar$time, tz="")
logdat_lascar$hour<-hour(logdat_lascar$time)
logdat_lascar$datehour <- interaction(logdat_lascar$date, logdat_lascar$hour)

logdat_lascar_dt <- setDT(logdat_lascar)
logdat_lascar_dt$logdatehour <- interaction(logdat_lascar_dt$logger, logdat_lascar_dt$datehour)
logdat_lascar_dt<- logdat_lascar_dt[!(is.na(logdat_lascar_dt$date))]

maxlog_lascar <- logdat_lascar_dt[, mean(a_temp), by =logdatehour]
maxlog_lascar$logdatehour<- as.character(maxlog_lascar$logdatehour)
maxlog_lascar$logger <- sapply(strsplit(maxlog_lascar$logdatehour, "\\."),`[`,1)
maxlog_lascar$date <- sapply(strsplit(maxlog_lascar$logdatehour, "\\."),`[`,2)
maxlog_lascar$hour <- sapply(strsplit(maxlog_lascar$logdatehour, "\\."),`[`,3)
maxlog_lascar$datehour <- interaction(maxlog_lascar$date, maxlog_lascar$hour)
maxlog_lascar$day_time <- paste0(maxlog_lascar$date, " ",maxlog_lascar$hour, "h")

##sandbox------
maxlog_lascar$x<- 0
maxlog_lascar$y<- 0


maxlog_lascar[maxlog_lascar$logger == "L9"]$x <- 1
maxlog_lascar[maxlog_lascar$logger == "L9"]$y <- 1

maxlog_lascar[maxlog_lascar$logger == "L1"]$x <- 1
maxlog_lascar[maxlog_lascar$logger == "L1"]$y <- 3

maxlog_lascar[maxlog_lascar$logger == "L3"]$x <- 2
maxlog_lascar[maxlog_lascar$logger == "L3"]$y <- 2

maxlog_lascar[maxlog_lascar$logger == "L5"]$x <- 3
maxlog_lascar[maxlog_lascar$logger == "L5"]$y <- 3

maxlog_lascar[maxlog_lascar$logger == "L4"]$x <- 3
maxlog_lascar[maxlog_lascar$logger == "L4"]$y <- 1

library(akima)
maxlog_lascar1 <-subset(maxlog_lascar, !(logger =="L6") & hour == 14)
maxlog_lascar1$time <- as.POSIXct(maxlog_lascar1$datehour, format = "%Y-%m-%d.%H")
maxlog_lascar1 <- subset(maxlog_lascar1, time > as.POSIXct("2019-04-03 00:00:01", tz=""))
a <- interp(x=maxlog_lascar1$time, y=maxlog_lascar1$y, z=maxlog_lascar1$V1, xo = seq(min(maxlog_lascar1$time), max(maxlog_lascar1$time), by="hour"), yo = seq(min(maxlog_lascar1$y), max(maxlog_lascar1$y), by=1), duplicate = "mean")
a$x<-as.POSIXct(a$x, origin = "1970-01-01")
cols = rev(colorRampPalette(c('darkred','red','lightblue', 'blue'))(27))
filled.contour(a, col = cols, plot.title = title(main = paste0("airtemp"), ylab="field width", xlab=""))

#air temp within field
ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & !(logger =="L6")), aes(x=date, y=a_temp))+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& !(logger =="L6")), colour = "blue")+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& !(logger =="L6")), colour = "red")+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& !(logger =="L6")), colour = "blue",  method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& !(logger =="L6")), colour = "red",  method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle("Air temperature within field")+
  xlab("")+
  ylab("Air temperature [°C]")+
  scale_color_discrete(labels = c("night", "day"))+
  theme(legend.position="right")

#air humidity within field
ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & !(logger =="L6")), aes(x=date, y=a_rh))+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& !(logger =="L6")), colour = "blue")+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& !(logger =="L6")), colour = "red")+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& !(logger =="L6")), colour = "blue",  method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& !(logger =="L6")), colour = "red",  method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle("Air Relative Humidity")+
  xlab("")+
  ylab("Air Relative Humidity [%]")+
  scale_color_discrete(labels = c("night", "day"))+
  theme(legend.position="right")

#air temp shaded
ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & (logger =="L6")), aes(x=date, y=a_temp))+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& (logger =="L6")), colour = "blue")+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& (logger =="L6")), colour = "red")+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& (logger =="L6")), colour = "blue", method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& (logger =="L6")), colour = "red", method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle("Air temperature")+
  xlab("")+
  ylab("Air temperature [°C]")+
  scale_color_discrete(labels = c("night", "day"))+
  theme(legend.position="right")

#air relative humidity shaded
ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & (logger =="L6")), aes(x=date, y=a_rh))+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& (logger =="L6")), colour = "blue")+
  geom_point(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& (logger =="L6")), colour = "red")+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 23& (logger =="L6")), colour = "blue", method = "gam", formula = y ~ s(x, bs = "cs"))+
  geom_smooth(data = subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="")& hour == 14& (logger =="L6")), colour = "red", method = "gam", formula = y ~ s(x, bs = "cs"))+
  ggtitle("Air Relative Humidity")+
  xlab("")+
  ylab("Air Relative Humidity [%]")+
  scale_color_discrete(labels = c("night", "day"))+
  theme(legend.position="right")

ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & logger == "L6"), aes(x=time, y=a_temp, colour = logger))+geom_line()


ggplot(subset(logdat_lascar, time > as.POSIXct("2019-05-01 00:00:01", tz="") & !(logger == "L6")& hour == 14), aes(x=date, y=a_temp))+
  geom_point()+
  stat_summary(aes(y=a_temp, group = date), fun.y = "mean", colour = "red", geom = "line", group=logger)


#time series analysis
ts_s_temp <- logdat_merge  %>% 
  filter(logger == "A")  %>% 
  filter(variable == "PAR")  %>% 
  select(date, hour, m_value)  %>% 
  summarise_all(., mean)  %>%  
  as.data.frame()  %>% 
  mutate(time = as.POSIXct(paste0(date, " ", hour, ":00:00"), format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(time > as.POSIXct("2019-03-29 00:00:01")) %>% 
  select(m_value, time)
  

library(forecast)
ts_s_temp_xts <- msts(ts_s_temp$m_value, seasonal.periods = c(24))
ts_s_temp_xts  %>% decompose()  %>% autoplot()
