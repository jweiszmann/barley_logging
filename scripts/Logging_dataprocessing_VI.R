library(readr)
library(rlist)
library(tidyverse)
library(lubridate)
library(plotly)
library(tidyquant)
##----load data (Ecotech)----

#set working directory (Ecotech):
setwd("E:/Uni/Ucloud_sync/Barley_Climate_Logging/data/2021/Ecotech") #CHANGE TO CORRECT DIRECTORY

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
setwd("E:/Uni/Ucloud_sync/Barley_Climate_Logging/data/2021/Lascar")  #CHANGE TO CORRECT DIRECTORY

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

##----simple plots--- 
#choose loggers to be plotted (can also be more, or just one):
log_sel <- c("E1", "E2", "E4")

#plot; y = variable that should be displayed
logdat  %>% 
  filter(logger %in% log_sel)  %>% 
  ggplot(., aes(x=time, y=PAR, colour = logger)) + 
  geom_line()

logdat  %>% 
  filter(logger %in% log_sel)  %>% 
  ggplot(., aes(x=time, y=hk_bat, colour = logger)) + 
  geom_point()+ 
  geom_smooth(method="lm")

##plot a specific time window:
logdate  %>% 
  filter(logger %in% log_sel)  %>% 
  filter(time > as.POSIXct("2021-04-01 00:00:01", tz=""))  %>% #FROM
  filter(time < as.POSIXct("2021-04-08 00:00:01", tz=""))  %>% #TO
  ggplot(., aes(x=time, y=s_temp, colour = logger)) + 
  geom_line()



