time_start <- as.POSIXct("2019-04-03 00:00:01", tz="")
time_end <- as.POSIXct("2019-04-11 00:00:01", tz="")


#aitemp nonshaded summary
logdat_lascar_summary <- logdat_lascar %>% 
  filter(time > time_start, time < time_end)  %>% 
  mutate(daynight = ifelse(hour > 6 & hour <= 18, "day", "night")) %>% 
  group_by(date, daynight)%>% 
  mutate(meantemp_DN = mean(a_temp))%>% 
  group_by(datehour)%>% 
  mutate(meantemp = mean(a_temp)) %>% 
  mutate(datehourposix = as.POSIXct(paste(date, hour, 0, 1), tz = "", format= "%Y-%m-%d %H %M%S"))



#air temp nonshaded
ggplot(subset(logdat_lascar_summary, time > time_start & time < time_end), aes(x=datehourposix, y=meantemp))+
  geom_point(aes(colour = daynight))+
  # geom_smooth(colour = "black", se = F)+
  scale_x_datetime(date_breaks = "2 days", date_minor_breaks = "1 day")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1))+
  ylab("air temperature [°C]")+
  xlab("")+
  labs(colour = "")+
  geom_segment(aes(y = min(logdat_lascar_summary$meantemp), yend = min(logdat_lascar_summary$meantemp), x = mean(logdat_lascar_summary$time[logdat_lascar_summary$meantemp == min(logdat_lascar_summary$meantemp)])), xend = min(logdat_lascar_summary$time)-days(2), colour = "black")+
  geom_segment(aes(y = max(logdat_lascar_summary$meantemp), yend = max(logdat_lascar_summary$meantemp), xend = mean(logdat_lascar_summary$time[logdat_lascar_summary$meantemp == max(logdat_lascar_summary$meantemp)])), x = min(logdat_lascar_summary$time)-days(2), colour = "black")+
  scale_y_continuous(breaks = c(round(min(logdat_lascar_summary$meantemp), digits = 2), 10, 15, 20, 25, round(max(logdat_lascar_summary$meantemp), digits = 2)))

#PAR summary
logdat_summary %<>% 
  filter(time > time_start, time < time_end)%>% 
  filter(logger %in% c("A", "B", "C", "F", "G"))%>%
  mutate(daynight = ifelse(hour > 6 & hour <= 18, "day", "night")) %>% 
  group_by(date, daynight)%>% 
  mutate(meantemp_DN = mean(s_temp))%>% 
  group_by(datehour)%>% 
  mutate(meantemp = mean(s_temp))   %>%
  ungroup() %>% 
  group_by(logger, datehour)  %>%
  mutate(meanpar = mean(PAR))


logdat_summary  %>%  filter(time > time_start, time < time_end)  %>% 
  ggplot(., aes(x=datehourposix, y= meanpar)) +
  geom_line()+
  scale_x_datetime(date_breaks = "2 days", date_minor_breaks = "1 day")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust=1))+
  ylab("PAR [µE]")+
  xlab("")+
  labs(colour = "")
  