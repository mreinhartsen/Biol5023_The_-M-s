# library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
arcfish<- read.delim("ARC_FINAL_18Jan2018.txt", na.strings = c("NA",""))

# Create Dates and Times --------------------------------------------------
names(arcfish)
#date
dt<-with(arcfish,paste(year,month,day,sep="-"))
arcfish$date<-ymd(dt,tz="America/Halifax")
#daily start time
dt<-with(arcfish,paste(year,month,day,starttime_hour,starttime_min,sep="-"))
arcfish$starttime<-ymd_hm(dt,tz="America/Halifax")
#daily end time
dt<-with(arcfish,paste(year,month,day,endtime_hour,endtime_min,sep="-"))
arcfish$endtime<-ymd_hm(dt,tz="America/Halifax")
#Fishing start time
dt<-with(arcfish,paste(timein_year,timein_month,timein_day,timein_hour,timein_min,sep="-"))
arcfish$fishstarttime<-ymd_hm(dt,tz="America/Halifax")
#Fishing end time
dt<-with(arcfish,paste(timein_year,timeout_month,timeout_day,timeout_hour,timeout_min,sep="-"))
arcfish$fishendtime<-ymd_hm(dt,tz="America/Halifax")
arcfish$time<-arcfish$endtime-arcfish$starttime

arcfish2<-arcfish%>%filter(site %in% c("Lakeside","Tideside"))%>%
group_by(date, site)%>%slice(1)%>%summarize(sumtime=sum(time))%>%data.frame()
#break down effort(time) by week
arcfish2<-arcfish%>%filter(site %in% c("Lakeside","Tideside"))%>%
  group_by(week=floor_date(date, "week"), site)%>%slice(1)%>%summarize(sumtime=sum(time))%>%data.frame()
#Project Fishing effort
ggplot(arcfish2, aes(x=as.Date(week), y=sumtime))+
  geom_point(aes(shape=site),size=4)+
  geom_bar(aes(fill=site), stat="identity")+
  scale_x_date(date_labels = "%b %d",date_breaks = "2 week")+
  xlab("Date")+
  ylab("Project Time (hours per week)")+
  guides(fill=guide_legend(title="Site"), shape=FALSE)+
  theme_bw(12)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#separate gillnet
arcgn<-arcfish%>%filter(gear_type %in% "Gillnet")
arcgn$gear_type<-factor(arcgn$gear_type)
levels(arcgn$gear_type)

#Check for fishstart and end times that are NA
which(is.na(arcgn$fishstarttime))
which(is.na(arcgn$fishendtime))
arcgn$common_name<-factor(arcgn$common_name)
table(arcgn$common_name, useNA = "always")

arcgn$meshsize_inch
arcgn$meshsize_inch[arcgn$meshsize_inch %in% c("2.78","2.875","2.89","2.895")]<-"2.895"
arcgn$meshsize_inch[arcgn$meshsize_inch %in% c("G","N")]<-"1.5"
arcgn$meshsize_inch<-factor(arcgn$meshsize_inch)

which(arcgn$fishendtime <0)
which(arcgn$fishstarttime <0)
arcgn$dt<-difftime(arcgn$fishendtime,arcgn$fishstarttime)
which(arcgn$dt >200)

#Add in Effort
arcgn<-arcgn%>%group_by(date)%>%mutate(effort=as.numeric(difftime(fishendtime,fishstarttime, units = "min")))%>%data.frame()
head(arcgn)
str(arcgn)
#Summary by
arcgnsum<-arcgn%>%filter(!is.na(common_name), site %in% c("Tideside","Lakeside"))%>%
  group_by(date, site, common_name, meshsize_inch, effort)%>%
<<<<<<< HEAD
  summarize(netabundance=sum(abundance))

  # mutate(cpue=(netabundance/effort)*30)%>%
arcgnsum<-arcgnsum %>%
  group_by(date,common_name) %>%
  summarize(netdayeffort=sum(effort),netdayabundance=sum(netabundance)) %>%
  mutate(cpue=(netdayabundance/netdayeffort))

#filter for target species, aka Alewife
arcA<-arcgnsum%>%filter(common_name=="Alewife") 



m1 <- lm(cpue~date,data=arcA)
summary(m1)
ggplot(arcA,aes(date,cpue)) +
  geom_point()


ggplot(arcA,aes(x=date,y=cpue))+
  geom_point(size=3)
  # geom_line()+
  # geom_smooth()+
  # scale_x_date(date_labels = "%b %d",date_breaks = "2 week")+
  # xlab("Date")+
  # ylab("CPUE (Fish/30 Min)")+
  # theme_bw(12)+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  # facet_grid(.~site) 
=======
  summarize(netabundance=sum(abundance))%>%
  mutate(cpue=(netabundance/effort)*30)%>%
  data.frame()
>>>>>>> b82e8029f4c9b6d4d11a2c96f8d69c822efd94a6
