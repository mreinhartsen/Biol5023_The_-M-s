library(tidyverse)
library(lubridate)
arcfish<- read.delim("ARC_FINAL_14Feb2018.txt", na.strings = c("NA",""))

# Create Dates and Times --------------------------------------------------
names(arcfish)
#date
dt<-with(arcfish,paste(year,month,day,sep="-"))
arcfish$date<-ymd(dt,tz="America/Halifax")
#daily start time & end time
dt<-with(arcfish,paste(year,month,day,starttime_hour,starttime_min,sep="-"))
arcfish$starttime<-ymd_hm(dt,tz="America/Halifax")
dt<-with(arcfish,paste(year,month,day,endtime_hour,endtime_min,sep="-"))
arcfish$endtime<-ymd_hm(dt,tz="America/Halifax")
#Fishing start time & end time
dt<-with(arcfish,paste(timein_year,timein_month,timein_day,timein_hour,timein_min,sep="-"))
arcfish$fishstarttime<-ymd_hm(dt,tz="America/Halifax")
dt<-with(arcfish,paste(timein_year,timeout_month,timeout_day,timeout_hour,timeout_min,sep="-"))
arcfish$fishendtime<-ymd_hm(dt,tz="America/Halifax")
arcfish$time<-arcfish$endtime-arcfish$starttime
#Separate by site/ group by set-time
arcfish2<-arcfish%>%filter(site %in% c("Lakeside","Tideside"))%>%
group_by(date, site)%>%slice(1)%>%summarize(sumtime=sum(time))%>%data.frame()
#break down effort(time) by week
arcfish2<-arcfish%>%filter(site %in% c("Lakeside","Tideside"))%>%
  group_by(week=floor_date(date, "week"), site)%>%slice(1)%>%summarize(sumtime=sum(time))%>%data.frame()

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
#Summary by site and CPUE
arcgnsum<-arcgn%>%filter(!is.na(common_name), site %in% c("Tideside","Lakeside"))%>%
  group_by(date, site, common_name, meshsize_inch, effort)%>%
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
  summarize(netabundance=sum(abundance))%>%
  mutate(cpue=(netabundance/effort)*30)%>%
  data.frame()
#----------------------------------------
  arcfish3<-arcfish%>%filter(site %in% c("Lakeside","Tideside"), !is.na(surfacetemp_fishfinder_farenheit))%>%
    group_by(day=floor_date(date, "day"), site)%>%slice(1)%>%summarize(meantemp=mean(surfacetemp_fishfinder_farenheit))%>%data.frame()
  arcfish3$sstc<-(arcfish3$meantemp-32)/1.8
  
  arcA<-arcgnsum%>%filter(common_name=="white") 
  m1 <- lm(cpue~date,data=arcA)
  summary(m1)
  ggplot(arcA,aes(date,cpue)) +
    geom_point()