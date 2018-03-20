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
  summarize(netabundance=sum(abundance))%>%
  mutate(cpue=(netabundance/effort)*30)%>%
  data.frame()
#------------------------------------------------------------------------------------
#Model CPUE: variables
#Predictive variable: time of year/ time of day/tide height/temp
#Response variable: relative CPUE per mesh size (catch/"effort", defined as gillnet set time)
#CPUE --> Time net is set in water/# fish caught
#------------------------------------------------------------------------


