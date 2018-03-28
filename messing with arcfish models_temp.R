# library(tidyverse)
library(lubridate)
library(tidyverse)
library(gam)
library(mgcv)
library(effects)
install.packages("visreg")
library(visreg)
arcfish<- read.delim("ARC_FINAL_14Feb2018.txt", na.strings = c("NA",""))

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
arcgn$meshsize_inch<-as.integer(arcgn$meshsize_inch)
arcgn$airtemp_c<-as.integer(arcgn$airtemp_c)
arcgn$tl_cm<-as.integer(arcgn$tl_cm)
arcgn$surfacetemp_fishfinder_farenheit<-as.integer(arcgn$surfacetemp_fishfinder_farenheit)
table(arcgn$surfacetemp_fishfinder_farenheit,useNA="no")
table(arcgn$percentcloudcover,useNA="no")
table(arcgn$airtemp_c,useNA="no")
table(arcgn$tl_cm,useNA="no")
table(arcgn$sex,useNA="no")

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
  group_by(date, effort,common_name,airtemp_c,percentcloudcover,surfacetemp_fishfinder_farenheit)%>%
  summarize(netabundance=sum(abundance)) %>%
  mutate(cpue=(netabundance/effort)*30)


arcgnsum<-arcgnsum %>%
  group_by(date,effort,common_name,airtemp_c,percentcloudcover,surfacetemp_fishfinder_farenheit) %>%
  summarize(netdayeffort=sum(effort),netdayabundance=sum(netabundance)) %>%
  mutate(cpue=(netdayabundance/netdayeffort))
#non-cpue related
arcgnsum3<-arcgn%>%filter(!is.na(common_name), site %in% c("Tideside","Lakeside"))%>%
  group_by(date, effort,common_name,abundance,tl_cm,surfacetemp_fishfinder_farenheit, airtemp_c,percentcloudcover)%>%
  summarize(netabundance=sum(abundance))

#filter for target species, aka Alewife
arcA<-arcgnsum%>%filter(common_name=="Alewife")%>%data.frame()
# arcSB<-arcgnsum%>%filter(common_name=="Striped Bass")%>%data.frame()
# arcWP<-arcgnsum%>%filter(common_name=="White Perch")%>%data.frame()

arcgnsum2<-arcA%>%filter(cpue<2)%>%filter(date >= "2017-04-25"|date <= "2017-05-25")%>%filter(airtemp_c>0)%>%filter(!is.na(percentcloudcover))
ggplot(arcgnsum2, aes(x=airtemp_c, y=cpue))+
  geom_point()

CPUEMAX <- max(arcgnsum2$cpue)
ArcMax <- filter(arcgnsum2, cpue == CPUEMAX)
arcgnsum2$days_since_Max <- as.numeric(abs(as.Date(as.character(arcgnsum2$date), format="%Y-%m-%d") - as.Date(as.character(ArcMax$date), format="%Y-%m-%d")))

#linear model
m1 <- lm(netdayabundance~ + percentcloudcover + surfacetemp_fishfinder_farenheit + airtemp_c + offset(netdayeffort), data=arcgnsum)
summary(m1)
plot(m1)
plot(allEffects(m1))

#alewife
m1b <- glm(netdayabundance ~airtemp_c + percentcloudcover + surfacetemp_fishfinder_farenheit + offset(log(netdayeffort)), family = "poisson",data=arcgnsum2)
summary(m1b)
plot(m1b)
plot(allEffects(m1b))

#-----------------------------------
#Generalized additived model
#m2 <- gam(netdayabundance~ te(surfacetemp_fishfinder_farenheit)+offset(log(netdayeffort)),link=date(),data=arcgnsum)
m2 <- gam(netdayabundance~ offset(log(netdayeffort))+ s(surfacetemp_fishfinder_farenheit),data=arcgnsum2)
summary(m2)
visreg(m2)
plot(allEffects(m2))

#m6 Generalized Additive Model Smoothed
#m6<-gam(netdayabundance~s(surfacetemp_fishfinder_farenheit) +s(percentcloudcover) +s(days_since_Max)+ offset(log(netdayeffort)),link=date(),family="poisson",data=arcgnsum2, method = "REML")

m6<-gam(netdayabundance~s(surfacetemp_fishfinder_farenheit)+
        s(percentcloudcover)+
        s(days_since_Max)+
        offset(log(netdayeffort)),link=date,family="poisson",data=arcgnsum2, method = "REML")


summary(m6)
visreg(m6)
plot(m6)



# Now let's look at the "peak" run in the spring based on our abundance data


m4<-gam(cpue~airtemp_c + percentcloudcover + surfacetemp_fishfinder_farenheit,link=meshsize_inch,data=arcgnsum2)
summary(m4)
coefsc = coefficients(m4); coefsc
plot(m4)
gam.check(m4)
plot(allEffects(m4))

#---------------------
m5<-lm(netdayabundance + offset(netdayeffort)~airtemp_c,data=arcgnsum2)
summary(m5)

m3 <- lm(cpue~airtemp_c + percentcloudcover,data=arcSB)
summary(m3)
plot(allEffects(m3))
#---------------------
  #fit model with abundance; use repsonse of effort offset