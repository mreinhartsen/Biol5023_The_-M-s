# library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gam)
library(mgcv)
library(effects)

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

arcfish2<-arcfish%>%filter(site %in% c("Lakeside","Tideside")) %>%
group_by(date, site) %>% slice(1) %>% summarize(sumtime=sum(time)) %>% data.frame()
#break down effort(time) by week
arcfish2<-arcfish %>% filter(site %in% c("Lakeside","Tideside")) %>%
  group_by(week=floor_date(date, "week"), site) %>% slice(1) %>% summarize(sumtime=sum(time)) %>% data.frame()
#Project Fishing effort
ggplot(arcfish2, aes(x=as.Date(week), y=sumtime))+
  geom_point(aes(shape=site),size=4)+
  # geom_bar(aes(fill=site), stat="identity")+
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
  summarize(netabundance=sum(abundance))

  # mutate(cpue=(netabundance/effort)*30)%>%
arcgnsum<-arcgnsum %>%
  group_by(date,common_name) %>%
  summarize(netdayabundance=sum(netabundance)) %>%
  # mutate(cpue=(netdayabundance/netdayeffort)) %>%
  mutate(cpue=(netdayabundance/effort)*30)


#filter for target species, aka Alewife
arcA<-arcgnsum%>%filter(common_name=="Alewife")

m1 <- lm(cpue~date,data=arcA)

#filter for target species, pick one
arcA<-arcgnsum%>%filter(common_name=="Alewife") #N = 32
#arcA<-arcgnsum%>%filter(common_name=="Rainbow Smelt") #N = 7
#arcA<-arcgnsum%>%filter(common_name=="White Sucker") #N = 19
#arcA<-arcgnsum%>%filter(common_name=="White Perch") #N = 25
#arcA<-arcgnsum%>%filter(common_name=="Striped Bass") #N = 37
#arcA<-arcgnsum%>%filter(common_name=="Smallmouth Bass") #N = 2
#arcA<-arcgnsum%>%filter(common_name=="American Smooth Flounder") #N = 4
#arcA<-arcgnsum%>%filter(common_name=="Atlantic Tomcod") #N = 3
#arcA<-arcgnsum%>%filter(common_name=="Summer Flounder") #N = 1
#arcA<-arcgnsum%>%filter(common_name=="Blueback Herring") #N = 1
#arcA<-arcgnsum%>%filter(common_name=="Atlantic Sturgeon") #N = 1
#arcA<-arcgnsum%>%filter(common_name=="American Shad") #N = 5
#arcA<-arcgnsum%>%filter(common_name=="American Eel") #N = 1
#arcA<-arcgnsum%>%filter(common_name=="Winter Flounder") #N = 2
#arcA<-arcgnsum%>%filter(common_name=="Sea Lamprey") #N = 1

cat("N = ",nrow(arcA),"\n") # prints the number of days the fish was caught

stopifnot(nrow(arcA) >= 10) # stops program if fish were caught under 10 days total

CPUEMAX <- max(arcA$cpue)
ArcMax <- filter(arcA, cpue == CPUEMAX)
arcA$days_since_Max <- as.numeric(abs(as.Date(as.character(arcA$date), format="%Y-%m-%d") - as.Date(as.character(ArcMax$date), format="%Y-%m-%d")))
arcA$days_since_Maxsigned <- as.numeric(as.Date(as.character(arcA$date), format="%Y-%m-%d") - as.Date(as.character(ArcMax$date), format="%Y-%m-%d"))

#Linear model
<<<<<<< HEAD

m1 <- glm(netdayabundance~days_since_Max+offset(netdayeffort),data=arcA, family = "gaussian")

m1 <- glm(netdayabundance+offset(netdayeffort)~days_since_Max,data=arcA, family = "gaussian")

=======
m1 <- glm(netdayabundance+offset(netdayeffort)~days_since_Max,data=arcA, family = "gaussian")
>>>>>>> c07da34972a4d8ce6d2a324a26cafd5b042d3b63
>>>>>>> aabd41e8b05fc32e71e9535a791d8d2f0f2a1508
summary(m1)
m1fitted =data.frame(arcA,pred=m1$fitted)

plot(m1)

ggplot(m1fitted) +
  geom_point(aes(days_since_Max,netdayabundance))+
  geom_line(aes(days_since_Max,pred))

# ggplot(m1fitted) +
#   geom_point(aes(date,netdayabundance))+
#   geom_line(aes(date,pred))

#Poisson model
m2 <- glm(netdayabundance+offset(netdayeffort)~days_since_Max,data=arcA, family = "poisson")
summary(m2)
m2fitted =data.frame(arcA,pred=m2$fitted)

plot(m2)


m2plot <- ggplot(m2fitted) +
  geom_point(aes(date,cpue))+
  geom_line(aes(date,pred))

# Cannot do binomial on relative abundance index!!!

 m3 <- glm(cpue~date,data=arcA, family = "binomial")
 summary(m3)
 m3fitted =data.frame(arcA,pred=m3$fitted)

#
# ggplot(m3fitted) +
#   geom_point(aes(date,cpue))+

ggplot(m2fitted) +
  geom_point(aes(days_since_Max,netdayabundance))+
  geom_line(aes(days_since_Max,pred))

# ggplot(m2fitted) +
#   geom_point(aes(date,netdayabundance))+

#   geom_line(aes(date,pred))


# =======
  # summarize(netabundance=sum(abundance))%>%
  # mutate(cpue=(netabundance/effort)*30)%>%
  # data.frame()


plot(m2)



#GAM

m5 <- gam(netdayabundance~days_since_Max+offset(netdayeffort),data=arcA)
summary(m5)
m5fitted =data.frame(arcA,pred=m5$fitted)


ggplot(m5fitted) +
  geom_point(aes(days_since_Max,netdayabundance))+
  geom_line(aes(days_since_Max,pred))

plot(allEffects(m5))


#GAM family poisson
m6 <- gam(netdayabundance~s(days_since_Max), offset = log(netdayeffort), data=arcA, family = "poisson", fit = TRUE)
summary(m6)
m6fitted =data.frame(arcA,pred=m6$fitted)

#playing with GAM
plot(m6,pages=1,residuals=TRUE)
plot(m6,pages=1,seWithMean=TRUE)
gam.check(m6)

G <- gam(netdayabundance~s(days_since_Max),fit=FALSE,data=arcA, family = "poisson")
b <- gam(G=G)
print(b)

G <- gam(netdayabundance~s(days_since_Max),fit=FALSE,data=arcA,sp=b$sp,family = "poisson")
G$lsp0 <- log(b$sp*10)
gam(G=G)

b0 <- gam(netdayabundance~s(days_since_Max),data=arcA,method="REML")
plot(b0,pages=1,scheme=1,unconditional=TRUE)

bt <- gam(netdayabundance~te(days_since_Max,k=7),data=arcA,
          method="REML")
plot(bt,pages=1)
plot(bt,pages=1,scheme=2) ## alternative visualization
AIC(b0,bt)

##### plot GAM
ggplot(m6fitted) +
  geom_point(aes(days_since_Max,netdayabundance))+
  geom_line(aes(days_since_Max,pred))


m7 <- gam(netdayabundance+offset(netdayeffort)~days_since_Max,data=arcA, family = "gamma")
summary(m7)
m6fitted =data.frame(arcA,pred=m6$fitted)

m6 <- gam(netdayabundance+offset(netdayeffort)~days_since_Max,data=arcA, family = "poisson")
summary(m5)
m5fitted =data.frame(arcA,pred=m5$fitted)
<<<<<<< HEAD

=======
>>>>>>> c07da34972a4d8ce6d2a324a26cafd5b042d3b63
>>>>>>> aabd41e8b05fc32e71e9535a791d8d2f0f2a1508
