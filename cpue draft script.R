libary(tidyverse)
arcgn<-mydata
table(arcgn$common_name, useNA = "always")
arcgn$meshsize_inch
arcgn$meshsize_inch[arcgn$meshsize_inch %in% c("2.78","2.875","2.89","2.895")]<-"2.895"
arcgn$meshsize_inch[arcgn$meshsize_inch %in% c("G","N")]<-"1.5"
arcgn$meshsize_inch<-factor(arcgn$meshsize_inch)
names(arcgn)
which(arcgn$fishendtime <0)
which(arcgn$fishstarttime <0)
arcgn$dt<-difftime(arcgn$fishendtime,arcgn$fishstarttime)
which(arcgn$dt >200)
rcep$medtime<-arcep$starttime+(arcep$endtime-arcep$starttime)/2
which(arcep$medtime<1.5) #check them
#Check for fishstart and end times that are NA
which(is.na(arcep$fishstarttime))
which(is.na(arcep$fishendtime))
#Replace fish start/end times with medtime (median time of the day)
arcep<-arcep%>%mutate(fishstarttime = if_else(!is.na(fishstarttime), fishstarttime, medtime))
#Add in Effort
arcgn<-arcgn%>%group_by(date)%>%mutate(effort=as.numeric(difftime(fishendtime,fishstarttime, units = "min")))%>%data.frame()
head(arcgn)
str(arcgn)
#Summary by
arcgnsum<-arcgn%>%filter(!is.na(common_name), site %in% c("Tideside","Lakeside"))%>%
  group_by(date, site, common_name, meshsize_inch, effort)%>%
  summarize(netabundance=sum(abundance))%>%
  mutate(cpue=(netabundance/effort)*30)%>%
  data.frame()
ggplot(arcgnsum,aes(x=as.Date(date),y=cpue))+
  geom_point()+
  facet_grid(.~common_name)


# model.results<-glm(response~predictor+predictor2,family="poisson",data=mydata)
# summary(model.results)
