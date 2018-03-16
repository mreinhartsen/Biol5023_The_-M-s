library(tidyverse)
library(lubridate)
# fix date
blpw.all <- readRDS("blpw.all.RDS")
blpw.all$year<-as.integer(blpw.all$year)
blpw.all$month<-as.integer(blpw.all$month)

#create date objects
blpw.all$date<-with(blpw.all, ymd(paste(year,month,day,sep = "-")))

#group by band # and order by dates & mutate a new variable for net change in mass (relative mass)
table1<-blpw.all%>%group_by(band)%>%
  arrange(band,date)%>%
  mutate(rel_mass=mass-first(mass))%>%data.frame()

#horizontally faceted plot by location
ggplot(table1,aes(x=as.factor(date),y=rel_mass,group=month))+
  geom_jitter(aes(color=band))+
  geom_point(aes(color=band), alpha=0.2)+
  geom_line(aes(color=band))+
  facet_wrap(~location)+
  theme(legend.position="none")




