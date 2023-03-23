##Basic parameter for workspace
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/GIT/covid19-eng-lockdown/process/")

##Importing the global data
tbl.g.mob<-read.csv("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/09_IJGS SI/DATA_REVISION/ORI/CKDelta/20201229_gyration.csv")
tbl.g.mob<-subset(tbl.g.mob,grepl("E0",code) & gender=="all" & age=="all") #Excluding Isle of Scily due to the sampel sizes
colnames(tbl.g.mob)<-c("date","LTLA19CD","LTLA19NM","gender","age","med.radius","sample")

require(lubridate)
tbl.g.mob$date<-ymd(tbl.g.mob$date)

library(tidyr)
library(dtwclust)
library(dplyr)
library(ggplot2)
library(reshape2)
require(tseries)
require(forecast)
require(scales)
require(stringr)
require(plyr)
require(showtext)
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

tbl.l.mob<-tbl.g.mob[c("date","LTLA19CD","LTLA19NM","med.radius")]
tbl.l.mob<-tbl.l.mob[order(tbl.l.mob$LTLA19CD,tbl.l.mob$date),]

plot.whole.med<- ggplot(data=tbl.l.mob.median,aes(x=date, y=ts(daily.med)))+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  theme_bw()+
  theme(text=element_text(size=rel(7),family="notosanskr",colour="black"))+
  geom_boxplot(aes(x=date, y=daily.med,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="Daily median radius of gyration (km)",
       face="bold")+
  scale_x_date(labels=date_format("%d %b %Y"))+
  scale_y_continuous(limits=c(0.5, 3.5))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-05-11"],linetype=2, colour="red")
ggsave("plot.whole.median.national.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

##Updating the mobility metrics relative to 2020-03-03 as reference day
tbl.mob.per<-subset(tbl.l.mob,date>="2020-03-03" & date<"2020-07-01")
tbl.mob.per<-na.omit(tbl.mob.per)
tbl.mob.per<- tbl.mob.per %>% dplyr::group_by(LTLA19CD,LTLA19NM) %>% dplyr::arrange(date, .by_group = TRUE) %>% 
  dplyr::mutate(PER = ((med.radius-first(med.radius))/first(med.radius)*100)) %>% dplyr::ungroup()

require(plyr)
tbl.mob.per <-tbl.mob.per %>%
  dplyr::arrange(LTLA19CD,date) %>% 
  dplyr::group_by(LTLA19NM) %>% 
  dplyr::mutate(m.per.07 = zoo::rollmean(PER, k = 7, fill = NA)) %>% 
  dplyr::mutate(m.per.14 = zoo::rollmean(PER, k = 14, fill = NA)) %>% 
  dplyr::mutate(m.per.21 = zoo::rollmean(PER, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

tbl.l.mob.per.median<-ddply(tbl.mob.per,.(date),summarise,daily.med=median(PER))
plot.whole.med.per<- ggplot(data=tbl.l.mob.per.median,aes(x=date, y=ts(daily.med)))+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  theme_bw()+
  theme(text=element_text(size=rel(7),family="notosanskr",colour="black"))+
  geom_boxplot(aes(x=date, y=daily.med,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="Median mobility reduction compared to the baseline (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b %Y"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-05-11"],linetype=2, colour="red")
ggsave("plot.rel.median.national.Jul.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

saveRDS(tbl.mob.per,"input.mob.data.rda")
rm(list = ls())