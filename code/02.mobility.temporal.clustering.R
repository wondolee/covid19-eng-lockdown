##Basic parameter for workspace
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/09_IJGS SI/FINAL_CODE/")

tbl.mob.per<-readRDS("input.mob.data.rda")

##Should be excluding the Isle of Sicilly
dyw.input.mob<-as.data.frame(tbl.mob.per)
dyw.input.mob<-subset(dyw.input.mob,date>="2020-03-23" & date<"2020-05-12") #23 March 2020 to 11 May 2020 
dyw.input.mob<-subset(dyw.input.mob,LTLA19CD!="E06000053")
dyw.input.mob<-droplevels(dyw.input.mob) ##Excluding the Isle of Sicilly

df_list_mob_g <- as.list(utils::unstack(dyw.input.mob, PER ~ LTLA19CD))
df_list_mob_g.07 <- as.list(utils::unstack(dyw.input.mob, m.per.07 ~ LTLA19CD))
df_list_mob_g.14 <- as.list(utils::unstack(dyw.input.mob, m.per.14 ~ LTLA19CD))
df_list_mob_g.21 <- as.list(utils::unstack(dyw.input.mob, m.per.21 ~ LTLA19CD))

##DTW disatnce
cfg.dtw <- compare_clusterings_configs(
  "h",
  k = 2L:20L,
  controls = list(
    hierarchical = hierarchical_control(method = "all", symmetric = TRUE)
  ),
  distances = pdc_configs("d", dtw = list())
)


evaluator <- cvi_evaluators("Sil")
comparison.per <- compare_clusterings(df_list_mob_g, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.07 <- compare_clusterings(df_list_mob_g.07, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.14 <- compare_clusterings(df_list_mob_g.14, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.21 <- compare_clusterings(df_list_mob_g.21, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)

best.per <- repeat_clustering(df_list_mob_g, comparison.per, comparison.per$pick$config_id)
best.per.07 <- repeat_clustering(df_list_mob_g.07, comparison.per.07, comparison.per.07$pick$config_id)
best.per.14 <- repeat_clustering(df_list_mob_g.14, comparison.per.14, comparison.per.14$pick$config_id)
best.per.21 <- repeat_clustering(df_list_mob_g.21, comparison.per.21, comparison.per.21$pick$config_id)

p.per<-plot(best.per,type="sc")
p.per.07<-plot(best.per.07,type="sc")
p.per.14<-plot(best.per.14,type="sc")
p.per.21<-plot(best.per.21,type="sc")

require(plyr)
max.ts.clst.m.per.14<-join_all(list(comparison.per$pick,comparison.per.07$pick,
                                    comparison.per.14$pick,comparison.per.21$pick),type="full")

require(dplyr)
h04.per<-as.data.frame(cutree(best.per, k=4L))
h04.per <- tibble::rownames_to_column(h04.per, "LTLA19CD")
colnames(h04.per)<- c("LTLA19CD","CLU")
write.csv(h04.per,"h04.per.dtw.csv")
saveRDS(h04.per,"mobility.temporal.cluster.rda")

require(sf)
require(ggplot2)
require(RColorBrewer)
require(classInt)
require(maptools)
require(ggrepel)
require(scales)
require(rgdal)
require(ggspatial)
require(extrafont)
loadfonts(device = "win")

ltla<-st_read("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/09_IJGS SI/DATA_REVISION/PROCESS/en.ltla.shp")
eng.la<-st_read("D:/WORKSPACE/COVID19/REVISE_LAST/ENGLAND_REGIONS.shp")
eng.la.coord <- sf::st_point_on_surface(eng.la)
la.coords <- as.data.frame(sf::st_coordinates(eng.la.coord))
la.coords$NAME <- eng.la.coord$RGN11NM

ltla<-left_join(ltla,h04.per,by=c("LTLA19CD"))
ltla$CLU<-as.factor(ltla$CLU)
ltla<-na.omit(ltla)
saveRDS(ltla,"ltla.h04.per.dtw.rda")

map.dtw.h04<-ggplot(ltla)+geom_sf(aes(fill=CLU))+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  #scale_fill_manual(values = gi.map,name="Gi* values")+
  theme_bw()+
  theme(legend.position = "bottom",plot.title = element_text(size =30, face = "bold"),
        legend.title=element_text(size=25), 
        legend.text=element_text(size=25))+
  annotation_scale(location = "br", height = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  geom_sf(data=eng.la,fill=NA,color="black",size=1,show.legend=FALSE)+
  geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   colour = "Black",size=10,family="Raleway",fontface="bold",
                   box.padding = unit(1, "lines"),
                   point.padding = unit(1, "lines"),
                   segment.color = 'grey50')
ggsave("map.dtw.h04.per.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

graph.data<-left_join(tbl.mob.per,h04.per,by="LTLA19CD")
graph.data$Cluster<-as.factor(graph.data$CLU)
#graph.data<-subset(graph.data,date>="2020-03-23" & date<"2020-05-11")

plot.trend.Jul<- ggplot(data=graph.data,aes(x=date, y=ts(m.per.14),fill=Cluster))+
  theme_bw()+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  geom_boxplot(aes(x=date, y=m.per.14,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="14-day rolling average of mobility reduction compared to the baseline (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-05-11"],linetype=2, colour="red")+
  facet_grid(~CLU)
ggsave("plot.temp.trend.m.14.per.Jul.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

require(gridExtra)
plot.whole<-grid.arrange(plot.whole.med.per.jun,plot.trend.Jun,ncol=2)
ggsave("both.per.png", width=300, height=200, units = "mm", dpi = 300, bg = "white",plot.whole)

graph.data$week<-week(graph.data$date)
graph.data$day<-wday(graph.data$date)
graph.data<-na.omit(graph.data)

require(dplyr)
m.per.14.overall<-as.data.frame(graph.data %>% group_by(week) %>% summarise(Mean=mean(m.per.14),Median=median(m.per.14), Std=sd(m.per.14)))
m.per.14.clu<-as.data.frame(graph.data %>% group_by(CLU,week)%>% 
                              summarise(Mean=mean(m.per.14),Median=median(m.per.14), Std=sd(m.per.14)))
m.per.14.clu.median<-ddply(graph.data,.(CLU,date),summarise,PER=median(PER),PER.07=median(m.per.07),PER.14=median(m.per.14),
                           PER.21=median(m.per.21))
write.csv(m.per.14.clu.median,"median,per.changes.by.date.clu.csv")
#m.mob.14.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(m.mob.14),Median=median(m.mob.14), Std=sd(m.mob.14)))
#m.mob.14.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(m.mob.14),Median=median(m.mob.14), Std=sd(m.mob.14)))
#m.per.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(PER),Median=median(PER), Std=sd(PER)))
#m.per.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(PER),Median=median(PER), Std=sd(PER)))
#m.mob.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(med.radius),Median=median(med.radius), Std=sd(med.radius)))
#m.mob.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(med.radius),Median=median(med.radius), Std=sd(med.radius)))
