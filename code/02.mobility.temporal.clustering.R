##Basic parameter for workspace
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/GIT/covid19-eng-lockdown/process/")
data.path<-"d:/WORKSPACE/GIT/covid19-eng-lockdown/data/"
tbl.mob.per<-readRDS(paste0(data.path,"input.mob.data.rda"))

require(future)
plan(multisession)
availableCores() #20
plan(multisession, workers = 12)

require(dtwclust)
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
write.csv(h04.per,"h04.per.dtw.csv")

h04.per<-readRDS(paste0(data.path,"mobility.temporal.cluster.rda"))
colnames(h04.per)<- c("LTLA19CD","CLU")
h04.per$CLU<-as.factor(h04.per$CLU)
levels(h04.per$CLU)<-c("G4","G3","G1","G2")
h04.per$CLU<-factor(h04.per$CLU,
                    levels=c("G1","G2","G3","G4"))
#saveRDS(h04.per,"mobility.temporal.cluster.rda")

require(sf)
require(ggplot2)
require(RColorBrewer)
require(classInt)
require(maptools)
require(ggrepel)
require(scales)
require(rgdal)
require(ggspatial)
require(showtext)
font_add_google("Noto Sans KR", "notosanskr")
showtext_auto()

ltla<-st_read(paste0(data.path,"en.ltla.shp"))
eng.la<-st_read(paste0(data.path,"ENGLAND_REGIONS.shp"))
eng.la.coord <- sf::st_point_on_surface(eng.la)
la.coords <- as.data.frame(sf::st_coordinates(eng.la.coord))
la.coords$NAME <- eng.la.coord$RGN11NM

ltla<-left_join(ltla,h04.per,by=c("LTLA19CD"))
ltla<-na.omit(ltla)
#st_write(ltla,"ltla.h04.per.dtw.geojson")

cols<-c("G1"="#ca0020","G2"="#f4a582","G3"="#92c5de","G4"="#0571b0")

map.dtw.h04<-ggplot()+
  geom_sf(data=ltla,aes(fill=CLU),linewidth=0.2,colour="gray60")+
  #scale_fill_brewer(palette = "OrRd",labels=c("Not Sig.","Hot spot","Cold spot"))
  scale_fill_manual(values=cols,drop=FALSE)+
  theme_bw()+
  theme(text=element_text(size=rel(6), family="notosanskr",),
        plot.title = element_text(size =rel(5), family="notosanskr",
                                  face = "bold"),
        legend.title=element_text(size =rel(5), family="notosanskr",
                                  face = "bold"), 
        legend.text=element_text(size =rel(5), family="notosanskr"),
        legend.position= "bottom")+
        annotation_scale(location = "br", height = unit(0.5, "cm")) +
        annotation_north_arrow(location = "tl", 
                         style = north_arrow_nautical, 
                         height = unit(2, "cm"),
                         width = unit(2, "cm"))+
  labs(fill="Cluster",x=NULL,y=NULL)+
  geom_sf(data=eng.la,fill=NA,color="black",linewidth=0.3,
          show.legend=FALSE)+
  geom_label_repel(data=la.coords, aes(X, Y, label = NAME),
                   colour = "Black",size=rel(15),
                   family="notosanskr",
                   fontface="bold",
                   alpha=.7,
                   segment.color = 'gray50')
ggsave("map.dtw.h04.per.png", width=200, height=200, 
       units = "mm", dpi = 300, bg = "white")

freq.clu.by.region<-plyr::count(ltla,c("REG_NM","CLU"))
write.csv(freq.clu.by.region,"freq.clu.by.region.csv",row.names=FALSE,
          fileEncoding="UTF-8")

graph.data<-left_join(tbl.mob.per,h04.per,by="LTLA19CD")
graph.data$Cluster<-as.factor(graph.data$CLU)
#graph.data<-subset(graph.data,date>="2020-03-23" & date<"2020-05-11")

plot.trend.Jul<- ggplot(data=graph.data,aes(x=date, y=ts(m.per.07),
                                            fill=CLU))+
  theme_bw()+
  theme(text=element_text(size=rel(6.5), family="notosanskr"),
        plot.title = element_text(size =rel(5), family="notosanskr",
                                  face = "bold"),
        legend.title=element_text(size =rel(5), family="notosanskr",
                                  face = "bold"), 
        legend.text=element_text(size =rel(5), family="notosanskr",
                                 face = "bold"),
        strip.text.x = element_text(size =rel(10), family="notosanskr",
        face = "bold"),
        legend.position= "bottom")+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  geom_boxplot(aes(x=date, y=m.per.07,group=date),
               colour="gray50",alpha=.8,outlier.shape=0.1,linewidth=0.1)+
  scale_fill_manual(values=cols,drop=FALSE)+
  labs(x="Day",y="7-day rolling average of mobility reduction compared to the baseline (%)",
       fill="Cluster", face="bold",size=rel(15), family="notosanskr",)+
  scale_x_date(labels=date_format("%d %b"),breaks="2 months")+
  scale_y_continuous(limits=c(-100, 25))+
  geom_smooth(method ="loess",span=0.2,colour="black",linewidth=0.5,
              se=TRUE)+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-05-11"],linetype=2, colour="red")+
  facet_grid(~CLU)
ggsave("plot.temp.trend.m.7.per.Jul.png", width=150, height=100, 
       scale=2,units = "mm", dpi = 300, bg = "white")

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
