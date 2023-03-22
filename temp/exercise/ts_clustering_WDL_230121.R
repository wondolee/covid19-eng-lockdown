###time-sereis clsutering using a hierarchial clustering algorithm using dynamic time warping distance measure
###Written by Won Do Lee, 23 Jan 2021
##Environemntal settings
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/S-DoT/TS") ##please update your working space.

##Origianl data retained from Oxford COVID-19 Impact Monitor project webpage
#blob:https://www.oxford-covid-19.com/f2a10aaa-5fb8-4fed-93d1-a90d63bfaa4b, download as "gyration.csv"

##Data cleaning - please match the format for Seoul ambinet population data
mob.data<-read.csv("gyration.csv");colnames(mob.data)<-c("CCG19NM","CCG19CD","date","med.radius")
mob.data$date<-as.POSIXlt(mob.data$date,format=c('%B %d, %Y'))
mob.eng.data<-mob.data[grepl("E38",mob.data$CCG19CD),] #grab only England data
mob.eng.data<-droplevels(mob.eng.data)
mob.eng.data<-mob.eng.data[order(mob.eng.data$CCG19CD,mob.eng.data$date),]
#mob.eng.data contains mobility metric measure among the areal code, name, and dates                     

##FYI - to noramalise the value
require(BBmisc)
mob.eng.data.nom<-normalize(mob.eng.data,method="standardize")

##Relative measure compared to the baseline - here, baseline is the first day of dataset, e.g. 2020-01-01.
require(dplyr)
mob.eng.data<-mob.eng.data %>% dplyr::arrange(CCG19CD, date) %>%
  dplyr::group_by (CCG19CD) %>% 
  dplyr::mutate(PER = ((med.radius-first(med.radius))/first(med.radius)*100)) %>% 
  dplyr::ungroup()
##FYI: this dataset already have updated in the relative measure, so this process is not necessary.

##Rolling-average option for relative measure
mob.eng.data<-mob.eng.data %>% dplyr::arrange(CCG19CD, date) %>% dplyr::group_by (CCG19CD) %>%
  dplyr::mutate(m.per.07 = zoo::rollmean(med.radius, k = 7, fill = NA)) %>% 
  dplyr::mutate(m.per.14 = zoo::rollmean(med.radius, k = 14, fill = NA)) %>% 
  dplyr::mutate(m.per.21 = zoo::rollmean(med.radius, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

##Time-series clustering 
require(dtwclust)

#Updating the noramlised DTW measure as 'nDTW' just in case
ndtw <- function(x, y, ...) {
  dtw(x, y, ...,
      step.pattern = asymmetric,
      distance.only = TRUE)$normalizedDistance
}

proxy::pr_DB$set_entry(FUN = ndtw, names = c("nDTW"),
                       loop = TRUE, type = "metric", distance = TRUE,
                       description = "Normalized, asymmetric DTW")

##Covert data frame to list format excluding null value for rolling average option
input.list.per<-as.list(utils::unstack(mob.eng.data, PER~CCG19CD))
input.list.per.07<-as.list(utils::unstack(mob.eng.data[complete.cases(mob.eng.data$m.per.07), ], m.per.07~CCG19CD))
input.list.per.14<-as.list(utils::unstack(mob.eng.data[complete.cases(mob.eng.data$m.per.14), ], m.per.14~CCG19CD))
input.list.per.21<-as.list(utils::unstack(mob.eng.data[complete.cases(mob.eng.data$m.per.21), ], m.per.21~CCG19CD))

##DTW disatnce configuration
cfg.dtw <- compare_clusterings_configs("h",k = 2L:20L, 
                                       controls = list(hierarchical = hierarchical_control(method = "all", symmetric = TRUE)
                                                       ),distances = pdc_configs("d", dtw = list())
                                       )

cfg.ndtw <- compare_clusterings_configs("h",k = 2L:20L, 
                                       controls = list(hierarchical = hierarchical_control(method = "all", symmetric = TRUE)
                                       ),distances = pdc_configs("d", nDTW = list())
)

##CVI - cluster validity index - using Silhouette index 
#Reference for SI (Arbelaitz, O., Gurrutxaga, I., Muguerza, J., Pérez, J.M., Perona, I., 2013. An extensive comparative study of cluster validity indices. Pattern Recognit. 46, 243–256. https://doi.org/10.1016/j.patcog.2012.07.021)
evaluator <- cvi_evaluators("Sil")

##Identifying the (optimised) clusters using a hierarchial clustering algorithm with DTW measure, compared to the SI 
com.ts.cl.per <- compare_clusterings(input.list.per, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
com.ts.cl.per.07 <- compare_clusterings(input.list.per.07, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
com.ts.cl.per.14 <- compare_clusterings(input.list.per.14, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
com.ts.cl.per.21 <- compare_clusterings(input.list.per.21, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)

##Optimal cluster
best.per <- repeat_clustering(input.list.per, com.ts.cl.per, com.ts.cl.per$pick$config_id)
best.per.07 <- repeat_clustering(input.list.per.07, com.ts.cl.per.07, com.ts.cl.per.07$pick$config_id)
best.per.14 <- repeat_clustering(input.list.per.14, com.ts.cl.per.14, com.ts.cl.per.14$pick$config_id)
best.per.21 <- repeat_clustering(input.list.per.21, com.ts.cl.per.21, com.ts.cl.per.21$pick$config_id)

##Plot the optimal cluster by each relative measure
plot.per<-plot(best.per,type="sc")
plot.per.07<-plot(best.per.07,type="sc")
plot.per.14<-plot(best.per.14,type="sc")
plot.per.21<-plot(best.per.21,type="sc")

##Combine the CVIs by selected optimised clusters by each relative measure
require(plyr)
max.ts.clst<-join_all(list(com.ts.cl.per$pick,com.ts.cl.per.07$pick,
                           com.ts.cl.per.14$pick,com.ts.cl.per.21$pick),type="full")

##Report the clusering ouputs as rda format, in case of m.per.14 as 4 group membership 
c.per.14.h04<-as.data.frame(cutree(best.per.14, k=4L))
c.per.14.h04 <- tibble::rownames_to_column(c.per.14.h04, "CCG19CD")
colnames(c.per.14.h04)<- c("CCG19CD","Clusters")
saveRDS(c.per.14.h04,"c.per.14.h04.dtw.rda")

##Creating the temporal trends plot 
require(ggplot2)
require(RColorBrewer)
require(classInt)
require(scales)

graph.data<-left_join(mob.eng.data,c.per.14.h04,by="CCG19CD")
graph.data$Clusters<-as.factor(graph.data$Clusters)
graph.data$date<-ymd(graph.data$date)
saveRDS(graph.data,"graph.data.rda")

plot.trend<- ggplot(data=graph.data,aes(x=date, y=ts(med.radius),fill=Clusters))+
  geom_boxplot(aes(x=date, y=med.radius,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="Mobility reduction compared to the baseline (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 50))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=graph.data$date[graph.data$date=="2020-11-05"],linetype=2, colour="red")+ #annotate the specific data as Second lockdown begin
  facet_grid(~Clusters)
ggsave("temp.trend.clu.per.14.png", width=500, height=200, units = "mm", dpi = 300, bg = "white")

##Creating the spatial distrubtion map - later on.