##Basic parameter for workspace
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/09_IJGS SI/DATA_REVISION/PROCESS/3rd_RESULT/rel/rel1/BASELINE_7DAYS/")

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

tbl.l.mob<-tbl.g.mob[c("date","LTLA19CD","LTLA19NM","med.radius")]
tbl.l.mob<-tbl.l.mob[order(tbl.l.mob$LTLA19CD,tbl.l.mob$date),]

require(plyr)

##Updating the mobility metrics relative to 2020-03-03 as reference day
tbl.mob.per.re<-subset(tbl.l.mob,date>="2020-02-27" & date<"2020-07-01")
tbl.mob.per.re<-na.omit(tbl.mob.per.re)
tbl.mob.per.re<-tbl.mob.per.re %>% dplyr::arrange(desc(LTLA19CD,date)) %>% 
  dplyr::group_by(LTLA19NM) %>%
  dplyr::mutate(RE.BASE = zoo::rollmean(med.radius,k=7,fill=NA)) %>%
  dplyr::ungroup()

rel.roll.7.base<-subset(tbl.mob.per.re,date=="2020-03-03")

tbl.mob.per.re<-subset(tbl.mob.per.re,date>="2020-03-03")
tbl.mob.per.re.pro<- tbl.mob.per.re %>% dplyr::group_by(LTLA19CD,LTLA19NM) %>% dplyr::arrange(date, .by_group = TRUE) %>% 
  dplyr::mutate(PER = ((med.radius-first(RE.BASE))/first(RE.BASE)*100)) %>% dplyr::ungroup()

tbl.mob.per.re.pro <-tbl.mob.per.re.pro %>%
  dplyr::arrange(desc(LTLA19CD,date)) %>% 
  dplyr::group_by(LTLA19NM) %>% 
  dplyr::mutate(m.per.re.07 = zoo::rollmean(PER, k = 7, fill = NA)) %>% 
  dplyr::mutate(m.per.re.14 = zoo::rollmean(PER, k = 14, fill = NA)) %>% 
  dplyr::mutate(m.per.re.21 = zoo::rollmean(PER, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

tbl.l.mob.per.median.re<-ddply(tbl.mob.per.re.pro, .(date),summarise,daily.med=median(PER))
plot.whole.med.per.re.Jul<- ggplot(data=subset(tbl.l.mob.per.median.re,date<"2020-07-01"),aes(x=date, y=ts(daily.med)))+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  theme_bw()+
  geom_boxplot(aes(x=date, y=daily.med,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="14-day rolling average of mobility reduction compared to the baseline week (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-05-11"],linetype=2, colour="red")
ggsave("plot.rel.baseweek.median.national.Jul.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

plot.whole.med.per.re.Jun<- ggplot(data=subset(tbl.l.mob.per.median.re,date>="2020-03-03"&date<"2020-05-31"),aes(x=date, y=ts(daily.med)))+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  theme_bw()+
  geom_boxplot(aes(x=date, y=daily.med,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="14-day rolling average of mobility reduction compared to the baseline week (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=tbl.l.mob$date[tbl.l.mob$date=="2020-05-11"],linetype=2, colour="red")
ggsave("plot.rel.baseweek.median.national.Jun.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

write.csv(tbl.l.mob.per.median.re,"baseline.week.median.mob.rel.measure.baseweek.csv")

##Should be excluding the Isle of Sicilly
dyw.input.mob.re<-as.data.frame(tbl.mob.per.re.pro)
dyw.input.mob.re<-subset(dyw.input.mob.re,date>="2020-03-23" & date<"2020-05-12") #23 March 2020 to 11 May 2020 
dyw.input.mob.re<-subset(dyw.input.mob.re,LTLA19CD!="E06000053")
dyw.input.mob.re<-droplevels(dyw.input.mob.re) ##Excludin

df_list_mob_g_re <- as.list(utils::unstack(dyw.input.mob.re, PER ~ LTLA19CD))
df_list_mob_g_re.07 <- as.list(utils::unstack(dyw.input.mob.re, m.per.re.07 ~ LTLA19CD))
df_list_mob_g_re.14 <- as.list(utils::unstack(dyw.input.mob.re, m.per.re.14 ~ LTLA19CD))
df_list_mob_g_re.21 <- as.list(utils::unstack(dyw.input.mob.re, m.per.re.21 ~ LTLA19CD))

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
comparison.per.re <- compare_clusterings(df_list_mob_g_re, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.re.07 <- compare_clusterings(df_list_mob_g_re.07, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.re.14 <- compare_clusterings(df_list_mob_g_re.14, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)
comparison.per.re.21 <- compare_clusterings(df_list_mob_g_re.21, "h", cfg.dtw,score.clus = evaluator$score,pick.clus = evaluator$pick)

best.per.re <- repeat_clustering(df_list_mob_g_re, comparison.per.re, comparison.per.re$pick$config_id)
best.per.re.07 <- repeat_clustering(df_list_mob_g_re.07, comparison.per.re.07, comparison.per.re.07$pick$config_id)
best.per.re.14 <- repeat_clustering(df_list_mob_g_re.14, comparison.per.re.14, comparison.per.re.14$pick$config_id)
best.per.re.21 <- repeat_clustering(df_list_mob_g_re.21, comparison.per.re.21, comparison.per.re.21$pick$config_id)

p.per.re<-plot(best.per.re,type="sc")
p.per.re.07<-plot(best.per.re.07,type="sc")
p.per.re.14<-plot(best.per.re.14,type="sc")
p.per.re.21<-plot(best.per.re.21,type="sc")

require(plyr)
max.ts.clst.m.per.re.14<-join_all(list(comparison.per.re$pick,comparison.per.re.07$pick,
                                    comparison.per.re.14$pick,comparison.per.re.21$pick),type="full")


require(dplyr)
h04.per.re<-as.data.frame(cutree(best.per.re, k=4L))
h04.per.re <- tibble::rownames_to_column(h04.per.re, "LTLA19CD")
colnames(h04.per.re)<- c("LTLA19CD","CLU")
write.csv(h04.per.re,"h04.per.re.dtw.csv")

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

ltla<-left_join(ltla,h04.per.re,by=c("LTLA19CD"))
ltla$CLU<-as.factor(ltla$CLU)
ltla<-na.omit(ltla)
saveRDS(ltla,"ltla.h04.per.re.dtw.rda")

map.dtw.h04.re<-ggplot(ltla)+geom_sf(aes(fill=CLU))+
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
ggsave("map.dtw.h04.per.re.png", width=400, height=400, units = "mm", dpi = 300, bg = "white")

graph.data.re<-left_join(tbl.mob.per.re.pro,h04.per.re,by="LTLA19CD")
graph.data.re$Cluster<-as.factor(graph.data.re$CLU)

plot.trend.Jul.re<- ggplot(data=graph.data.re,aes(x=date, y=ts(m.per.re.14),fill=Cluster))+
  theme_bw()+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  geom_boxplot(aes(x=date, y=m.per.re.14,group=date),colour="gray50",outlier.shape=0.2)+
  labs(x="Day",y="14-day rolling average of mobility reduction compared to the baseline week(%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-05-11"],linetype=2, colour="red")+
  facet_grid(~CLU)
ggsave("plot.temp.trend.m.14.per.re.Jul.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

plot.trend.per.re.Jun<- ggplot(data=subset(graph.data.re,date>="2020-03-03"&date<"2020-05-31"),aes(x=date, y=ts(m.per.re.14),fill=Cluster))+
  theme_bw()+
  #geom_line(color="grey20",size=0.02,alpha=0.4)+
  #geom_boxplot(aes(x=date, y=m.per.14,group=date),colour="gray",outlier.shape=0.1)+
  labs(x="Day",y="14-day rolling average of mobility reduction compared to the baseline week (%)",face="bold")+
  scale_x_date(labels=date_format("%d %b"))+
  scale_y_continuous(limits=c(-100, 0))+
  geom_smooth(method ="loess",span=0.2,fill="gray80")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-03-03"],linetype=2, colour="black")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-03-23"],linetype=2, colour="red")+
  geom_vline(xintercept=graph.data.re$date[graph.data.re$date=="2020-05-11"],linetype=2, colour="red")+
  facet_grid(~CLU)
ggsave("plot.temp.trend.m.14.per.re.Jun.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

require(gridExtra)
plot.whole.per.re<-grid.arrange(plot.whole.med.per.re.Jun,plot.trend.per.re.Jun,ncol=2)
ggsave("both.per.re.png", width=300, height=200, units = "mm", dpi = 300, bg = "white",plot.whole.per.re)

graph.data.re$week<-week(graph.data.re$date)
graph.data.re$day<-wday(graph.data.re$date)
graph.data.re<-na.omit(graph.data.re)

require(dplyr)
m.per.14.overall<-as.data.frame(graph.data.re%>% group_by(week) %>% summarise(Mean=mean(m.per.re.14),Median=median(m.per.re.14), Std=sd(m.per.re.14)))
m.per.14.clu<-as.data.frame(graph.data.re %>% group_by(CLU,week)%>% 
                             summarise(Mean=mean(m.per.re.14),Median=median(m.per.re.14), Std=sd(m.per.re.14)))
m.per.14.clu.median<-ddply(graph.data.re,.(CLU,date),summarise,PER=median(PER),PER.07=median(m.per.re.07),PER.14=median(m.per.re.14),
                           PER.21=median(m.per.re.21))
write.csv(m.per.14.clu.median,"median,per.changes.by.date.clu.baseweek.csv")
#m.mob.14.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(m.mob.14),Median=median(m.mob.14), Std=sd(m.mob.14)))
#m.mob.14.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(m.mob.14),Median=median(m.mob.14), Std=sd(m.mob.14)))
#m.per.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(PER),Median=median(PER), Std=sd(PER)))
#m.per.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(PER),Median=median(PER), Std=sd(PER)))
#m.mob.overall<-as.data.frame(graph.data%>% group_by(week) %>% summarise(Mean=mean(med.radius),Median=median(med.radius), Std=sd(med.radius)))
#m.mob.clu<-as.data.frame(graph.data%>% group_by(CLU,week)%>% summarise(Mean=mean(med.radius),Median=median(med.radius), Std=sd(med.radius)))

##independent variable
indep.var<-readRDS("D:/OneDrive - Nexus365/WORK/Document/RESEARCH/COVID19/09_IJGS SI/DATA_REVISION/PROCESS/TEMP/Cuebiq_110121/REL/indep.var.rda")
input.data<-left_join(ltla,indep.var,by=c("LTLA19CD","LTLA19NM"))

##Descriptive analysis of cluster memberships - min, max, average and median value of daily median radius of gyration
require(dplyr)
des.m.14.min<-graph.data.re %>% arrange(CLU, PER) %>% group_by(CLU) %>% slice(1:3)
des.m.14.max<-graph.data.re %>% arrange(CLU, desc(PER)) %>% group_by(CLU) %>% slice(1:3)

write.csv(des.m.14.min,"min.per.by.cluster.baseweek.csv")
write.csv(des.m.14.max,"max.per.by.cluster.baseweek.csv")

require(psych)
des.avg.by.clu<-describe.by(as.data.frame(input.data),group=input.data$CLU)
des.avg<-input.data %>% summarise(across(everything(), mean))

##MNL
require(MASS)
require(nnet)
f.cl=CLU~1+scale(INC_80)+scale(INC_20)+scale(SG_C1)+scale(SG_AB)+scale(T.03)+
  scale(res.den)+scale(park.per)+scale(hospital.per)+
  scale(self_employed)+scale(H.G)+scale(H.B)+
  scale(E.IN)+scale(E.PA)+scale(E.AF)+scale(E.CAR)

f.cl.re1= CLU ~ 1+ scale(INC_20) + scale(SG_C1) + scale(hospital.per) + scale(self_employed) + 
  scale(H.G) + scale(E.CAR)

f.cl.re2= CLU ~ 1+scale(INC_20) + scale(SG_C1) + scale(hospital.per) +
  scale(self_employed) + scale(E.CAR)

f.cl.re3= CLU ~ 1+ scale(INC_20) + scale(res.den)+scale(hospital.per) + scale(self_employed) + 
  scale(H.B) + scale(E.AF) + scale(E.CAR)

f.cl.sel=CLU ~ 1+ scale(INC_20) + scale(hospital.per) + scale(self_employed) + 
  scale(H.G) + scale(E.AF) + scale(E.CAR)

ml.null<-multinom(CLU~1,data=input.data) #AIC=851.7188
ml.step<-multinom(f.cl,data=input.data) %>% stepAIC() #640.5025
ml.step.re1<-multinom(f.cl.re1,data=input.data) %>% stepAIC() #675.8764
ml.step.re2<-multinom(f.cl.re2,data=input.data) #%>% stepAIC() #678.838
ml.step.re3<-multinom(f.cl.re3,data=input.data) #%>% stepAIC() #682.7492 
ml.step.sel<-multinom(f.cl.sel,data=input.data) #%>% stepAIC() #675.8161

require(performance)
require(AER)
multicollinearity(ml.step.re1)
multicollinearity(ml.step.re2) #vif<10
coeftest(ml.step.re2)

require(mlogit)
m.data <- mlogit.data(data = input.data, shape="wide",choice = "CLU")

fit<-mlogit(CLU ~ 1| scale(INC_20) + scale(SG_C1) + scale(hospital.per) +
              scale(self_employed) + scale(E.CAR), data=m.data)
fit.ex<-mlogit(CLU ~ 1|scale(INC_20) + scale(SG_C1) + scale(hospital.per) +
                 scale(self_employed) + scale(E.CAR), alt.subset=c("2","3","4"),data=m.data)
fit.re<-mlogit(CLU ~ 1| scale(INC_20) + scale(hospital.per) + scale(self_employed) + 
                 scale(H.G) + scale(E.AF) + scale(E.CAR), data=m.data)

hmftest(fit,fit.ex)
lrtest(fit,fit.re)

coe_df <- data.frame((summary(fit)$CoefTable))
coe_df$factor <- row.names(summary(fit)$CoefTable)
coe_df$lower <- coe_df$Estimate - coe_df$Std..Error  
coe_df$upper <- coe_df$Estimate + coe_df$Std..Error 

require(ggplot2)
coe.df<-ggplot(data = coe_df) +
  geom_point(aes(factor, Estimate)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))
ggsave("coe.df.clu.per.dtw.baseweek.png", width=300, height=200, units = "mm", dpi = 300, bg = "white")

##ANOVA and Turkey's HSD
lm<-lm(scale(INC_20) + scale(SG_C1) + scale(hospital.per) +
         scale(self_employed) + scale(E.CAR)~CLU,data=input.data)
lm.aov<-aov(lm)
tukey<-TukeyHSD(lm.aov,'CLU',conf.level=0.95,data=input.data)
png("tukeyHSD.per.basewekk.png")
plot(tukey,las=1,col="brown")
dev.off()

##difference in mean by group memberships
anova.data<-input.data[c("LTLA19CD","LTLA19NM","CLU","INC_20", "res.den","hospital.per",
                         "self_employed", "H.B","E.AF","E.CAR")]
mean.by.cluster<-ddply(anova.data, .(CLU), summarise, INC_20=mean(INC_20), res.den=mean(res.den),
                       hospital.per=mean(hospital.per),self_employed=mean(self_employed),H.B()
                       e.car=mean(E.CAR), freq=length(CLU))

median.by.cluster<-ddply(anova.data, .(CLU), summarise, INC_20=median(INC_20),hospital.per=median(hospital.per),
                       self_employed=median(self_employed), e.car=median(E.CAR), freq=length(CLU))

predicted_alternative <- as.factor(apply(predict(fit,m.data),1,which.max))
selected_alternative <- input.data$CLU

require(caret)
confusionMatrix(predicted_alternative,selected_alternative)

require(mfx)
mod1<-probitmfx(CLU ~ 1+ scale(INC_20) + scale(res.den) + scale(hospital.per) + 
                  scale(self_employed) + scale(H.B) + scale(E.AF) + scale(E.CAR),data=input.data)

mod2<-probitmfx(CLU ~ 1+ scale(INC_20) + scale(res.den) + scale(hospital.per) + 
                  scale(self_employed) + scale(H.B) + scale(E.AF) + scale(E.CAR),data=input.data,atmean=TRUE)

mod3<-logitmfx(CLU ~ 1+ scale(INC_20) + scale(SG_C1) + scale(hospital.per) +
                 scale(self_employed) + scale(E.CAR),data=input.data)
rrr<-exp(coef(ml.step.sel))

require(pscl)
require(margins)
require(Zelig)
summary(ml.step.re1, cor = F)
effects<-marginal_effects(ml.step.re1)
myeffects(fit,covariate="INC_20",data=m.data)


pR2(ml.step.re1)

head(fitted(fit, type = "probabilities"), 4)
sum(log(fitted(fit, type = "outcome")))
logLik(fit)
apply(fitted(fit, type = "probabilities"), 2, mean)
effects(fit,type="rr")