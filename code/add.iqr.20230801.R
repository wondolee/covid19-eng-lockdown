rm(list=ls())
setwd("d:/WORKSPACE/GIT/covid19-eng-lockdown/process/")
mob.data<-readRDS("input.mob.data.rda")
cluster<-read.csv("h04.per.dtw.csv")

require(dplyr)
tbl.mob.data<-left_join(mob.data,cluster,by=c("LTLA19CD"))
tbl.mob.data$month<-lubridate::month(tbl.mob.data$date)

avg.mob.red<-tbl.mob.data %>% dplyr::group_by(CLU,month) %>%
  dplyr::summarise(avg.red=mean(PER),q1.red=quantile(PER,probs=0.25),
                   q3.red=quantile(PER,probs=0.75),IQR=q3.red-q1.red) %>%
  dplyr::ungroup()

require(reshape2)
cast.avg.red.mob.clu<-dcast(avg.mob.red %>% select(CLU,month,avg.red),
                       month~CLU, value.var =c("avg.red"))

write.csv(cast.avg.red.mob.clu,"table1.clu.by.avg.csv",row.names=FALSE)

cast.IQR.mob.clu<-dcast(avg.mob.red %>% select(CLU,month,IQR),
                            month~CLU, value.var =c("IQR"))
write.csv(cast.IQR.mob.clu,"table1.clu.by.IQR.csv",row.names=FALSE)

total.mob.red<-tbl.mob.data %>% dplyr::group_by(month) %>%
  dplyr::summarise(avg.red=mean(PER),q1.red=quantile(PER,probs=0.25),
                   q3.red=quantile(PER,probs=0.75),IQR=q3.red-q1.red) %>%
  dplyr::ungroup()
write.csv(total.mob.red,"table1.total.avg.IQR.csv",row.names=FALSE)
