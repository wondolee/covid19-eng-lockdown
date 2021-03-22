##Written by Won Do Lee (wondo.lee@ouce.ox.ac.uk) on 30 October 2020.
##This script follwed by Gov't dashbaord developer guide
##FYI: Check the latest developer guide of Gov't dashboard webpage (https://coronavirus-staging.data.gov.uk/details/developers-guide) 

##Setting the basic working envionment for your convenience
Sys.setlocale(locale="English_United Kingdom")
setwd("d:/WORKSPACE/EXERCISE/DTW_PARTITIONAL_210302/")

#Importing the packages
library(dplyr)
library(httr)
library(janitor)
library(jsonlite)
library(purrr)
library(readr)
library(stringr)

#Functions already provided by the Gov't dashboard development guidance - asked to be check the latest version if it is not working.
get_paginated_data <- function (filters, structure) {
  endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"
  results <- list()
  current_page <- 1
  repeat {
      query <- list(
      filters = paste(filters, collapse = ";"),
      structure = toJSON(structure, auto_unbox = TRUE),
      page = current_page)
      response <- GET(url = endpoint, query = query, timeout(30))
  #Handle errors
    if (response$status_code >= 400) {
      err_msg = http_status(response)
      stop(err_msg)
    } else if (response$status_code == 204) {
      break
    }
    
  #Convert response to JSON
    json_text <- content(response, "text")
    dt <- fromJSON(json_text)
    results <- rbind(results, dt$data)
    
    if (is.null( dt$pagination$`next`)) break
    current_page <- current_page + 1;
  }
  
  results %>%
    as_tibble() %>% 
    clean_names() %>% 
    mutate(date = as.Date(date)) %>% 
    arrange(area_name, date)
}

#Valid values of the areaType metric as follows - overview (whole UK), nation (England...), region, nhsregion, utla(Upper-tier LA), and ltla(Lower-tier LA)
area_types <- "nation" #"ulta" / "region" in regard to your interests
  
#Create the structure as a list of metrics
structure <- list(
    date = "date", 
    areaName = "areaName", 
    areaCode = "areaCode", 
    newCasesBySpecimenDate = "newCasesBySpecimenDate",
    cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
    cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
    newDeathsByDeathDate = "newDeathsByDeathDate",
    cumDeathsByDeathDate = "cumDeathsByDeathDate",
    cumDeaths28DaysByDeathDateRate="cumDeaths28DaysByDeathDateRate",
    newDeaths28DaysByPublishDate="newDeaths28DaysByPublishDate",
    cumDeaths28DaysByPublishDate="cumDeaths28DaysByPublishDate",
    cumDeaths28DaysByPublishDateRate="cumDeaths28DaysByPublishDateRate",
    newAdmissions = "newAdmissions", #Genrate NA (last accessed: 30 Oct 2020)
    cumAdmissions = "cumAdmissions", #NA
    covidOccuppiedMVBeds = "covidOccupiedMVBeds", #NA
    hospitalCases = "hospitalCases" #NA
    )

data <- map(area_types, function(area_type){
    filters <- str_glue("areaType={area_type}")
    results <- get_paginated_data(filters, structure)
    })


##Extracting the case across LTLAs in England
require(plyr)
data<-as.data.frame(ldply(data)) #convert list to sing data frame
colnames(data)<-c("date","NM","CD","daily.cases","cum.cases","cum.cases.rate","daily.deaths.date","cum.deaths.date",
                  "cum.deaths.date.rates","daily.deaths.pub.date","cum.deaths.pub.date","cum.deaths.pub.date.rates","daily.adminssion",
                  "cum.admission","covid.bed.occupied","hospital.cases")
en.data<-subset(data,grepl("England",NM)) #Omit the variables of admission-related attributes due to the null value

require(lubridate)
en.data$date<-ymd(en.data$date)
en.data$week<-week(en.data$date)

en.data <- en.data %>%
  dplyr::arrange(desc(date)) %>% 
  dplyr::group_by(NM,CD) %>% 
  dplyr::mutate(daily.cases.roll.07 = zoo::rollmean(daily.cases, k = 7, fill = NA),
                daily.cases.roll.14 = zoo::rollmean(daily.cases, k = 14, fill = NA),
                daily.deaths.date.roll.07 = zoo::rollmean(daily.deaths.date, k = 7, fill = NA),
                daily.deaths.date.roll.14 = zoo::rollmean(daily.deaths.date, k = 14, fill = NA),
                daily.deaths.pub.roll.07 = zoo::rollmean(daily.deaths.pub.date, k = 7, fill = NA),
                daily.deaths.pub.roll.14 = zoo::rollmean(daily.deaths.pub.date, k = 14, fill = NA)) %>% 
  dplyr::ungroup()

en.data<-en.data[order(en.data$date), ]

knitr::opts_chunk$set(echo = TRUE)
library(dtwclust)
library(TTR)
library(quantmod)
library(jsonlite)
library(changepoint)

data.covid<-xts(en.data[c(4)],order.by=en.data$date)
data.covid.death<-xts(en.data[c(7)],order.by=en.data$date)
plot(data.covid)
plot(data.covid.death)

data.covid.death<-na.omit(data.covid.death)

ZigZag(data.covid.death,change = 2)->zigzag ##removing the changes below some threshold (3% in here)
plot(zigzag)

m2.data<-as.numeric(zigzag)[1:364]
m2.pelt <- cpt.meanvar(m2.data, method = "PELT",penalty = "MBIC")
plot(m2.data,type='l')
abline(v=m2.pelt@cpts,col="blue")
cpts(m2.pelt)
timesplits<-map2(c(1,cpts(m2.pelt)),c(cpts(m2.pelt),length(m2.data)), function(x,y) m2.data[seq(x,y)])
timesplits <- reinterpolate(timesplits, new.length = max(lengths(timesplits)))
hc <- tsclust(timesplits, type = "partitional", k = length(timesplits)/3, 
              distance = "dtw2", trace = TRUE, centroid = "shape")

library(EnvCpt)
fit_envcpt = envcpt(na.omit(en.data$daily.deaths.date))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)

#plot(hc)
plot(hc,type="series")

sen.data.re<-subset(en.data,date>="2020-03-22" & date<="2020-05-12") #temporal sequences (i.e. 22 March to 11 May

en.data.overview<-ddply(en.data,.(date),summarise,daily.cases=sum(daily.cases,na.rm=TRUE),
                        daily.deaths.date=sum(daily.deaths.date,na.rm=TRUE),daily.deaths.pub.date=sum(daily.deaths.pub.date,na.rm=TRUE))
en.data.overview <- en.data.overview %>%
  dplyr::arrange(desc(date)) %>% 
  dplyr::mutate(daily.cases.roll.07 = zoo::rollmean(daily.cases, k = 7, fill = NA),
                daily.cases.roll.14 = zoo::rollmean(daily.cases, k = 14, fill = NA),
                daily.deaths.date.roll.07 = zoo::rollmean(daily.deaths.date, k = 7, fill = NA),
                daily.deaths.date.roll.14 = zoo::rollmean(daily.deaths.date, k = 14, fill = NA),
                daily.deaths.pub.roll.07 = zoo::rollmean(daily.deaths.pub.date, k = 7, fill = NA),
                daily.deaths.pub.roll.14 = zoo::rollmean(daily.deaths.pub.date, k = 14, fill = NA)) %>% 
  dplyr::ungroup()
en.data.overview.re<-subset(en.data.overview,date>="2020-03-22" & date<="2020-05-12") #temporal sequences (i.e. 22 March to 11 May