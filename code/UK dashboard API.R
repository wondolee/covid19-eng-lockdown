##Written by Won Do Lee (wondo.lee@ouce.ox.ac.uk) on 30 October 2020.
##This script written by follwing the instruction of UK Gov't dashbaord developer guide
##Please check the latest developer guide of UK Gov't dashboard webpage: https://coronavirus-staging.data.gov.uk/details/developers-guide

#Importing packages
require(dplyr)
require(httr)
require(janitor)
require(jsonlite)
require(purrr)
require(readr)
require(stringr)

#Functions followed by UK Gov't dashboard development guidance - should be check the latest version if it is not working.
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
    
    # Handle errors
    if (response$status_code >= 400) {
      err_msg = http_status(response)
      stop(err_msg)
    } else if (response$status_code == 204) {
      break
    }
    
    # Convert response to JSON
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
area_types <- "ltla" #"ulta" / "region" in regard to your interests
  
#Create the structure as a list of metrics; check a valid metrics for structure (https://coronavirus.data.gov.uk/details/developers-guide#methods-get)
structure <- list(
    date = "date", 
    areaName = "areaName", 
    areaCode = "areaCode", 
    newCasesBySpecimenDate = "newCasesBySpecimenDate", #New cases by publish date
    cumCasesBySpecimenDate = "cumCasesBySpecimenDate", #Cumulative cases by publish date
    cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate", #Rate of cumulative cases by publish date per 100k resident population
	maleCases= "maleCases", #Male cases (by age)
	femaleCases = "femaleCases", #Female cases (by age)
    newDeathsByDeathDate = "newDeathsByDeathDate", #New cases by specimen date
    newAdmissions = "newAdmissions", #New hospitalisation admissions
    cumAdmissions = "cumAdmissions", #Cumulative number of admissions
	covidOccupiedMVBeds = "covidOccupiedMVBeds", #COVID-19 occupied beds with mechanical ventilators
	hospitalCases = "hospitalCases" #NA
    )

data <- map(area_types, function(area_type){
    filters <- str_glue("areaType={area_type}")
    results <- get_paginated_data(filters, structure)
    })

require(plyr)
data<-as.data.frame(ldply(data)) #Transform from list format to a single data frame
require(lubridate)
data.first.lockdown<-subset(data,date>="2020-03-23" & date<"2020-05-12")
write.csv(data.first.lockdown,"dashboard.covid.first.lockdown.csv")