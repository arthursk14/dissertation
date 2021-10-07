# Packages

    require(tidyverse)
    require(readxl)

# Database
    
    setwd("C:/Users/arthu/Dropbox/Arthur - Dissertação - PPGE/Final/R")
    
    OWID <- as.tibble(read.csv("owid-covid-data.csv"))
    stringency  <- as.tibble(read.csv("covid-stringency-index.csv"))
    retail <- as.tibble(read.csv("change-visitors-retail-recreation.csv"))
    
    IMF <- as.tibble(read_excel("IMF.xlsx"))
    
# Filter
    
    stringency <- stringency %>% filter(as.Date(Day) <= "2021-05-31" & as.Date(Day) >= "2020-03-01")
    stringency <- stringency %>% group_by(Code) %>% summarise(stringency_index = mean(stringency_index))

    retail <- retail %>% filter(as.Date(Day) <= "2021-05-31" & as.Date(Day) >= "2020-03-01")
    retail <- retail %>% group_by(Code) %>% summarise(retail_and_recreation = mean(retail_and_recreation))
    
# Treating OWID data
    
    filter <- OWID %>% filter(date=="2021-05-31",
                                         !is.na(total_deaths_per_million))
    
# Join ISO code
    
    join <- inner_join(filter,IMF, by=c("iso_code"="ISO")) %>% 
            inner_join(.,stringency, by=c("iso_code"="Code")) %>%
            inner_join(.,retail, by=c("iso_code"="Code"))
      
# Utilized columns
    
    database <- join %>% select(iso_code, 
                                total_cases_per_million,
                                total_deaths_per_million, 
                             
                                delta_GDP,
                                delta_GGD,
                             
                                median_age,
                             
                                stringency_index.y,
                                retail_and_recreation
                             )
   
# Last filter
    
    data <- database %>% filter(total_cases_per_million>=500, !(delta_GDP==0 & delta_GGD==0), !(iso_code %in% c("ZWE", "NIC", "HTI", "JOR", "KWT")))
    
# Variables
    
    y = (mean(data$total_deaths_per_million)-data$total_deaths_per_million) - 
          min((mean(data$total_deaths_per_million)-data$total_deaths_per_million)) + 100
    
    x1 = (mean(data$delta_GDP)-data$delta_GDP) - 
      min((mean(data$delta_GDP)-data$delta_GDP)) + 100
    
    x2 = (data$delta_GGD-mean(data$delta_GGD)) - 
      min((data$delta_GGD-mean(data$delta_GGD))) + 100
    
    x = cbind(x1, x2)
    
    z = cbind(data$median_age,
              data$stringency_index.y,
              -data$retail_and_recreation)
    
    plot(x[,1],y)
    plot(x[,2],y)
    
    source("explaining.r")
    
    z = data$median_age
    
    source("ranking.r")
    