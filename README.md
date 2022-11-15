# googletrends
Pekka Haimi Master Thesis R scripts
Predicting birth rate in Finland with Google Trends data


## Loading needed libraries
library(trendecon)
library(rmarkdown)
library(ggplot2)
library(prophet)
library(lubridate)
library(dplyr)
library(tidyverse)


Downloading birth data


## Importing birth and population data
birthdata <- read_csv("birthsandpopulationY04Y22.csv", + col_types = cols(timestamp = col_date(format = "%Y-%m-%d")))
ggplot(birthdata, aes(x=timestamp, y=total)) + geom_line() + labs(x="time", y="Number of births", title="Total number of births monthly") 
ggplot(birthdata, aes(x=timestamp, y=first)) + geom_line() + labs(x="time", y="Number of births", title="Number of firstborns monthly") 

#decomposing total births
births_total<-birthdata$total
births_total <- ts(births_total,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_total <- decompose(births_total)
plot(decomp_births_total)
