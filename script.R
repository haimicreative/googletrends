## Pekka Haimi Master Thesis work 2022
#Installing packages

#install.packages("remotes")
#install.packages("rmarkdown")
#install.packages("prophet")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("tidyverse")


#remotes::install_github("trendecon/trendecon")


## Loading needed libraries
library(trendecon)
library(rmarkdown)
library(ggplot2)
library(prophet)
library(lubridate)
library(dplyr)
library(tidyverse)


############### Birth data ##################################

## Importing birth and population data
birthdata <- read_csv("birthsandpopulationY04Y22.csv", + col_types = cols(timestamp = col_date(format = "%Y-%m-%d")))
ggplot(birthdata, aes(x=timestamp, y=total)) + geom_line() + labs(x="time", y="Number of births", title="Total number of births monthly") 
ggplot(birthdata, aes(x=timestamp, y=first)) + geom_line() + labs(x="time", y="Number of births", title="Number of firstborns monthly") 

#decomposing total births
births_total<-birthdata$total
births_total <- ts(births_total,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_total <- decompose(births_total)
plot(decomp_births_total)

#seasonally adjusting total births
births_total_seasonadj <- births_total - decomp_births_total$seasonal
plot.ts(births_total_seasonadj)
View(births_total_seasonadj)
#decomposing firstborns
births_firstborn<-birthdata$first
births_firstborn <- ts(births_firstborn,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_firstborn <- decompose(births_firstborn)
plot(decomp_births_firstborn)

#seasonally adjusting firstborns
births_firstborn_seasonadj <- births_firstborn - decomp_births_firstborn$seasonal
plot.ts(births_firstborn_seasonadj)


################## Google Trends data ###############

#Downloading raw data

#proc_keyword_init("raskaustesti", "FI")
#proc_keyword_init("clearblue", "FI")
#proc_keyword_init("ovulaatiotesti","FI")
#proc_keyword_init("raskauspahoinvointi","FI")

### Creating composite index of the birth keywords
kw_syntyvyys <- c("raskaustesti","clearblue","ovulaatiotesti","raskauspahoinvointi")

proc_index(kw_syntyvyys,"FI","syntyvyysindex")

###Importing seasonally adjusted data for keywords
raskaustesti_sa <- read_csv("raw/fi/raskaustesti_sa.csv", col_types = cols(time = col_date(format = "%Y-%m-%d")))
raskaustesti_sa$month <- floor_date(raskaustesti_sa$time, "month")
raskaustesti_monthly <- (raskaustesti_sa %>% group_by(month) %>% summarize(mean = mean(value)))


clearblue_sa <- read_csv("raw/fi/clearblue_sa.csv", col_types = cols(time=col_date(format = "%Y-%m-%d")))
clearblue_sa$month <- floor_date(clearblue_sa$time, "month")
clearblue_monthly <- (clearblue_sa %>% group_by(month) %>% summarize(mean = mean(value)))

ovulaatiotesti_sa <- read_csv("raw/fi/ovulaatiotesti_sa.csv", col_types = cols(time=col_date(format = "%Y-%m-%d")))
ovulaatiotesti_sa$month <- floor_date(ovulaatiotesti_sa$time, "month")
ovulaatiotesti_monthly <- (ovulaatiotesti_sa %>% group_by(month) %>% summarize(mean = mean(value)))


raskauspahoinvointi_sa <- read_csv("raw/fi/raskauspahoinvointi_sa.csv", col_types = cols(time=col_date(format = "%Y-%m-%d")))
raskauspahoinvointi_sa$month <- floor_date(raskauspahoinvointi_sa$time, "month")
raskauspahoinvointi_monthly <- (raskauspahoinvointi_sa %>% group_by(month) %>% summarize(mean = mean(value)))

##Plotting the individual seasonally adjusted keywords
ggplot(raskaustesti_monthly, aes(x=month,y=mean)) + geom_line()
ggplot(clearblue_monthly, aes(x=month,y=mean)) + geom_line()
ggplot(ovulaatiotesti_monthly, aes(x=month,y=mean)) + geom_line()
ggplot(raskauspahoinvointi_monthly, aes(x=month,y=mean)) + geom_line()


syntyvyysindeksi <- read_csv("raw/fi/syntyvyysindex_sa.csv", col_types = cols(time = col_date(format = "%Y-%m-%d"), 
                                                                              value = col_number()))
syntyvyysindeksi$month <- floor_date(syntyvyysindeksi$time, "month")
syntyvyysindeksi %>% group_by(month) %>% summarize(mean = mean(value))

syntyvyysindeksi_monthly <- (syntyvyysindeksi %>% group_by(month) %>% summarize(mean = mean(value)))

ggplot(syntyvyysindeksi_monthly, aes(x=month,y=mean)) + geom_line()
