---
  title: "Pekka Haimi Master Thesis"
author: "Pekka Haimi"
date: "2022-11-10"
output:
  html_document:
  keep_md: yes
---
  This is analysis part of Master Thesis written by Pekka Haimi.  
Aim of the analysis:
  
  * Import Finnish birth and population data

+ Download Google Trends data with the TrendEcon package for individual keywords

+ Aggregate Google Trends data from daily to monthly level 

+ Creating index of multiple keywords

+ Plotting all data

Next steps are creating forecasting model and testing if individual keywords and the composite index make it more accurate



### Loading needed libraries

```{r,echo=TRUE, message=FALSE}
library(trendecon)
library(rmarkdown)
library(ggplot2)
library(prophet)
library(lubridate)
library(dplyr)
library(tidyverse)
library(plotly)
library(zoo)
library(forecast)
library(gtrendsR)
```

### Importing birth and population data

```{r,echo=TRUE, message=FALSE}
birthdata <- read_csv("birthsandpopulationY04Y22.csv",  col_types = cols(timestamp = col_date(format = "%Y-%m-%d")))
ggplot(birthdata, aes(x=timestamp, y=total)) + geom_line() + labs(x="time", y="Number of births", title="Total number of births monthly") 
ggplot(birthdata, aes(x=timestamp, y=first)) + geom_line() + labs(x="time", y="Number of births", title="Number of firstborns monthly") 
```




### Decomposing total births and firstborns
```{r,echo=TRUE, message=FALSE}
births_total<-birthdata$total
births_total <- ts(births_total,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_total <- decompose(births_total)
plot(decomp_births_total)

births_firstborn<-birthdata$first
births_firstborn <- ts(births_firstborn,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_firstborn <- decompose(births_firstborn)
plot(decomp_births_firstborn)
```


### Seasonally adjusting total births and comparing it to unadjusted data
```{r,echo=TRUE, message=FALSE}

births_total_seasonadj <- births_total - decomp_births_total$seasonal
plot.ts(births_total_seasonadj)
ggplot(birthdata, aes(x=timestamp, y=total)) + geom_line() + labs(x="time", y="Number of births", title="Total number of births monthly") 
```


### Seasonally adjusting firstborns and comparing it to unadjusted data
```{r,echo=TRUE, message=FALSE}
births_firstborn_seasonadj <- births_firstborn - decomp_births_firstborn$seasonal
plot.ts(births_firstborn_seasonadj)
ggplot(birthdata, aes(x=timestamp, y=first)) + geom_line() + labs(x="time", y="Number of births", title="Number of firstborns monthly") 

nt_births_firstborn <- births_firstborn - decomp_births_firstborn$trend
plot(nt_births_firstborn)
```

### downloading keyword data and applying seasonal adjustment 
```{r,echo=TRUE, message=FALSE}
kw_raskaustesti_raw <- ts_gtrends("raskaustesti",time="2004-01-01 2022-11-30",geo='FI')
kw_raskaustesti_raw <- rename(kw_raskaustesti_raw, raskaustesti = value)

kw_raskausoireet_raw <- ts_gtrends("raskaus oireet",time="2004-01-01 2022-11-30",geo='FI')
kw_raskausoireet_raw <- rename(kw_raskausoireet_raw, raskausoireet = value)

kw_alkuraskaus_raw <- ts_gtrends("alkuraskaus",time="2004-01-01 2022-11-30",geo='FI',sadj=TRUE)
kw_alkuraskaus_sa <- rename(kw_alkuraskaus_sa, alkuraskaus = value)

kw_raskaustesti_nsa_raw <- ts_gtrends("raskaustesti",time="2004-01-01 2022-11-30",geo='FI')
kw_raskaustesti_nsa <- subset(kw_raskaustesti_nsa_raw,id=='sadj')
kw_raskaustesti_nsa <- subset(kw_raskaustesti_nsa,select=-id)
mutate(kw_raskaustesti_nsa, time= as.Date(time, format= "%Y-%m-%d"))
kw_raskaustesti_nsa <- rename(kw_raskaustesti_nsa, raskaustesti = value)

kw_monitermi_sa_raw <- ts_gtrends_index("alkuraskaus",time="2004-01-01 2022-11-30",geo='FI',sadj=TRUE)
kw_alkuraskaus_sa <- subset(kw_alkuraskaus_sa_raw,id=='sadj')
kw_alkuraskaus_sa <- subset(kw_alkuraskaus_sa,select=-id)
mutate(kw_alkuraskaus_sa, time= as.Date(time, format= "%Y-%m-%d"))
kw_alkuraskaus_sa <- rename(kw_alkuraskaus_sa, alkuraskaus = value)

tsbox::ts_plot(kw_raskaustesti_nsa)
tsbox::ts_plot(kw_raskaustesti_sa)
```

### Joining data to one table and applying lags




### Joining data and adding lags
```{r,echo=TRUE, message=FALSE}
df_totalbirths <- data.frame(totalbirths=as.matrix(births_total_seasonadj),time=as.Date(as.yearmon(time(births_total_seasonadj))))
df_firstborns <- data.frame(firstborns=as.matrix(births_firstborn_seasonadj),time=as.Date(as.yearmon(time(births_firstborn_seasonadj))))

df <- df_totalbirths %>% left_join(df_firstborns, by = c("time"="time"))
df <- df %>% left_join(kw_raskaustesti_sa, by = c("time"="time"))
df <- df %>% left_join(kw_raskausoireet_sa, by = c("time"="time"))
df <- df %>% left_join(kw_alkuraskaus_sa, by = c("time"="time"))

df <- df %>% relocate(time)



df %>% plot_ly(., x = ~time) %>% 
  add_trace(y = ~firstborns, name = 'esikoiset',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti, name = 'raskaustesti',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet, name = 'raskaus oireet',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~alkuraskaus, name = 'alkuraskaus',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))



df <- df %>%                           
  mutate(raskaustesti_L9 = lag(raskaustesti, n=8,))

df <- df %>%                           
  mutate(raskausoireet_L9 = lag(raskausoireet, n=8,))

df <- df %>%                           
  mutate(alkuraskaus_L9 = lag(alkuraskaus, n=8,))

df %>% plot_ly(., x = ~time) %>% 
  add_trace(y = ~totalbirths, name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti_L9, name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet_L9, name = 'raskaus oireet L9',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~alkuraskaus_L9, name = 'alkuraskaus L9',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))

df <- df %>% filter(time >="2010-01-01")

```

### Selvitä lagin määrä, librarynä forecast aikasarjalla, miten performoi, selittävillä muuttujilla,

