---
  title: "Pekka Haimi Master Thesis"
author: "Pekka Haimi"
date: "2022-11-10"
output:
  html_document:
  keep_md: yes
code_folding: hide
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
library(fpp3)
library(rmarkdown)
library(plotly)
library(forecast)
library(gtrendsR)
library(tidyverse)
library(kableExtra)
library(vars)
library(GGally)

```

### Importing birth and population data and downloading keyword data

```{r,echo=TRUE, message=FALSE}


dataset <- readr::read_csv("birthsandpopulationY04Y22.csv")
dataset <- dataset %>% dplyr::filter(timestamp >= "2009-01-01")

kw_raskaus_raw <- ts_gtrends("raskaus",time="2009-01-01 2022-12-31",geo='FI')
kw_raskaus_raw <- rename(kw_raskaus_raw, raskaus = value)

kw_raskaustesti_raw <- ts_gtrends("raskaustesti",time="2009-01-01 2022-12-31",geo='FI')
kw_raskaustesti_raw <- rename(kw_raskaustesti_raw, raskaustesti = value)

kw_raskausoireet_raw <- ts_gtrends("raskaus oireet",time="2009-01-01 2022-12-31",geo='FI')
kw_raskausoireet_raw <- rename(kw_raskausoireet_raw, raskausoireet = value)

kw_raskaana_raw <- ts_gtrends("raskaana",time="2009-01-01 2022-12-31",geo='FI')
kw_raskaana_raw <- rename(kw_raskaana_raw,raskaana = value)

keywords <- kw_raskaustesti_raw %>% left_join(kw_raskausoireet_raw, by = c("time"="time"))
keywords <- keywords %>% left_join(kw_raskaana_raw, by = c("time"="time"))
keywords <- keywords %>% left_join(kw_raskaus_raw, by = c("time"="time"))

dataset <- dataset %>% full_join(keywords,by = c("timestamp"="time"),keep=TRUE)

dataset <- dataset %>% mutate (Month = yearmonth(time))


dataset <- dataset  %>% 
  dplyr::select(Month,first,total,raskaus,raskaustesti,raskausoireet,raskaana,timestamp) %>% 
  as_tsibble(index = Month)

## adding lags
dataset <- dataset %>% mutate(raskaus_L12 = lag(raskaus, n=12,))
dataset <- dataset %>% mutate(raskaus_L11 = lag(raskaus, n=11,))
dataset <- dataset %>% mutate(raskaus_L10 = lag(raskaus, n=10,))
dataset <- dataset %>% mutate(raskaus_L9 = lag(raskaus, n=9,))
dataset <- dataset %>% mutate(raskaus_L8 = lag(raskaus, n=8,))
dataset <- dataset %>% mutate(raskaus_L7 = lag(raskaus, n=7,))
dataset <- dataset %>% mutate(raskaus_L6 = lag(raskaus, n=6,))
dataset <- dataset %>% mutate(raskaus_L5 = lag(raskaus, n=5,))
dataset <- dataset %>% mutate(raskaus_L4 = lag(raskaus, n=4,))
dataset <- dataset %>% mutate(raskaus_L3 = lag(raskaus, n=3,))

dataset <- dataset %>% mutate(raskaustesti_L12 = lag(raskaustesti, n=12,))
dataset <- dataset %>% mutate(raskaustesti_L11 = lag(raskaustesti, n=11,))
dataset <- dataset %>% mutate(raskaustesti_L10 = lag(raskaustesti, n=10,))
dataset <- dataset %>% mutate(raskaustesti_L9 = lag(raskaustesti, n=9,))
dataset <- dataset %>% mutate(raskaustesti_L8 = lag(raskaustesti, n=8,))
dataset <- dataset %>% mutate(raskaustesti_L7 = lag(raskaustesti, n=7,))
dataset <- dataset %>% mutate(raskaustesti_L6 = lag(raskaustesti, n=6,))
dataset <- dataset %>% mutate(raskaustesti_L5 = lag(raskaustesti, n=5,))
dataset <- dataset %>% mutate(raskaustesti_L4 = lag(raskaustesti, n=4,))
dataset <- dataset %>% mutate(raskaustesti_L3 = lag(raskaustesti, n=3,))

dataset <- dataset %>% mutate(raskausoireet_L12 = lag(raskausoireet, n=12,))
dataset <- dataset %>% mutate(raskausoireet_L11 = lag(raskausoireet, n=11,))
dataset <- dataset %>% mutate(raskausoireet_L10 = lag(raskausoireet, n=10,))
dataset <- dataset %>% mutate(raskausoireet_L9 = lag(raskausoireet, n=9,))
dataset <- dataset %>% mutate(raskausoireet_L8 = lag(raskausoireet, n=8,))
dataset <- dataset %>% mutate(raskausoireet_L7 = lag(raskausoireet, n=7,))
dataset <- dataset %>% mutate(raskausoireet_L6 = lag(raskausoireet, n=6,))
dataset <- dataset %>% mutate(raskausoireet_L5 = lag(raskausoireet, n=5,))
dataset <- dataset %>% mutate(raskausoireet_L4 = lag(raskausoireet, n=4,))
dataset <- dataset %>% mutate(raskausoireet_L3 = lag(raskausoireet, n=3,))

dataset <- dataset %>% mutate(raskaana_L12 = lag(raskaana, n=12,))
dataset <- dataset %>% mutate(raskaana_L11 = lag(raskaana, n=11,))
dataset <- dataset %>% mutate(raskaana_L10 = lag(raskaana, n=10,))
dataset <- dataset %>% mutate(raskaana_L9 = lag(raskaana, n=9,))
dataset <- dataset %>% mutate(raskaana_L8 = lag(raskaana, n=8,))
dataset <- dataset %>% mutate(raskaana_L7 = lag(raskaana, n=7,))
dataset <- dataset %>% mutate(raskaana_L6 = lag(raskaana, n=6,))
dataset <- dataset %>% mutate(raskaana_L5 = lag(raskaana, n=5,))
dataset <- dataset %>% mutate(raskaana_L4 = lag(raskaana, n=4,))
dataset <- dataset %>% mutate(raskaana_L3 = lag(raskaana, n=3,))

```


```{r,echo=TRUE, message=FALSE}
##Filtering data for 2010 onwards
dataset_2010 <- dataset %>% 
  dplyr::filter(timestamp >= "2010-01-01")


## creating training and testing sets
train <- dataset_2010 %>% 
  dplyr::filter(timestamp < "2020-07-01")
test <- dataset_2010 %>% 
  dplyr::filter(timestamp >= "2020-07-01")
```

```{r,echo=TRUE, message=FALSE}
##peruskuvaajat
dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% autoplot(vars(first,total))
dataset %>% autoplot(vars(raskaustesti,raskausoireet,raskaana))
```

```{r,echo=TRUE, message=FALSE}
## Plotting seasonal stuff
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(total)) %>% components() %>% autoplot()
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(first)) %>% components() %>% autoplot()

dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(raskaus)) %>% components() %>% autoplot()
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(raskaustesti)) %>% components() %>% autoplot()
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(raskausoireet)) %>% components() %>% autoplot()
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(stl=STL(raskaana)) %>% components() %>% autoplot()

##Subseries
dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% gg_season(total, labels = "both")
dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% gg_season(first, labels = "both")

dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% gg_subseries(total)
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% gg_subseries(first)

dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-12-31"))) %>% gg_subseries(raskaus)
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-12-31"))) %>% gg_subseries(raskaustesti)
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-12-31"))) %>% gg_subseries(raskausoireet)
dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-12-31"))) %>% gg_subseries(raskaana)

```

```{r,echo=TRUE, message=FALSE}
### Päällekkäiset kuvaajat

dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikki syntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaus, name = 'raskaus',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskaus_L9, name = 'raskaus L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L8, name = 'raskaus L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L7, name = 'raskaus L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L6, name = 'raskaus L6',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L5, name = 'raskaus L5',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L4, name = 'raskaus L4',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaus_L3, name = 'raskaus L3',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))


dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikki syntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti, name = 'raskaustesti',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskaustesti_L9, name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L8, name = 'raskaustesti L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L7, name = 'raskaustesti L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L6, name = 'raskaustesti L6',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L5, name = 'raskaustesti L5',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L4, name = 'raskaustesti L4',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L3, name = 'raskaustesti L3',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))

dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikki syntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskausoireet, name = 'raskausoireet',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet_L9, name = 'raskausoireet L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L8, name = 'raskausoireet L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L7, name = 'raskausoireet L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L6, name = 'raskausoireet L6',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L5, name = 'raskausoireet L5',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L4, name = 'raskausoireet L4',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L3, name = 'raskausoireet L3',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))



dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikki syntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaana, name = 'raskaana',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskaana_L9, name = 'raskaana L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L8, name = 'raskaana L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L7, name = 'raskaana L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L6, name = 'raskaana L6',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L5, name = 'raskaana L5',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L4, name = 'raskaana L4',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaana_L3, name = 'raskaana L3',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))




```


```{r,echo=TRUE, message=FALSE}
### Korrelaatiokuvaajat

dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% ggplot(aes(x=total,y=raskaus)) + geom_point() + geom_smooth(method=lm) + labs(x="Elävänä syntyneet lapset kuukausittain",y="Hakusanan raskaus hakuindeksi")


dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% ggplot(aes(x=total,y=raskaustesti)) + geom_point() + geom_smooth(method=lm) + labs(x="Elävänä syntyneet lapset kuukausittain",y="Hakusanan raskaustesti hakuindeksi")


dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% ggplot(aes(x=total,y=raskaustesti_L7)) + geom_point() + geom_smooth(method=lm) + labs(x="Elävänä syntyneet lapset kuukausittain",y="Hakusanan raskaustesti L7 hakuindeksi")


dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% ggplot(aes(x=total,y=raskausoireet)) + geom_point() + geom_smooth(method=lm) + labs(x="Elävänä syntyneet lapset kuukausittain",y="Hakusanan raskaus oireet hakuindeksi")

dataset %>%  dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% ggplot(aes(x=total,y=raskaana)) + geom_point() + geom_smooth(method=lm) + labs(x="Elävänä syntyneet lapset kuukausittain",y="Hakusanan raskaana oireet hakuindeksi")

```

```{r,echo=TRUE, message=FALSE}
##lag plots ## Mutta miksi?
dataset %>% 
  dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_lag(first,geom="point")
dataset %>% 
  dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_lag(total,geom="point")
dataset %>% gg_lag(raskaustesti,geom="point")
```


```{r,echo=TRUE, message=FALSE}
#No differencing 
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(total,
               plot_type='partial',lag=36) + 
  labs(title="not differenced",y="")

#Normally differenced ARIMA of  total
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(difference(total),
               plot_type='partial',lag=36) + 
  labs(title="normally differenced",y="")


#Seasonal differenced ARIMA of  total
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(difference(total,12),
               plot_type='partial',lag=36) + 
  labs(title="seasonally differenced",y="")


#Double differenced  data
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(difference(total,12) %>% difference(),
               plot_type='partial',lag=36) + 
  labs(title="double differenced",y="")


#Seasonal ARIMA of logged total
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(difference(log(total),12),
               plot_type='partial',lag=36) + 
  labs(title="seasonally differenced",y="")

#Double differenced log data
dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  gg_tsdisplay(difference(log(total),12) %>% difference(),
               plot_type='partial',lag=36) + 
  labs(title="seasonally differenced",y="")
```
#C
According to AutoArima for fit without log is 
model = 101 211

```{r,echo=TRUE, message=FALSE}
##Lets build ARIMA model for the total births

fit <- dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% 
  model( 
    ARIMA101210=ARIMA(log(total) ~ 0 + pdq(1,0,1) + PDQ(2,1,0)),
    ARIMA102210=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,0)),
    ARIMA103210=ARIMA(log(total) ~ 0 + pdq(1,0,3) + PDQ(2,1,0)),
    ARIMA101211=ARIMA(log(total) ~ 0 + pdq(1,0,1) + PDQ(2,1,1)),
    ARIMA102211=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1)),
    ARIMA103211=ARIMA(log(total) ~ 0 + pdq(1,0,3) + PDQ(2,1,1)),
    ARIMA101212=ARIMA(log(total) ~ 0 + pdq(1,0,1) + PDQ(2,1,2)),
    ARIMA102212=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,2)),
    ARIMA103212=ARIMA(log(total) ~ 0 + pdq(1,0,3) + PDQ(2,1,2)),
    auto= ARIMA(log(total),stepwise=FALSE,approx=FALSE)                  
  )


fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

fit %>% select(ARIMA101211) %>% gg_tsresiduals(lag=36)

##Ljung box
augment(fit) %>%
  filter(.model == "ARIMA101211") %>%
  features(.innov, ljung_box, lag=24, dof=4)

dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2022-07-01"))) %>% model(ARIMA102211=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1))) %>% forecast() %>% autoplot(dataset)

```

#Time to build our first Dynamic Regression model with different predictors
```{r,echo=TRUE, message=FALSE}
fit_t_raskaustesti <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model( 
    no_lag=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti)),
    L3=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L3)),
    L4=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L4)),
    L5=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L5)),
    L6=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L6)),
    L7=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L7)),
    L8=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L8)),
    L9=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L9)),
    L10=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L10)),
    L11=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L11)),
    L12=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L12))
  )

fit_t_raskaus <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model( 
    no_lag=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus)),
    L3=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L3)),
    L4=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L4)),
    L5=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L5)),
    L6=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L6)),
    L7=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L7)),
    L8=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L8)),
    L9=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L9)),
    L10=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L10)),
    L11=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L11)),
    L12=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L12))
  )

fit_t_raskausoireet <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model( 
    no_lag=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet)),
    L3=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L3)),
    L4=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L4)),
    L5=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L5)),
    L6=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L6)),
    L7=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L7)),
    L8=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L8)),
    L9=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L9)),
    L10=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L10)),
    L11=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L11)),
    L12=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L12))
  )

fit_t_raskaana <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model( 
    no_lag=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana)),
    L3=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L3)),
    L4=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L4)),
    L5=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L5)),
    L6=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L6)),
    L7=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L7)),
    L8=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L8)),
    L9=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L9)),
    L10=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L10)),
    L11=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L11)),
    L12=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L12))
  )





glance(fit_t_raskaustesti) %>% arrange(AICc) %>% select(.model:BIC)
glance(fit_t_raskaus) %>% arrange(AICc) %>% select(.model:BIC)
glance(fit_t_raskausoireet) %>% arrange(AICc) %>% select(.model:BIC)
glance(fit_t_raskaana) %>% arrange(AICc) %>% select(.model:BIC)



```
### Creating "the" model
```{r,echo=TRUE, message=FALSE}
fit_t <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model(
    main_model=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L7) + log(raskaus_L7)+ log(raskausoireet_L7)+log(raskaana_L7)),
    Only_arima=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1)),
    raskaustesti=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L7)),
    raskaus=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaus_L7)),
    raskausoireet=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L7)),
    raskaana=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaana_L7)))


glance(fit_t) %>% arrange(AICc) %>% select(.model:BIC)       

fit_t_best <- dataset %>% dplyr::filter(timestamp >= "2010-01-01") %>% 
  model( 
    main_model=ARIMA(total ~ pdq(1,0,1) + PDQ (2,1,1) + raskaustesti_L7 + raskaus_L4 + raskausoireet_L6 + raskaana_L3))

report(fit_t_best)


```
### Training model and forecast
```{r,echo=TRUE, message=FALSE}

fit_t_best_train <- dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2020-01-01"))) %>% 
  model( 
    raskausoireet=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskausoireet_L7)),
    ARIMAlog=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1)),
    main_model=ARIMA(log(total) ~ 0 + pdq(1,0,2) + PDQ(2,1,1) + log(raskaustesti_L7) + log(raskaus_L7)+ log(raskausoireet_L7)+log(raskaana_L7)))


fit_t_arimas_train <- dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2020-01-01"))) %>% 
  model( 
    ARIMAlog=ARIMA(log(total)),
    ARIMA010010=ARIMA(log(total) ~ pdq(0,1,0) + PDQ(0,1,0)),
    ARIMA110110=ARIMA(log(total) ~ pdq(1,1,0) + PDQ(1,1,0)),
    ARIMA100110=ARIMA(log(total) ~ pdq(1,0,0) + PDQ(1,1,0)),
    ARIMA111111=ARIMA(log(total) ~ pdq(1,1,1) + PDQ(1,1,1))
  )


fit_t_best_train_2 <- dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2020-07-01"))) %>% 
  model( 
    main_model=ARIMA(total))

future <- dataset %>% dplyr::filter(between(timestamp,as.Date("2020-02-01"),as.Date("2022-12-01")))


fit_t_best_train %>% 
  forecast(future) %>% 
  autoplot(dataset,level=95) + facet_wrap(vars(.model), ncol = 2) + 
  guides(colour="none",fill="none",level="none") +
  geom_label(aes(x=yearmonth("2020 Jan"), y=5000, label = paste0("AICc = ", format(AICc))), 
             data = (glance(fit_t_best_train)))

fit_t_best_train %>%  forecast(future) %>% accuracy(dataset) %>% select(.model,RMSE:MAPE)

accuracy(fcs) %>%
  select(.model, RMSE:MAPE)                       


fit_t_best_train %>% forecast(future) %>% autoplot(dataset,level=NULL)
fit_t_arimas_train %>% forecast(future) %>% autoplot(dataset,level=NULL)
fit_t_best_train_2 %>% forecast(future) %>% autoplot(dataset)




#mallien vertailu template
fit_t_best_train <- dataset %>% dplyr::filter(between(timestamp,as.Date("2010-01-01"),as.Date("2020-01-01"))) %>% 
  model( 
    ARIMAlog=ARIMA(log(total)),
    ARIMAlog_predictors=ARIMA(log(total) ~ raskaustesti_L7 + raskaus_L4 + raskausoireet_L6 + raskaana_L3),
    ARIMA010010=ARIMA(log(total) ~ pdq(1,1,0) + PDQ(1,1,0)),
    ARIMA110110=ARIMA(log(total) ~ pdq(1,1,0) + PDQ(1,1,0)),
    TSLM=TSLM(total ~ raskaustesti_L7 + raskaus_L4 + raskausoireet_L6 + raskaana_L3),
    ETS=ETS(total)
  )

```
### Forecasting
```{r,echo=TRUE, message=FALSE}

forecast(fit_t_best_train, h=36) %>%
  filter(.model=='auto') %>%
  autoplot(dataset) +
  labs(title = "y",
       y="")

forecast(fit, h=36) %>%
  filter(.model=='arima011011') %>%
  autoplot(dataset) +
  labs(title = "y",
       y="")




```
dataset %>% ACF(total,lag_max=12)
dataset %>% ACF(total) %>% autoplot()
dataset %>% ACF(total,lag_max = 48) %>% autoplot()

dataset %>% ACF(first,lag_max=12)
dataset %>% ACF(first) %>% autoplot()
dataset %>% ACF(first,lag_max = 48) %>% autoplot()



dataset %>% ACF(raskaustesti,lag_max=12)
dataset %>% ACF(raskaustesti) %>% autoplot()
dataset %>% ACF(raskaustesti,lag_max = 48) %>% autoplot()

```

```{r,echo=TRUE, message=FALSE}
### Decomposing with STL method “Seasonal and Trend decomposition using Loess”
dcmp_total <- dataset %>% model(stl=STL(total))

dcmp_first <- dataset %>% model(stl=STL(first))
dcmp_raskaustesti <- dataset %>% model(stl=STL(raskaustesti))


#Joinig Seasonally adjusted data to dataset
tsibble_dcmp_total <- components(dcmp_total) %>% as_tsibble()
tsibble_dcmp_total <- tsibble_dcmp_total %>% mutate(total_sa_remaind=season_year+remainder)
tsibble_dcmp_total <- tsibble_dcmp_total %>% 
  mutate(total_sa = season_adjust) %>% 
  dplyr::select(Month,total_sa,total_sa_remaind)

dataset <- dataset %>% left_join(tsibble_dcmp_total,by=c("Month"="Month"))

tsibble_dcmp_raskaustesti <- components(dcmp_raskaustesti) %>% as_tsibble()
tsibble_dcmp_raskaustesti <- tsibble_dcmp_raskaustesti %>% mutate(raskaustesti_sa_remaind=season_year+remainder)
tsibble_dcmp_raskaustesti <- tsibble_dcmp_raskaustesti %>% 
  mutate(raskaustesti_sa = season_adjust) %>% 
  dplyr::select(Month,raskaustesti_sa,raskaustesti_sa_remaind)

dataset <- dataset %>% left_join(tsibble_dcmp_raskaustesti,by=c("Month"="Month"))


####
components(dcmp_total) %>% autoplot()
components(dcmp_total) %>% as_tsibble() %>% autoplot(total,colour="gray") + geom_line(aes(y=trend),colour="#D55E00") +labs(y="Syntyneet", title="Syntyneet Suomessa, trendi ja absoluuttiset syntymät")
components(dcmp_total) %>% as_tsibble() %>% autoplot(total,colour="gray")+geom_line(aes(y=season_adjust),colour = "#0072B2") + labs(y="Syntyneet",title="Kausitasoitetut syntymät ja absoluuttiset syntymät")

components(dcmp_raskaustesti) %>% autoplot()
components(dcmp_raskaustesti) %>% as_tsibble() %>% autoplot(raskaustesti,colour="gray") + geom_line(aes(y=trend),colour="#D55E00") +labs(y="Hakutermin indeksi", title="Hakutermin raskaustesti hakuindeksin trendi ja indeksi")
components(dcmp_raskaustesti) %>% as_tsibble() %>% autoplot(raskaustesti,colour="gray")+geom_line(aes(y=season_adjust),colour = "#0072B2") + labs(y="Hakutermin indeksi",title="Kausitasoitettu hakuindeksi raskaustesti ja indeksi")


```
### Seasonal analysis
```{r,echo=TRUE, message=FALSE}
dataset %>% gg_season(total,labels="both") + labs(y="Syntynyttä lasta",title="Elävänä syntyneet kuukausittain")
dataset %>% gg_season(first,labels="both") + labs(y="Syntynyttä esikoislasta",title="Elävänä syntyneet esikoiset kuukausittain")
dataset %>% gg_season(raskaustesti,labels="both") + labs(y="Hakuindeksiluku",title="Hakutermin raskaustesti yleisyys kuukausittain")
dataset %>% gg_season(raskausoireet,labels="both") + labs(y="Hakuindeksiluku",title="Hakutermin raskaus oireet yleisyys kuukausittain")
dataset %>% gg_season(raskaana,labels="both") + labs(y="Hakuindeksiluku",title="Hakutermin raskaana yleisyys kuukausittain")

##Subseries
dataset %>% gg_subseries(total)+ labs(y="Syntynyttä lasta",title="Elävänä syntyneet kuukausittain")
dataset %>% gg_subseries(first)+ labs(y="Syntynyttä esikoislasta",title="Elävänä syntyneet esikoiset kuukausittain")
dataset %>% gg_subseries(raskaustesti)+ labs(y="Hakuindeksiluku",title="Hakutermin raskaustesti yleisyys kuukausittain")
dataset %>% gg_subseries(raskausoireet)+ labs(y="Hakuindeksiluku",title="Hakutermin raskausoireet yleisyys kuukausittain")
dataset %>% gg_subseries(raskaana)+ labs(y="Hakuindeksiluku",title="Hakutermin raskaana yleisyys kuukausittain")


### overlaying plots

dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti, name = 'raskaustesti',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskaustesti_L9, name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L8, name = 'raskaustesti L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L7, name = 'raskaustesti L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskaustesti_L6, name = 'raskaustesti L6',mode = 'lines', yaxis="y2") 
%>%
  layout(yaxis2=list(overlaying="y", side="right"))

dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~log(total), name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~log(raskaustesti), name = 'raskaustesti',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~log(raskaustesti_L9), name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~log(raskaustesti_L8), name = 'raskaustesti L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~log(raskaustesti_L7), name = 'raskaustesti L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~log(raskaustesti_L6), name = 'raskaustesti L6',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))


dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total_sa_remaind, name = 'kaikkisyntyneet kausitasoitettu',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti_sa_remaind, name = 'raskaustesti kausitasoitettu',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))



dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskausoireet, name = 'raskausoireet',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet_L9, name = 'raskausoireet L9',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L8, name = 'raskausoireet L8',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L7, name = 'raskausoireet L7',mode = 'lines', yaxis="y2") %>%
  add_trace(y = ~raskausoireet_L6, name = 'raskausoireet L6',mode = 'lines', yaxis="y2") %>%
  layout(yaxis2=list(overlaying="y", side="right"))





dataset %>% plot_ly(., x = ~timestamp) %>% 
  add_trace(y = ~total, name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti_L9, name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet_L9, name = 'raskaus oireet L9',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))


```
### Linear regression part

```{r,echo=TRUE, message=FALSE}

dataset %>% pivot_longer(c(first,total),names_to="Series") %>% autoplot(value)
dataset %>% pivot_longer(c(raskaustesti,raskausoireet,raskaana),names_to="Series") %>% autoplot(value)

dataset %>% ggplot(aes(x = total,y = raskaustesti))+labs(y="Hakuindeksi: Raskaustesti",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% ggplot(aes(x = total,y = raskaustesti_L9))+labs(y="Hakuindeksi: Raskaustesti L9",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% ggplot(aes(x = total,y = raskaustesti_L8))+labs(y="Hakuindeksi: Raskaustesti L8",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% ggplot(aes(x = total,y = raskaustesti_L7))+labs(y="Hakuindeksi: Raskaustesti L7",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% ggplot(aes(x = total,y = raskaustesti_L6))+labs(y="Hakuindeksi: Raskaustesti L6",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)


dataset %>% ggplot(aes(x = total,y = raskausoireet))+labs(y="Hakuindeksi: Raskausoireet",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% ggplot(aes(x = total,y = raskaana))+labs(y="Hakuindeksi: Raskaana",x="Syntyneet") + geom_point() + geom_smooth(method="lm",se=FALSE)
dataset %>% GGally::ggpairs(columns=2:6)


## Linear regression equation (TSLM)
#Naive model
fits <- train %>% 
  model(
    naive=TSLM(log(total) ~ log(lag(total))),
    naive_12 = TSLM(log(total) ~ log(lag(total)) + log(lag(total,12))),
    GT_raskaustesti = TSLM(log(total) ~ log(lag(total)) + log(lag(total,n=12))+log(raskaustesti))
  )

tidy(fits)[, c(1, 2, 3, 4, 6)] %>%
  kable(
    format = "html",
    table.attr = "style='width:75%;' ",
    caption = "Model Estimation Results",
    digits = 3
  ) %>%
  kable_classic_2(full_width = F)

fit_fcs <- fits %>% forecast(new_data=test)
fit_fcs %>% autoplot(dplyr::filter(dataset,timestamp> "2010-01-01"),level=NULL)

fc_models <- models %>% forecast(new_data=dataset)
fcs_split %>%
  autoplot(filter(file, year(Month) > 2016), level = NULL)


fc_models %>%
  autoplot()

fc_models[, c(1, 2, 6, 7)] %>%
  kable(
    format = "html",
    table.attr = "style='width:50%;' ",
    caption = "Model Forecast Results",
    col.names = c("Model", "Type", "MPE", "MAPE"),
    digits = 3
  ) %>%
  kable_classic_2(full_width = F)




``` 

```{r,echo=TRUE, message=FALSE}  

###Stupid testing
dataset %>% model(TSLM(total ~ raskaustesti)) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12))) %>% report()
dataset %>% model(TSLM(log(total) ~ log(lag(total)) + log(lag(total,n=12)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=9)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=8)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=7)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=6)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=5)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=4)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=3)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=2)))) %>% report()
dataset %>% model(TSLM(total ~ lag(total) + lag (total,n=12) + log(lag(raskaustesti,n=1)))) %>% report()


dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=12)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=11)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=10)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=9)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=8)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=7)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=6)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=5)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=4)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=3)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=2)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskausoireet,n=1)))) %>% report()


dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=12)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=11)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=10)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=9)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=8)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=7)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=6)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=5)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=4)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=3)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=2)))) %>% report()
dataset %>% model(TSLM(log(total) ~ lag(total) + lag (total,n=12) + log(lag(raskaana,n=1)))) %>% report()

dataset %>% model(TSLM(total ~ raskausoireet)) %>% report()
dataset %>% model(TSLM(total ~ raskausoireet_L9)) %>% report()
dataset %>% model(TSLM(total ~ raskausoireet_L8)) %>% report()
dataset %>% model(TSLM(total ~ raskausoireet_L7)) %>% report()
dataset %>% model(TSLM(total ~ raskausoireet_L6)) %>% report()

dataset %>% model(TSLM(total ~ raskaana)) %>% report()
dataset %>% model(TSLM(total ~ raskaana_L9)) %>% report()
dataset %>% model(TSLM(total ~ raskaana_L8)) %>% report()
dataset %>% model(TSLM(total ~ raskaana_L7)) %>% report()
dataset %>% model(TSLM(total ~ raskaana_L6)) %>% report()

fit_total_keywords <- dataset %>% model(tslm=TSLM(total ~ raskaustesti + raskausoireet + raskaana))
report(fit_total_keywords)

fit_total_keywords_L9 <- dataset %>% model(tslm=TSLM(total ~ raskaustesti_L9 + raskausoireet_L9 + raskaana_L9))
report(fit_total_keywords_L9)

fit_total_keywords_L8 <- dataset %>% model(tslm=TSLM(total ~ raskaustesti_L8 + raskausoireet_L8 + raskaana_L8))
report(fit_total_keywords_L8)

fit_total_keywords_L7 <- dataset %>% model(tslm=TSLM(total ~ raskaustesti_L7 + raskausoireet_L7 + raskaana_L7))
report(fit_total_keywords_L7)

## Fitted values

augment(fit_total_keywords) %>% ggplot(aes(x=Month)) + geom_line(aes(y=total,colour="Data")) + geom_line(aes(y=.fitted,colour="Fitted"))+labs(y=NULL,title="Syntyneet") + scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) + guides(colour=guide_legend(title=NULL))

augment(fit_total_keywords_L9) %>% ggplot(aes(x=Month)) + geom_line(aes(y=total,colour="Data")) + geom_line(aes(y=.fitted,colour="Fitted"))+labs(y=NULL,title="Syntyneet") + scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) + guides(colour=guide_legend(title=NULL))


```
```{r,echo=TRUE, message=FALSE}


### AR(1) modeling
Ar1 <- dataset %>% model(
  ar1 = SARIMA(total ~ pdq(1,0,0)),                          
  auto = ARIMA(total, stepwise = TRUE, approx = FALSE))


##ARIMA modeling

dataset %>% gg_tsdisplay(difference(total, 12),
                         plot_type='partial', lag_max = 24)

Arimafit <- dataset %>% model(
  arima101211 = ARIMA(total ~ pdq(1,0,1) + PDQ(2,1,1)),                          
  auto = ARIMA(total, stepwise = TRUE, approx = FALSE))

Arimafit %>% pivot_longer(everything(), names_to = "Model name",
                          values_to = "Orders") 

glance(Arimafit) 

Arimafit %>% dplyr::select(auto) %>% gg_tsresiduals(lag=36)

augment(Arimafit) %>%
  dplyr::filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=4)

fcst_arimafit2 <- dataset %>%
  model(arima101211 = ARIMA(total ~ pdq(1,0,1) + PDQ(2,1,1))) %>% 
  forecast(h = "3 years") 


fcst_arimafit2 %>% 
  autoplot()

dataset %>%
  model(ARIMA(log(total) ~ 0 + pdq(1,0,1) + PDQ(2,1,1))) %>%
  forecast() %>%
  autoplot(dataset) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")

dataset %>%
  model(ARIMA(total)) %>%
  forecast(h="3 years") %>%
  autoplot(dataset) +
  labs(title = "",
       y = "")

dataset %>%
  model(ARIMA(total ~ 0 + pdq(1,0,1) + PDQ(2,1,1))) %>%
  forecast(h="3 years") %>%
  autoplot(dataset) +
  labs(title = "",
       y = "")

```


### Decomposing total births and firstborns
```{r,echo=TRUE, message=FALSE}
births_total<-birthdata$total

##Plotting seasonal changes for total births

birthdata %>% mutate(timestamp=yearmonth(timestamp)) %>%  
  as_tsibble(index=timestamp) -> births_tsibble

births_tsibble %>% gg_season(total,labels="both") +
  labs(y="Total births",
       title ="Seasonal changes of births")

births_tsibble %>%
  gg_subseries(total) +
  labs(
    y = "Total births",
    title = "Seasonal changes of births")


births_total <- ts(births_total,start=c(2004,1),end=c(2022,7),frequency=12)
decomp_births_total <- decompose(births_total)
plot(decomp_births_total)




births_firstborn<-birthdata$first

##Plotting seasonal changes for firstborns


births_tsibble %>% gg_season(first,labels="both") +
  labs(y="Firstborns",
       title ="Seasonal changes of firstborns in Finland")


births_tsibble %>%
  gg_subseries(first) +
  labs(
    y = "Firstborns",
    title = "Seasonal changes of firstborns in Finland")

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

kw_raskaustesti_trend <- tsbox::ts_trend(kw_raskaustesti_raw)
kw_raskaustesti_sa <- tsbox::ts_seas(kw_raskaustesti_raw)
tsbox::ts_plot(kw_raskaustesti_raw)
tsbox::ts_plot(kw_raskaustesti_trend)
tsbox::ts_plot(kw_raskaustesti_sa)

kw_raskausoireet_raw <- ts_gtrends("raskaus oireet",time="2004-01-01 2022-11-30",geo='FI')
kw_raskausoireet_raw <- rename(kw_raskausoireet_raw, raskausoireet = value)
kw_raskausoireet_sa <- tsbox::ts_seas(kw_raskausoireet_raw)

kw_alkuraskaus_raw <- ts_gtrends("alkuraskaus",time="2004-01-01 2022-11-30",geo='FI')
kw_alkuraskaus_raw <- rename(kw_alkuraskaus_raw, alkuraskaus = value)


kw_monitermi_sa_raw <- ts_gtrends_index(c("raskaustesti","raskaus oireet","alkuraskaus"),time="2004-01-01 2022-11-30",geo='FI',sadj=TRUE)
kw_monitermi_sa <- subset(kw_monitermi_sa_raw,id=='sadj')
kw_monitermi_sa <- subset(kw_monitermi_sa,select=-id)
mutate(kw_monitermi_sa, time= as.Date(time, format= "%Y-%m-%d"))
tsbox::ts_plot(kw_monitermi_sa)


tsbox::ts_plot(kw_raskaustesti_nsa)
tsbox::ts_plot(kw_raskaustesti_sa)
```

### Joining data to one table and applying lags




### Joining data and adding lags
```{r,echo=TRUE, message=FALSE}
df_totalbirths <- data.frame(totalbirths=as.matrix(births_total_seasonadj),time=as.Date(as.yearmon(time(births_total_seasonadj))))
df_firstborns <- data.frame(firstborns=as.matrix(births_firstborn_seasonadj),time=as.Date(as.yearmon(time(births_firstborn_seasonadj))))

df <- df_totalbirths %>% left_join(df_firstborns, by = c("time"="time"))
df <- df %>% left_join(kw_raskaustesti_raw, by = c("time"="time"))
df <- df %>% left_join(kw_raskausoireet_raw, by = c("time"="time"))
df <- df %>% left_join(kw_alkuraskaus_raw, by = c("time"="time"))

df <- df %>% relocate(time)
df<- transform(df, raskaustesti = as.numeric(raskaustesti), 
               raskausoireet = as.numeric(raskausoireet),
               alkuraskaus = as.numeric(raskausoireet))


df %>% plot_ly(., x = ~time) %>% 
  add_trace(y = ~firstborns, name = 'esikoiset',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti, name = 'raskaustesti',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet, name = 'raskaus oireet',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~alkuraskaus, name = 'alkuraskaus',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))

df %>% mutate(time=yearmonth(time)) %>%  
  as_tsibble(index=time) -> df


df <- df %>%                           
  mutate(raskaustesti_L9 = lag(raskaustesti, n=8,))

df <- df %>%                           
  mutate(raskausoireet_L9 = lag(raskausoireet, n=8,))

df <- df %>%                           
  mutate(alkuraskaus_L9 = lag(alkuraskaus, n=8,))

df <- df %>% filter(time >="2010-01-01")


df %>% plot_ly(., x = ~time) %>% 
  add_trace(y = ~totalbirths, name = 'kaikkisyntyneet',mode = 'lines') %>% 
  add_trace(y = ~raskaustesti_L9, name = 'raskaustesti L9',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~raskausoireet_L9, name = 'raskaus oireet L9',mode = 'lines', yaxis="y2") %>% 
  add_trace(y = ~alkuraskaus_L9, name = 'alkuraskaus L9',mode = 'lines', yaxis="y2") %>% 
  layout(yaxis2=list(overlaying="y", side="right"))


p1 <- ggplot(df, aes(time, log(totalbirths), color = "Total births")) +
  geom_line() + labs(y = "log of Total births") + theme_bw() + theme(legend.title = element_blank())
p1

df %>%    ggplot(aes(x = time)) +
  geom_line(aes(y = raskaustesti, color = "Raskaustesti")) +
  geom_line(aes(y = raskausoireet, color = "Raskaus Oireet")) +
  geom_line(aes(y = alkuraskaus, color = "Alkuraskaus")) +
  labs(
    title = "Google Trends Queries",
    caption = "Source: Google Trends",
    y = "Search Popularity Index",
    x = "Month"
  )  +
  theme(legend.position = "top", legend.title = element_blank())

```
### Analysing seasonality of the birth data
```{r,echo=TRUE, message=FALSE}

autoplot(df)
### Seasonal changes of keywords
df %>% gg_season(raskaustesti,labels="both") +
  labs(y="Raskaustesti",
       title ="Raskaustesti")

df %>%
  gg_subseries(raskaustesti) +
  labs(
    y = "Raskaustesti",
    title = "Seasonal changes of keyword raskaustesti")

df %>%
  gg_subseries(raskausoireet) +
  labs(
    y = "Raskausoireet",
    title = "Seasonal changes of keyword raskausoireet")


```

### Splitting dataset and building the model
```{r,echo=TRUE, message=FALSE}
#Transforming data frame to tsibble
df <- df %>% as_tsibble(index= time)

train <- df %>% filter(time < '2021-01-01')
test <- df %>% filter(time > '2020-12-01')

fits <- train %>%
  model(
    fit_naive = TSLM(log(totalbirths) ~ 1 + lag(log(totalbirths))),
    fit_GT     = TSLM(
      log(totalbirths) ~ 1 + lag(log(totalbirths)) + log(raskaustesti + 1) + log(raskausoireet + 1) + log(alkuraskaus + 1)
    )
  )

tidy(fits)[, c(1, 2, 3, 4, 6)] %>%
  kable(
    format = "html",
    table.attr = "style='width:75%;' ",
    caption = "Model Estimation Results",
    digits = 3
  ) %>%
  kable_classic_2(full_width = F)


fits_split <- train %>%
  model(
    fit_GT_raskaustesti = TSLM(log(totalbirths) ~ 1 + lag(log(totalbirths)) + log(raskaustesti + 1)),
    fit_GT_raskausoireet = TSLM(log(totalbirths) ~ 1 + lag(log(totalbirths)) + log(raskausoireet + 1)),
    fit_GT_alkuraskaus = TSLM(log(totalbirths) ~ 1 + lag(log(totalbirths))+ log(alkuraskaus + 1)),
  )                   


tidy(fits_split)[, c(1, 2, 3, 4, 6)] %>%
  kable(
    format = "html",
    table.attr = "style='width:75%;' ",
    caption = "Model Estimation Results",
    digits = 3
  ) %>%
  kable_classic_2(full_width = F)

#Forecast Estimation and Results

accuracy(fcs, test)[, c(1, 2, 6, 7)] %>%
  kable(
    format = "html",
    table.attr = "style='width:50%;' ",
    caption = "Model Forecast Results",
    col.names = c("model", "type", "MPE", "MAPE"),
    digits = 3,
  ) %>%
  kable_classic_2(full_width = F)


```

### Selvitä lagin määrä, librarynä forecast aikasarjalla, miten performoi, selittävillä muuttujilla,
dataset %>% model(ets=ETS(box_cox(first,0.3)),
                  arima=ARIMA(log(first)),
                  snaive=SNAIVE(first)) %>% 
  forecast(h="12 months") %>% 
  autoplot(dataset)
