---
title: "Google Trends and Births"
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
 



```r
## Loading libraries
library(trendecon)

## Importing birth and population data and downloading Google Trends data 
birthsandpopulation <- read.csv("birthsandpopulationY04Y22.csv")

##Defining keywords for the analysis
s_terms <- c("ClearBlue", "HCG", "IVF", "raskauspahoinvointi", "ovulaatio", "ovulaatiotesti", "raskaus", "raskaana", "raskaustesti", "synnytys", "ultraääni")


##Downloading basic google trends
basic_trends <- ts_gtrends(
                keyword = s_terms,
                time ="2004-01-01 2022-07-31",
                geo ="FI"
)
```

```
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 10 seconds
```

```
## Retrying...
```

```
## Attempt 3/5
```

```
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 10 seconds
```

```
## Retrying...
```

```
## Attempt 3/5
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 15 seconds
```

```
## Retrying...
```

```
## Attempt 4/5
```

```
## Downloading data for 2004-01-01 2022-07-31
## Downloading data for 2004-01-01 2022-07-31
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 10 seconds
```

```
## Retrying...
```

```
## Attempt 3/5
```

```
## Downloading data for 2004-01-01 2022-07-31
## Downloading data for 2004-01-01 2022-07-31
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```
## Downloading data for 2004-01-01 2022-07-31
## Downloading data for 2004-01-01 2022-07-31
```

```
## Server response: 429 - too many requests
```

```
## Waiting for 5 seconds
```

```
## Retrying...
```

```
## Attempt 2/5
```

```r
##Downloading detailed and consistent data with TrendEcon and saving it as CSV

##dc_clearblue <- ts_gtrends_mwd(c("ClearBlue"), geo = "FI")
```

