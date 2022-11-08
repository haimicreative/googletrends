## Pekka Haimi Master Thesis work 2022
#Installing packages
#install.packages("remotes")
#remotes::install_github("trendecon/trendecon")


## Loading needed libraries
library(trendecon)

## Importing birth and population data

birthsandpopulation <- read.csv("birthsandpopulationY04Y22.csv")
View(birthsandpopulation)

##Defining keywords for the analysis
s_terms <- c("ClearBlue", "HCG", "IVF", "raskauspahoinvointi", "ovulaatio", "ovulaatiotesti", "raskaus", "raskaana", "raskaustesti", "synnytys", "ultraääni")


##Downloading basic google trends
basic_trends <- ts_gtrends(
                keyword = s_terms,
                time ="2004-01-01 2022-07-31",
                geo ="FI"
)

##Downloading detailed and consistent data with TrendEcon

dc_clearblue <- ts_gtrends_mwd(c("ClearBlue"), geo = "FI")


