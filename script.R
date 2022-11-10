## Pekka Haimi Master Thesis work 2022
#Installing packages
#install.packages("remotes")
#install.packages("rmarkdown")

#remotes::install_github("trendecon/trendecon")


## Loading needed libraries
library(trendecon)
library(rmarkdown)

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

proc_keyword_init("raskaustesti", "FI")
proc_keyword_init("clearblue", "FI")
proc_keyword_init("ovulaatiotesti","FI")
proc_keyword_init("raskaana","FI")

##Downloading detailed and consistent data with TrendEcon

dc_clearblue <- ts_gtrends_mwd(c("ClearBlue"), geo = "FI",to = "2022-07-31")

## Saving the data frame
write.csv(dc_clearblue, "dc_clearblue.csv", row.names=FALSE, quote=FALSE,) 

## Doing this to every keyword
dc_hcg <- ts_gtrends_mwd(c("HCG"), geo = "FI")
write.csv(dc_hcg, "dc_hcg.csv", row.names=FALSE, quote=FALSE) 

dc_ivf <- ts_gtrends_mwd(c("IVF"), geo = "FI")
write.csv(dc_ivf, "dc_ivf.csv", row.names=FALSE, quote=FALSE) 

dc_raskauspahoinvointi <- ts_gtrends_mwd(c("raskauspahoinvointi"), geo = "FI")
write.csv(dc_raskauspahoinvointi, "dc_raskauspahoinvointi.csv", row.names=FALSE, quote=FALSE)

dc_ovulaatio <- ts_gtrends_mwd(c("ovulaatio"), geo = "FI")
write.csv(dc_ovulaatio, "dc_ovulaatio.csv", row.names=FALSE, quote=FALSE)

dc_ovulaatiotesti <- ts_gtrends_mwd(c("ovulaatiotesti"), geo = "FI")
write.csv(dc_ovulaatiotesti, "dc_ovulaatiotesti.csv", row.names=FALSE, quote=FALSE)

dc_raskaus <- ts_gtrends_mwd(c("raskaus"), geo = "FI")
write.csv(dc_raskaus, "dc_raskaus.csv", row.names=FALSE, quote=FALSE)

dc_raskaana <- ts_gtrends_mwd(c("raskaana"), geo = "FI")
write.csv(dc_raskaana, "dc_raskaana.csv", row.names=FALSE, quote=FALSE)

dc_raskaustesti <- ts_gtrends_mwd(c("raskaustesti"), geo = "FI")
write.csv(dc_raskaustesti, "dc_raskaustesti.csv", row.names=FALSE, quote=FALSE)

dc_synnytys <- ts_gtrends_mwd(c("synnytys"), geo = "FI")
write.csv(dc_synnytys, "dc_synnytys.csv", row.names=FALSE, quote=FALSE)

dc_ultra <- ts_gtrends_mwd(c("ultraääni"), geo = "FI")
write.csv(dc_ultra, "dc_ultra.csv", row.names=FALSE, quote=FALSE)



