# %load shiny-examples/063-superzip-example/global.R
library(dplyr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

#cleantable <- allzips %>%
#  select(
#    City = city.x,
#    State = state.x,
#    Zipcode = zipcode,
#    Rank = rank,
#    Score = centile,
#    Superzip = superzip,
#    Population = adultpop,
#    College = college,
#    Income = income,
#    Lat = latitude,
#    Long = longitude
#  )

cleantable <- powiatyDataLatLng


#zgonyCSV <- read.csv("bdl_data/zgony.csv",header=TRUE, sep=",")
#names(zgonyCSV)<-c("powiat","nowotwory","cukrzyca","zawal.serca","niewyd.ukl.oddech.")

#populacja<-read.csv("bdl_data/NARO_3304_CTAB_20181124191233.csv",header=TRUE,sep=";")