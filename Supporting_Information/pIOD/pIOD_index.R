
#positive IOD index from Cai et al (2021)

#libraries
suppressMessages( library(lubridate))

## import the appropriate data (weekly pca and iod)
#weekly PCA OISST data
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST")
load("pIODweekly_pca.rda")

#load in wtio/etio data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data

#TODO: find the old code (correlation_iod.rmd) for the time series and correlation plots 
#modified from correlation_iod.rmd
son.ind <- which(month(pred.df$date) %in% c(9,10,11))



