#get (and plot) RMSE differences

#as (all-data RMSE - withheld season RMSE), for multiple withheld seasons

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)


#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#loop through to get each year
SE.rmse <- SEvalid$rmse
SE.rmse.wo <- SErefit.wo.years$rmse

SE.rmse[[1]]
SE.rmse$`2001-2002`
SE.rmse.wo[[1]]
#test block
i <- 2
test.base.2001 <- SE.rmse[[i]]$base.pred
temp.wo2001 <- SE.rmse.wo[[i]]
early.wo2001 <- lapply(temp.wo2001, function(x)  x[1,])
peak.wo2001 <- lapply(temp.wo2001, function(x)  x[2,])
late.wo2001 <- lapply(temp.wo2001, function(x)  x[3,])

sapply(early.wo2001, function(x) test.base.2001[1]-x)
sapply(peak.wo2001, function(x) test.base.2001[2]-x)
sapply(late.wo2001, function(x) test.base.2001[3]-x)



i <- 1
for (i in 1:length(seasons)) {
  
}
lapply(SE.rmse.wo, function(x)  x[[i]])


