
#temporal autocorrelation

#libraries
suppressMessages( library( dplyr)) 
suppressMessages( library( scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
#load("Data/base_RAMPmodels.rda") #"base" model
#load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/std_RAMPmodels.rda") #updates lasso models

#TODO: update these notes after complete:
# Assuming your OLS model is fit and your data frame has columns:
# resid = model residuals
# year  = wildfire season year
# week  = calendar week number

#SEmodels
SE.early.lm <- SEmodels[[1]] 
SE.peak.lm <- SEmodels[[2]]
SE.late.lm <- SEmodels[[3]]


# Sort by year then week within year
#group weeks
SE.early <- 38:50
SE.peak <- c(51, 52, 1, 2)
SE.late <- 3:14


# Determine max lag based on number of weeks per year in this sub-season
max.early.lag <- length(SE.early) - 1  
max.peak.lag <- length(SE.peak) - 1  
max.late.lag <- length(SE.late) - 1  

early.resid <- residuals(SE.early.lm)

#get acf for each year
acf_early <- NULL
for (j in 1:20) {
  x <- early.resid[seq(j, 260, by = 20)]
  acf_early <- rbind(acf_early,
                 acf(x, lag.max = max.early.lag, plot = FALSE)$acf[-1])  # drop lag 0
}

colMeans(acf_early) #means by lag


peak.resid <- residuals(SE.peak.lm)

#get acf for each year
acf_peak <- NULL
for (j in 1:20) {
  x <- peak.resid[seq(j, 80, by = 20)]
  acf_peak <- rbind(acf_peak,
                     acf(x, lag.max = max.peak.lag, plot = FALSE)$acf[-1])  # drop lag 0
}

colMeans(acf_peak) #means by lag



late.resid <- residuals(SE.late.lm)

#get acf for each year
acf_late <- NULL
for (j in 1:20) {
  x <- late.resid[seq(j, 240, by = 20)]
  acf_late <- rbind(acf_late,
                    acf(x, lag.max = max.late.lag, plot = FALSE)$acf[-1])  # drop lag 0
}

colMeans(acf_late) #means by lag

