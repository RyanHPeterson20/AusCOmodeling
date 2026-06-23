
#temporal autocorrelation

#libraries
suppressMessages( library( dplyr)) 
suppressMessages( library( scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits

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



df <- df[order(df$year, df$week), ]
df$resid <- residuals(your_ols_model)

