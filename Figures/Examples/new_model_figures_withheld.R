##repeated prediction outputs for several different withheld years

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data

#setup (seasons)


