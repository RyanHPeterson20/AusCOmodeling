
#time-series figures for figure 3 and SI.

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(lubridate))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix (resp.matrix, etc.)
load("Data/modeldata.rda") #resp/pred data 
load("Data/lagdata.rda") #lagged data (SEAus.lag, etc.)
load("Data/base_RAMPmodels.rda") #"base" model (e.g., SEmodels)

#functions
source("Functions/modeling_functions.R")
source("Figures/pred_ts_plot_functions.R") #testing new figure automation,

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

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14




