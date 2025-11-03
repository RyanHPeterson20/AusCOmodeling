#base data cleaning for Aus Wildfire CO-climate modeling

#creates .csv and .rda files for external and internal use.

#notes: 
## for data cleaning and prep 

#libraries
suppressMessages(library( lubridate)) #for basic date management

#TODO: import from ~/Data
#import as raw data 
setwd("~/CO_AUS/AusCOmodeling") 
#predictor data:


#response data: MOPITT V9J WEDCEN for both regions
#for north-east Australia
NEAus.raw <- read.csv("Data/NEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)
#for south-east Australia
SEAus.raw <- read.csv("Data/SEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)

#TODO: load in functions as needed (also move any internal functions over)



