#base data cleaning for Aus Wildfire CO-climate modeling

#creates .csv and .rda files for external and internal use.

#notes: 
## for data cleaning and prep 

#libraries
##for date/data mgmt
suppressMessages(library(lubridate))

#TODO: import from ~/Data
#import as raw data 
setwd("~/CO_AUS/AusCOmodeling") 
#predictor data: from
nino.raw <- read.csv("Data/nino_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
iod.raw  <- read.csv("Data/iod_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
tsa.raw  <- read.csv("Data/tsa_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
aao.raw  <- read.csv("Data/aao_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)

#response data: MOPITT V9J WEDCEN for both regions
NEAus.raw <- read.csv("Data/NEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)
SEAus.raw <- read.csv("Data/SEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)

#TODO: load in functions as needed (also move any internal functions over)



