
#specific data cleaning for "raw" climate mode index data

#includes "updated" index data 

#libraries
##for .nc files
suppressMessages(library(ncdf4))
suppressMessages(library(terra))
##for date/data mgmt
suppressMessages(library(lubridate))

#TODO: import functions as needed

#import data
setwd("~/CO_AUS/AusCOmodeling") 

#NINO 3.4
nino.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/nino34.nc") #TODO: get the correct one
#DMI
dmi.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/dmi.nc")
#WTIO
wtio.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/wtio.nc")
#ETIO
etio.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/setio.nc")
#TSA
tsa.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/tsa.nc") #TODO: get the correct one
#SAM (AAO) 
sam.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/sam.nc") #monthly

#AAO csv
aao.csv  <- read.csv("Data/norm.daily.aao.cdas.z700.19790101_current.csv", header = TRUE, stringsAsFactors = FALSE)


##setup##

#get times from .nc files
time.sam <- ncvar_get(sam.nc, "TSAXIS") # days since 1900-01-01 00:00:0.0
time.sam <- as_datetime("1900-01-01T00:00:00") + days(time.sam)

time.dmi <- ncvar_get(dmi.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.dmi <- as_datetime("1900-01-01T00:00:00") + days(time.dmi)






