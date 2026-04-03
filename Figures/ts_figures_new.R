##time series figures 

#create ts sub-figures for figure 1

#libraries
suppressMessages( library(fields)) #envelope plot
suppressMessages( library(scales)) #for alpha()
suppressMessages( library(lubridate)) #for temporal data
suppressMessages( library(rcartocolor)) #color choices

#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda")
#load .csv data
pred.raw <- read.csv("Data/pred_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
resp.raw  <- read.csv("Data/resp_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
resp.alt.raw  <- read.csv("Data/resp_alt_anoms.csv", header = TRUE, stringsAsFactors = FALSE) #raw and clim avg. 

#color setup
#response colors
top.col.resp <- "#C71C1C"
bot.col.resp <- "#2A5674FF"

## response ts ##
resp.raw <- resp.raw[resp.raw$date <= "2021-01-06", ]



