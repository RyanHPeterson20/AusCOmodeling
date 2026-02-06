
#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(lubridate))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
source("Functions/modeling_functions.R")

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

SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

#get lag 1-52 from week 51 and lags 1-3 from week 2 (for the peak group)
SEpreds.peak <- SE.pred$mid
SEpreds.peak51 <- SEAus.lag$`Week  51`
SEpreds.peak2 <- SEAus.lag$`Week  2`

#up to lag 52
SEpreds.peak51.nino <- SEpreds.peak51[ ,3:54]
SEpreds.peak51.wtio <- SEpreds.peak51[ ,107:158]
SEpreds.peak51.etio <- SEpreds.peak51[ ,159:210]
SEpreds.peak51.tsa <- SEpreds.peak51[ ,211:262] 
SEpreds.peak51.aao <- SEpreds.peak51[ ,263:314]
SEpreds.peak51.olr <- SEpreds.peak51[ ,315:366]
#only up to lag 3
SEpreds.peak2.nino <- SEpreds.peak2[ ,3:5]
SEpreds.peak2.wtio <- SEpreds.peak2[ ,107:109]
SEpreds.peak2.etio <- SEpreds.peak2[ ,159:161]
SEpreds.peak2.tsa <- SEpreds.peak2[ ,211:213] 
SEpreds.peak2.aao <- SEpreds.peak2[ ,263:265]
SEpreds.peak2.olr <- SEpreds.peak2[ ,315:317]

#all lags together
SEpreds.peak.nino <- cbind(SEpreds.peak2.nino, SEpreds.peak51.nino)
SEpreds.peak.wtio <- cbind(SEpreds.peak2.wtio, SEpreds.peak51.wtio)
SEpreds.peak.etio <- cbind(SEpreds.peak2.etio, SEpreds.peak51.etio)
SEpreds.peak.tsa <- cbind(SEpreds.peak2.tsa, SEpreds.peak51.tsa)
SEpreds.peak.aao <- cbind(SEpreds.peak2.aao, SEpreds.peak51.aao)
SEpreds.peak.olr <- cbind(SEpreds.peak2.olr, SEpreds.peak51.olr)

#time/date setup

#for 2001/2002 peak season
pred.df[pred.df$week == 1 & pred.df$year == 2002, ]$date  #lag 1 week 2
pred.df[pred.df$week == 50 & pred.df$year == 2001, ]$date #lag 1 week 51
start.2001 <- pred.df[pred.df$week == 51 & pred.df$year == 2000, ]$date #lag 52 week 51

pred.df[51:105, ]$date



##----- time series -----##

#ts setup
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"


#2001/2002 pred data
nino.anom.2001 <- as.numeric(rev(SEpreds.peak.nino[1, ]))
wtio.anom.2001 <- as.numeric(rev(SEpreds.peak.wtio[1, ]))
etio.anom.2001 <- as.numeric(rev(SEpreds.peak.etio[1, ]))
tsa.anom.2001 <- as.numeric(rev(SEpreds.peak.tsa[1, ]))
aao.anom.2001 <- as.numeric(rev(SEpreds.peak.aao[1, ]))
olr.anom.2001 <- as.numeric(rev(SEpreds.peak.olr[1, ]))




#temp functions



#TODO: test code delete when done.

#select window
date.start <- ymd(start.2001)
date.end <- date.start + weeks(54)

pred.dates <- pred.df[pred.df$date >= date.start & pred.df$date <= date.end, ]

pred.time <- as.Date(pred.dates$date)
pred.time.range <- range(pred.time)

x.ticks.pred <- seq(
  floor_date(pred.time.range[1], unit = "month"),
  ceiling_date(pred.time.range[2], unit = "month"),
  by = "1 month"
)

x.labs.pred <- ifelse(month(x.ticks.pred) == 1,
                      format(x.ticks.pred, "%b\n%Y"),
                      format(x.ticks.pred, "%b"))

# Optional: vertical lines at year boundaries
x.year.pred <- ymd(paste0(seq(year(pred.time.range[1]), year(pred.time.range[2])), "0101"))


### plots to explore specific (and all) years.


#time series plots for interesting years 2001, 2010, & 2011 (and 2005, 2015)


