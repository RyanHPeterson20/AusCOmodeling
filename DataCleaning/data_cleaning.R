#base data cleaning for Aus Wildfire CO-climate modeling

#creates .csv and .rda files for external and internal use.

#notes: 
## for data cleaning and prep 

#libraries
##for date/data mgmt
suppressMessages(library(lubridate))

#import data 
setwd("~/CO_AUS/AusCOmodeling") 
#predictor data: from
nino.raw <- read.csv("Data/nino_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
iod.raw  <- read.csv("Data/iod_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
tsa.raw  <- read.csv("Data/tsa_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
aao.raw  <- read.csv("Data/aao_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
#TODO: add in OLR

#response data: MOPITT V9J WEDCEN for both regions
NEAus.raw <- read.csv("Data/NEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)
SEAus.raw <- read.csv("Data/SEAus_V9JMOPITT_weeklyanomalies_WEDCEN.csv", header = TRUE, stringsAsFactors = FALSE)

#load internal functions
source("Functions/dataclean_functions.R") # functions 

##setup##
NEAus.raw$time <- ymd(NEAus.raw$time)
SEAus.raw$time <- ymd(SEAus.raw$time)


#combine into a single pred df and single resp df
pred.df <- data.frame(nino.anom = nino.raw$sst, dmi.anom = iod.raw$dmi.anom, wtio.anom = iod.raw$wtio.anom,
                      etio.anom = iod.raw$etio.anom, tsa.anom = tsa.raw$sst, aao.anom = aao.raw$anom, 
                      date = nino.raw$date, week = nino.raw$week, year = year(nino.raw$date))
resp.df <- data.frame(NEAus.anom = NEAus.raw$anomaly_co, SEAus.anom = SEAus.raw$anomaly_co, 
                      date = NEAus.raw$time, week = epiweek(NEAus.raw$time), year = year(NEAus.raw$time))
#alternative response data for actual CO data and CO climatologies
resp.alt.df <- data.frame(NEAus.co = NEAus.raw$x_co, SEAus.co = SEAus.raw$x_co, NEAus.clim = NEAus.raw$x_co.climatology,
                          SEAus.clim = SEAus.raw$x_co.climatology, date = NEAus.raw$time, week = epiweek(NEAus.raw$time), 
                          year = year(NEAus.raw$time))

#get min and max dates
pred.min.date <- min(as_date(pred.df$date))
pred.max.date <- max(as_date(pred.df$date))

resp.min.date <- min(NEAus.raw$time)
resp.max.date <- max(NEAus.raw$time)

new.pred.min <- resp.min.date - weeks(52)

#get upper bound of response data
resp.df <- resp.df[which(resp.df$date <= pred.max.date), ]
resp.alt.df <- resp.alt.df[which(resp.alt.df$date <= pred.max.date), ]

#get lower bound for predictor data
pred.df <- pred.df[which(pred.df$date >= new.pred.min), ]

rownames(resp.df) <- NULL
rownames(resp.alt.df) <- NULL
rownames(pred.df) <- NULL

#export .csv for the base dfs
setwd("~/CO_AUS/AusCOmodeling/Data") 
#write csv
write.csv(pred.df, "pred_anoms.csv")
write.csv(resp.df, "resp_anoms.csv")
write.csv(resp.alt.df, "resp_alt_anoms.csv")


#prepare data for use in modeling

##clean up week 53 data
pred.week53 <- which(pred.df$week == 53)
resp.week53 <- which(resp.df$week == 53)

pred.df[,1:6] <- week53_avg(pred.df[,1:6], pred.week53)
pred.df <- pred.df[-pred.week53, ]

resp.df[,1:2] <- week53_avg(resp.df[,1:2], resp.week53)
resp.df <- resp.df[-resp.week53, ]

#TODO: add in resp.alt.df
rm( pred.week53, resp.week53)

#setup season data
##Note: we can adjust the seasonal boundaries here
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year)

#get seasons as 20..-20..
seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp.season <- paste0(season.years[i], "-", season.years[i+1])
  seasons <- c(seasons, temp.season)
}
rm(i, temp.season)

#create lag predictor lists and dfs
NE.laglist <- list()
#TODO: call functions

SE.laglist <- list()



#TODO: test functions here, then move everything over to dataclean_functions.R
#pass:
##resp.df
##pred.df
##season.weeks
##seasons

#setup: 

i <- season.weeks[1]
resp.temp <- resp.df[resp.df$week == i, ]

if (i <= 14) {
  resp.temp <- resp.temp[-which(resp.temp$year == min(resp.df$year)), ]
}

NEdf.temp <- data.frame(seasons, NEAus.anom = resp.temp$NEAus.anom)
SEdf.temp <- data.frame(seasons, SEAus.anom = resp.temp$SEAus.anom)

#min/max year for response week i
min.year <- min(resp.temp$year)
max.year <- max(resp.temp$year)

nino.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
dmi.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
wtio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
etio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
tsa.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
aao.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))

#olr.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))


week.vec <- c()
lag.vec <- c()

for (j in 1:52) {
  
lag.week <- i - j

if (lag.week <= 0) {
  lag.week <- 52 + lag.week
  lag.year <- c(min.year - 1, max.year -1)
} else {
  lag.year <- min.year #what did I do here?
  lag.year <- c(min.year, max.year)
}

#get lagged predictors
lag.pred <- pred.df[pred.df$week == j, ]
lag.pred <- lag.pred[which((lag.pred$year >= lag.year[1] & lag.pred$year <= lag.year[2])),]


nino.lag <- cbind(nino.lag, lag.pred$nino.anom)
dmi.lag <- cbind(dmi.lag, lag.pred$dmi.anom)
wtio.lag <- cbind(wtio.lag, lag.pred$wtio.anom)
etio.lag <- cbind(etio.lag, lag.pred$etio.anom)
tsa.lag <- cbind(tsa.lag, lag.pred$tsa.anom)
aao.lag <- cbind(aao.lag, lag.pred$aao.anom)

#olr.lag <- cbind(olr.lag, lag.olr$olr.anom)

week.vec <- c(week.vec, lag.week)
lag.vec <- c(lag.vec, j)

}


#test function: 
#TODO: move to dataclean_functions.R when done
pred_lags <- function(resp.df, pred.df, season.weeks, seasons){
  
  for (i in season.weeks) {
    
    resp.temp <- resp.df[resp.df$week == i, ]
    
    if (i <= 14) {
      resp.temp <- resp.temp[-which(resp.temp$year == min(resp.df$year)), ]
    }
    
    NEdf.temp <- data.frame(seasons, NEAus.anom = resp.temp$NEAus.anom)
    SEdf.temp <- data.frame(seasons, SEAus.anom = resp.temp$SEAus.anom)
    
    #min/max year for response week i
    min.year <- min(resp.temp$year)
    max.year <- max(resp.temp$year)
    
    nino.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    dmi.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    wtio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    etio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    tsa.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    aao.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    #olr.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    
    week.vec <- c()
    lag.vec <- c()
    for (j in 1:52) {
      lag.week <- i - j
      
      if (lag.week <= 0) {
        lag.week <- 52 + lag.week
        lag.year <- c(min.year - 1, max.year -1)
      } else {
        lag.year <- min.year #what did I do here?
        lag.year <- c(min.year, max.year)
      }
      
      #get lagged predictors
      lag.pred <- pred.df[pred.df$week == j, ]
      lag.pred <- lag.pred[which((lag.pred$year >= lag.year[1] & lag.pred$year <= lag.year[2])),]
      
      nino.lag <- cbind(nino.lag, lag.pred$nino.anom)
      dmi.lag <- cbind(dmi.lag, lag.pred$dmi.anom)
      wtio.lag <- cbind(wtio.lag, lag.pred$wtio.anom)
      etio.lag <- cbind(etio.lag, lag.pred$etio.anom)
      tsa.lag <- cbind(tsa.lag, lag.pred$tsa.anom)
      aao.lag <- cbind(aao.lag, lag.pred$aao.anom)
      #olr.lag <- cbind(olr.lag, lag.olr$olr.anom)
      
      week.vec <- c(week.vec, lag.week)
      lag.vec <- c(lag.vec, j)
    }
    
    
  }
  
}


