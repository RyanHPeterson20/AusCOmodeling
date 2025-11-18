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
#add in OLR
olr.raw <- read.csv("Data/olr_weekly_anoms.csv", header = TRUE, stringsAsFactors = FALSE)

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
pred.df <- data.frame(pred.df[,c(1:6)], olr.anom = olr.raw$olr.anom, pred.df[,c(7:9)] )

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

pred.df[,1:6] <- week53_avg(pred.df[,1:7], pred.week53)
pred.df <- pred.df[-pred.week53, ]

resp.df[,1:2] <- week53_avg(resp.df[,1:2], resp.week53)
resp.df <- resp.df[-resp.week53, ]

#TODO: repeat for resp.alt.df


rm( pred.week53, resp.week53)

#setup season data
##Note: we can adjust the seasonal boundaries here
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year)
season.years.pred <- unique(pred.df$year)

#get seasons as 20..-20..
seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp.season <- paste0(season.years[i], "-", season.years[i+1])
  seasons <- c(seasons, temp.season)
}
rm(i, temp.season)


#TODO: temp pred lag work, delete when done
#checking pred_lags(): internals from function; compare to NEAus.lag week 38
i <- season.weeks[1]

#response setup
resp.temp <- resp.df[resp.df$week == i, ]
NEdf.temp <- data.frame(seasons, NEAus.anom = resp.temp$NEAus.anom)

#min/max year for response week i
min.year <- min(resp.temp$year)
#max.year <- max(resp.temp$year)
max.year <- 2019

##predictor lag setup
nino.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))

lag.vec <- c()
#j <- 1 #lag 1
for (j in 1:52) {
  lag.week <- i - j
  
  if (lag.week <= 0) {
    lag.week <- 52 + lag.week
    lag.year <- c(min.year - 1, max.year -1)
  } else {
    lag.year <- c(min.year, max.year)
  }
  
  lag.pred <- pred.df[pred.df$week == lag.week, ]
  
  lag.pred$year >= lag.year[1]
  lag.pred$year <= lag.year[2]
  lag.pred <- lag.pred[which((lag.pred$year >= lag.year[1] & lag.pred$year <= lag.year[2])), ]
  
  nino.lag <- cbind(nino.lag, lag.pred$nino.anom)
  
  lag.vec <- c(lag.vec, j)
}


nino.lag <- nino.lag[,-1]
colnames(nino.lag) <- paste0("nino_lag", lag.vec[1:52])
nino.lag <- scale(nino.lag, center = TRUE, scale = TRUE)



#get lags for data
data.lags <- pred_lags(resp.df, pred.df, season.weeks, seasons)
NEAus.lag <- data.lags$NElag
SEAus.lag <- data.lags$SElag

#output resp and pred dfs
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(pred.df, resp.df, file = "modeldata.rda")

#output lag data as .rda
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(NEAus.lag, SEAus.lag, file = "lagdata.rda")
#load("lagdata.rda")

## data matrix
#response data matrix
#resp matrix [19x64] rows : year, columns : (NE Aus, SE Aus) weeks

resp.matrix <- matrix(NA, ncol = 58) #29 weeks for each season
colnames(resp.matrix) <- c(paste0("NEAus", season.weeks), paste0("SEAus", season.weeks))

for (j in season.years[1:20]) {
  temp.resp1 <- resp.df[resp.df$year == j, ]
  season.1 <- temp.resp1[temp.resp1$week %in% 38:52,]
  
  temp.resp2 <- resp.df[resp.df$year == j+1, ]
  season.2 <- temp.resp2[temp.resp2$week %in% 1:14, ]
  
  resp.matrix <- rbind(resp.matrix, c(season.1$NEAus.anom, season.2$NEAus.anom,
                                      season.1$SEAus.anom, season.2$SEAus.anom))
}

resp.matrix <- resp.matrix[-1, ]
rownames(resp.matrix) <- seasons

rm(j, temp.resp1, temp.resp2, season.1, season.2)

#predictor data matrix
#pred matrix [20x312] rows: years; columns: climate mode weeks

pred.matrix <- matrix(NA, ncol = 364)
colnames(pred.matrix) <- c(paste0("Nino_", 1:52), paste0("DMI_", 1:52), paste0("WTIO_", 1:52),
                           paste0("ETIO_", 1:52), paste0("TSA_", 1:52), paste0("AAO_", 1:52),
                           paste0("OLR_", 1:52))

for (k in season.years.pred[1:21]) {
  temp.pred <- pred.df[pred.df$year == k, ]
  pred.matrix <- rbind(pred.matrix, c(temp.pred$nino.anom, temp.pred$dmi.anom, 
                                      temp.pred$wtio.anom, temp.pred$etio.anom,
                                      temp.pred$tsa.anom, temp.pred$aao.anom,
                                      temp.pred$olr.anom))
}

pred.matrix <- pred.matrix[-1, ]
rownames(pred.matrix) <- season.years.pred[1:21]

rm(k, temp.pred)

#output matrices as .rda
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(resp.matrix, pred.matrix, file = "matrixdata.rda")
