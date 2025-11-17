
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
nino.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/nino34.nc") 
#DMI
dmi.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/dmi.nc")
#WTIO
wtio.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/wtio.nc")
#ETIO
etio.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/setio.nc")
#TSA
tsa.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/tsa.nc") 
#SAM (AAO) 
sam.nc <- nc_open("~/CO_AUS/AusCOmodeling/Data/sam.nc") #monthly

#AAO csv
aao.csv  <- read.csv("Data/norm.daily.aao.cdas.z700.19790101_current.csv", header = TRUE, stringsAsFactors = FALSE) #daily

##setup##

##prep .nc data 
#get times from .nc files
time.nino <- ncvar_get(nino.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.nino <- as_datetime("1900-01-01T00:00:00") + days(time.nino)

time.dmi <- ncvar_get(dmi.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.dmi <- as_datetime("1900-01-01T00:00:00") + days(time.dmi)

time.wtio <- ncvar_get(wtio.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.wtio <- as_datetime("1900-01-01T00:00:00") + days(time.wtio)

time.etio <- ncvar_get(etio.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.etio <- as_datetime("1900-01-01T00:00:00") + days(time.etio)

time.tsa <- ncvar_get(tsa.nc, "WEDCEN2") # days since 1900-01-01 00:00:0.0
time.tsa <- as_datetime("1900-01-01T00:00:00") + days(time.tsa)

time.sam <- ncvar_get(sam.nc, "TSAXIS") # days since 1900-01-01 00:00:0.0
time.sam <- as_datetime("1900-01-01T00:00:00") + days(time.sam)

#sst weekly data
sst.nino <- ncvar_get(nino.nc, "NINO34")
sst.dmi <- ncvar_get(dmi.nc, "DMI")
sst.wtio <- ncvar_get(wtio.nc, "WTIO")
sst.etio <- ncvar_get(etio.nc, "SETIO")
sst.tsa <- ncvar_get(tsa.nc, "TSA")
#sam monthyl data
sst.sam <- ncvar_get(sam.nc, "SAM") #monthly data

#create climate mode dfs
#get df
nino.df <- data.frame(sst = sst.nino, date = time.nino, week = epiweek(time.nino))
dmi.df <- data.frame(sst = sst.dmi, date = time.dmi, week = epiweek(time.dmi))
wtio.df <- data.frame(sst = sst.wtio, date = time.wtio, week = epiweek(time.wtio))
etio.df <- data.frame(sst = sst.etio, date = time.etio, week = epiweek(time.etio))
tsa.df <- data.frame(sst = sst.tsa, date = time.tsa, week = epiweek(time.tsa))

#combine dmi, wtio, etio into a single iod.df
iod.df <- data.frame(dmi.anom = dmi.df$sst, wtio.anom = wtio.df$sst, etio.anom = etio.df$sst,
                     date = dmi.df$date, week = dmi.df$week)


sam.monthly.df <- data.frame(sst = sst.sam, date = time.sam)

aao.date <- as_datetime(paste(aao.csv$year, aao.csv$month, aao.csv$day))
aao.df <- aao.csv
aao.df$date <- aao.date
aao.df$week <- epiweek(aao.date)
aao.df$wday <- wday(aao.date)
aao.df <- aao.df[1:17100, ]


weeks.n <- length(which(aao.df$wday == 7))-1
#it's messy but it kinda works
i <- 1
j <- 6
aao.avg <- mean(aao.df$aao_index_cdas[i:j])
i.new <- j+1
j.new <- j+7
for (k in 1:weeks.n) {
  aao.avg <- c(aao.avg, mean(aao.df$aao_index_cdas[i.new:j.new], na.rm = TRUE))
  i.new <- j.new+1
  j.new <- j.new+7
}

rm(i, i.new, j, j.new, k, weeks.n)

aao.temp.df <- aao.df[which(aao.df$wday == 4), ]
sam.df <- data.frame(anom = aao.avg, date = aao.temp.df$date, week = aao.temp.df$week)

sam.min.date <- which(sam.df$date == min(dmi.df$date))
sam.max.date <- which(sam.df$date == max(dmi.df$date))

sam.df <- sam.df[c(sam.min.date:sam.max.date), ]

rm(aao.temp.df, sam.min.date, sam.max.date)

#export data
setwd("~/CO_AUS/AusCOmodeling/Data") 
#write csv
write.csv(nino.df, "nino_weekly_anoms.csv")
write.csv(iod.df, "iod_weekly_anoms.csv")
write.csv(tsa.df, "tsa_weekly_anoms.csv")
write.csv(sam.df, "aao_weekly_anoms.csv")

