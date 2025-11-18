#preparing OLR data for understanding MJO effects (following Daniels et al, 2022)

#notes:
## region boundaries:
#lat: (10N, 10S); lon: (90, 160)

#libraries
suppressMessages( library(lubridate))
suppressMessages( library(ncdf4))

#update with olr.day.mean.nc
setwd("~/CO_AUS/AusCOmodeling") 
#olr_nc_full <- nc_open('Data/olr.day.mean.nc')

setwd("~/CO_AUS/AusCOmodeling") 
#load internal functions
source("Functions/dataclean_functions.R") # functions 

#lat-lon boundaries
#Daniels et al, 2022 uses MSEA boundary for OLR-MJO
##lat: (10N, 10S); lon: (90, 160)
lat.range <- c(-10, 10) 
lon.range <- c(90, 160)

olr.full <- olr_prep(olr_nc_full, lon.range, lat.range)
time.full <- ncvar_get(olr_nc_full, "time")
time.full <- as_datetime("1800-01-01T00:00:00") + hours(time.full)

olr.daily.df <- data.frame(olr.mean = apply(olr.full, 3, mean), date = time.full, 
                           week = epiweek(time.full), year = year(time.full))

olr.temp.df <- olr.daily.df[olr.daily.df$date <= "2020-12-31", ]

#get climatological avg
day.vec <- seq(as_datetime("2020-01-1"), as_datetime("2020-12-31"), "day")


olr.clim.mat <- matrix(NA, ncol = 3)
colnames(olr.clim.mat) <- c("clim.avg", "month", "day")

olr.anom <- c(length = length(olr.daily.df$olr.mean))

for (i in 1:length(day.vec)) {
  #1974-2020 clim .avg
  day.select <- month(olr.temp.df$date) == month(day.vec[i]) & day(olr.temp.df$date) == day(day.vec[i])
  olr.clim <- olr.temp.df[day.select, ]
  clim.avg <- mean(olr.clim$olr.mean, na.rm = TRUE)
  
  olr.clim.mat <- rbind(olr.clim.mat, c(clim.avg, month(day.vec[i]), day(day.vec[i])))
  
  day.select.full <- month(olr.daily.df$date) == month(day.vec[i]) & day(olr.daily.df$date) == day(day.vec[i])
  
  #get daily anoms
  olr.anom[day.select.full]  <- olr.daily.df[day.select.full, ]$olr.mean - clim.avg
}

olr.clim.mat <- olr.clim.mat[-1, ]

rm(i, olr.temp.df)

olr.df <- data.frame(olr.anom = olr.anom, date = olr.daily.df$date, week = olr.daily.df$week, year = olr.daily.df$year)
olr.df$wday <- wday(olr.df$date)

#wedcen weekly avg
n.weeks <- length(which(olr.df$wday == 1))
i <- 2 #first sun
j <- 8 #first following sat
olr.wk.avg <- mean(olr.df$olr.anom[i:j])
i.new <- j+1
j.new <- j+7
for (k in 2:n.weeks) {
  olr.wk.avg <- c(olr.wk.avg, mean(olr.df$olr.anom[i.new:j.new], na.rm = TRUE))
  i.new <- j.new+1
  j.new <- j.new+7
}

rm(i, j, i.new, j.new, k)

olr.anom.df <- olr.df[which(olr.df$wday == 4), -5]
olr.anom.df$olr.anom <- olr.wk.avg

#update time range between 2000-01-05 and 
olr.anom.df <- olr.anom.df[olr.anom.df$year >= 2000 & olr.anom.df$date <= "2021-04-07", ]


#export data
setwd("~/CO_AUS/AusCOmodeling/Data") 
#write csv
write.csv(olr.anom.df, "olr_weekly_anoms.csv")

