
#get weekly OISST data for comparison

#libraries
#.nc files
suppressMessages(library(ncdf4))
suppressMessages(library(terra))

# date/data mgmt
suppressMessages(library(lubridate))
suppressMessages(library(abind))

#data load
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST")
#1990-present
nc.OISST.weekly1 <- nc_open("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST/sst.wkmean.1990-present.nc")
#1981-1989
nc.OISST.weekly2 <- nc_open("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST/sst.wkmean.1981-1989.nc")
#OISST land sea mask
nc.OISST.lsm <- nc_open("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST/lsmask.nc")

#function import
#TODO: update pIOD_functions.R with necessary functions
setwd("~/CO_AUS/AusCOmodeling/Functions")
source("pIOD_functions.R")


#setup
#indian ocean region (includes 1 deg buffer)
wide_maxLon <- 121
wide_minLon <- 29
wide_maxLat <- 21
wide_minLat <- -21

lon.wide <- seq(wide_minLon, wide_maxLon, by = 1)
lat.wide <- seq(wide_minLat, wide_maxLat, by = 1)

#grid list from 
grid.list <- list(x = lon.wide,
                  y = lat.wide)

#pIOD (40, -5) X (100, 5)
pIOD_maxLon <- 100
pIOD_minLon <- 40 
pIOD_maxLat <- 5
pIOD_minLat <- -5


#weekly 1:1990-present (2023)
#extract lon, lat
lat.grid <- nc.OISST.weekly1[["dim"]][["lat"]][["vals"]]
lon.grid <- nc.OISST.weekly1[["dim"]][["lon"]][["vals"]]

#reorder lon
lon.grid[lon.grid >= 180] <- lon.grid[lon.grid >= 180] - 360
lon.order <- order(lon.grid)

lon.grid <- lon.grid[lon.order]

#reorder lat (maybe?)
lat.order <- order(lat.grid)
lat.grid <- lat.grid[lat.order]

#time data (sunday centered)
times1 <- ncvar_get(nc.OISST.weekly1, "time") # days since 1800-01-01 00:00:0.0
times1 <- as_datetime("1800-01-01T00:00:00") + days(times1)
times1 <- times1 + days(3) #wedcen
#wday(times1, label = TRUE)

#sst data, as array (lon, lat, time)
sst.oisst.weekly1 <- ncvar_get(nc.OISST.weekly1, "sst")
sst.oisst.weekly1 <- sst.oisst.weekly1[lon.order, lat.order, ]

#weekly 2:1981-1989
lat.grid <- nc.OISST.weekly2[["dim"]][["lat"]][["vals"]]
lon.grid <- nc.OISST.weekly2[["dim"]][["lon"]][["vals"]]

#reorder lon
lon.grid[lon.grid >= 180] <- lon.grid[lon.grid >= 180] - 360
lon.order <- order(lon.grid)

lon.grid <- lon.grid[lon.order]

#reorder lat (maybe?)
lat.order <- order(lat.grid)
lat.grid <- lat.grid[lat.order]

#time data (thursday centered)
times2 <- ncvar_get(nc.OISST.weekly2, "time") # days since 1800-01-01 00:00:0.0
times2 <- as_datetime("1800-01-01T00:00:00") + days(times2)

times2 <- times2 - days(1) #wedcen
#wday(times2, label = TRUE)

#sst data, as array (lon, lat, time)
sst.oisst.weekly2 <- ncvar_get(nc.OISST.weekly2, "sst")
sst.oisst.weekly2 <- sst.oisst.weekly2[lon.order, lat.order, ]

#combine 
sst.oisst.weekly <- abind(sst.oisst.weekly2, sst.oisst.weekly1, along = 3)
dim(sst.oisst.weekly)

times.oisst <- c(times2, times1)

lat.range.base <- range(which(lat.grid <= (wide_maxLat) & lat.grid >= (wide_minLat)))
lon.range.base <- range(which(lon.grid <= (wide_maxLon) & lon.grid >= (wide_minLon)))

sst.oisst.weekbase <- sst.oisst.weekly[lon.range.base[1]:lon.range.base[2], lat.range.base[1]:lat.range.base[2], ]
#dim(sst.oisst.weekbase)


##Land Sea Mask
#extract lon, lat
lat.grid.lsm <- nc.OISST.lsm[["dim"]][["lat"]][["vals"]]
lon.grid.lsm <- nc.OISST.lsm[["dim"]][["lon"]][["vals"]]

#reorder lon for 
lon.grid.lsm[lon.grid.lsm >= 180] <- lon.grid.lsm[lon.grid.lsm >= 180] - 360
this.order.lsm <- order(lon.grid.lsm)

lon.grid.lsm <- lon.grid.lsm[this.order.lsm]

#setup (or find) a land sea mask.
lsm <- ncvar_get(nc.OISST.lsm, "mask")
lsm <- lsm[this.order.lsm, ]

#get lsm for IOD only
lsm_IOD <- lsm[lon.range.base[1]:lon.range.base[2], lat.range.base[1]:lat.range.base[2]] 

lsm.IOD <- lsm_IOD[ ,ncol(lsm_IOD):1]

#setup (land sea mask for OISST data)
lsm.IOD.array <- array(lsm.IOD, dim = dim(sst.oisst.weekbase))

#get lsm mask
sst.oisst.weeklynew <- ifelse(lsm.IOD.array == 1, sst.oisst.weekbase, NA)

dim(sst.oisst.weeklynew) #lon, lat, time
length(times.oisst) #actual dates (wedcen)


## prep weekly OISST data

#get min as 1982
year1982 <- which(year(times.oisst) == 1982)
minweek.ind <- min(year1982)
min.week <- times.oisst[minweek.ind]

#get max as 2015
year2015 <- which(year(times.oisst) == 2015)
maxweek.ind <- max(year2015)
max.week <- times.oisst[maxweek.ind]

#extend to April 2021 *week 14
year2021 <- which(year(times.oisst) == 2021)
#get isoweek/epiweek to check for week 14
endweek.ind <- year2021[which(epiweek(times.oisst[year2021]) == 14)]
end.week <- times.oisst[endweek.ind]

#get 1982-2015
times.base <- times.oisst[minweek.ind:maxweek.ind]
sst.weekly.base <- sst.oisst.weeklynew[,,minweek.ind:maxweek.ind] 

#get 1982-2021
times.new <- times.oisst[minweek.ind:endweek.ind]
sst.weekly.new <- sst.oisst.weeklynew[,,minweek.ind:endweek.ind] 


#select for weeks in a given season (1982-2015) 
#w/ index
son.weeks <- which(month(times.base) %in% c(9, 10, 11))

#w/ dates
son.dates <- times.base[son.weeks]

#get list of years for each season
son.years <- year(son.dates)

#get seasonal weeks of sst data
son.sst.weekly <- sst.weekly.base[,,son.weeks]

years.base <- 1982:2015


#for new 1982-2021
#w/ index
son.weeks.new <- which(month(times.new) %in% c(9, 10, 11))

#w/ dates
son.dates.new <- times.new[son.weeks.new]

#get list of years for each season
son.years.new <- year(son.dates.new)

#get seasonal weeks of sst data
son.sstnew.weekly <- sst.weekly.new[,,son.weeks.new]

years.new <- 1982:2021








