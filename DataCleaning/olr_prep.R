
#preparing OLR data for understanding MJO effects (following Daniels et al, 2022)

#notes:
## region boundaries:
#lat: (10N, 10S); lon: (90, 160)

#libraries
suppressMessages( library(lubridate))
suppressMessages( library(ncdf4))

#load raw olr data from .nc files
setwd("~/CO_AUS/AusCOmodeling") 
#1999-2021
olr_nc_1999 <- nc_open('Data/OLR/olr-daily_v01r02_19990101_19991231.nc') #1999
olr_nc_2000 <- nc_open('Data/OLR/olr-daily_v01r02_20000101_20001231.nc') #2000
olr_nc_2001 <- nc_open('Data/OLR/olr-daily_v01r02_20010101_20011231.nc') #2001
olr_nc_2002 <- nc_open('Data/OLR/olr-daily_v01r02_20020101_20021231.nc') #2002
olr_nc_2003 <- nc_open('Data/OLR/olr-daily_v01r02_20030101_20031231.nc') #2003
olr_nc_2004 <- nc_open('Data/OLR/olr-daily_v01r02_20040101_20041231.nc') #2004
olr_nc_2005 <- nc_open('Data/OLR/olr-daily_v01r02_20050101_20051231.nc') #2005
olr_nc_2006 <- nc_open('Data/OLR/olr-daily_v01r02_20060101_20061231.nc') #2006 
olr_nc_2007 <- nc_open('Data/OLR/olr-daily_v01r02_20070101_20071231.nc') #2007
olr_nc_2008 <- nc_open('Data/OLR/olr-daily_v01r02_20080101_20081231.nc') #2008
olr_nc_2009 <- nc_open('Data/OLR/olr-daily_v01r02_20090101_20091231.nc') #2009
olr_nc_2010 <- nc_open('Data/OLR/olr-daily_v01r02_20100101_20101231.nc') #2010
olr_nc_2011 <- nc_open('Data/OLR/olr-daily_v01r02_20110101_20111231.nc') #2011
olr_nc_2012 <- nc_open('Data/OLR/olr-daily_v01r02_20120101_20121231.nc') #2012
olr_nc_2013 <- nc_open('Data/OLR/olr-daily_v01r02_20130101_20131231.nc') #2013
olr_nc_2014 <- nc_open('Data/OLR/olr-daily_v01r02_20140101_20141231.nc') #2014
olr_nc_2015 <- nc_open('Data/OLR/olr-daily_v01r02_20150101_20151231.nc') #2015
olr_nc_2016 <- nc_open('Data/OLR/olr-daily_v01r02_20160101_20161231.nc') #2016
olr_nc_2017 <- nc_open('Data/OLR/olr-daily_v01r02_20170101_20171231.nc') #2017
olr_nc_2018 <- nc_open('Data/OLR/olr-daily_v01r02_20180101_20181231.nc') #2018 
olr_nc_2019 <- nc_open('Data/OLR/olr-daily_v01r02_20190101_20191231.nc') #2019
olr_nc_2020 <- nc_open('Data/OLR/olr-daily_v01r02_20200101_20201231.nc') #2020
olr_nc_2021 <- nc_open('Data/OLR/olr-daily_v01r02_20210101_20211231.nc') #2021

setwd("~/CO_AUS/AusCOmodeling") 
#load internal functions
source("Functions/dataclean_functions.R") # functions 

#lat-lon boundaries
#Daniels et al, 2022 uses MSEA boundary for OLR-MJO
##lat: (10N, 10S); lon: (90, 160)
lat.range <- c(-10, 10) 
lon.range <- c(90, 160)


#TODO: move to dataclean_functions.R
olr_prep <- function(olr_nc, regional_lon, regional_lat){
  lat_grid <- olr_nc[["dim"]][["lat"]][["vals"]]
  lon_grid <- olr_nc[["dim"]][["lon"]][["vals"]]
  
  olr <-  ncvar_get(olr_nc, "olr")
  
  #adjust lon grid:
  lon_grid[lon_grid >= 180] <- lon_grid[lon_grid >= 180] - 360
  
  lon_order <- order(lon_grid)
  lon_grid <- lon_grid[lon_order]
  
  olr <- olr[lon_order, , ]
  
  #bound within given region
  lon_min <- regional_lon[1]
  lon_max <- regional_lon[2]
  lat_min <- regional_lat[1]
  lat_max <- regional_lat[2]
  
  reg_lon <- which(lon_grid >= lon_min & lon_grid <= lon_max)
  reg_lat <- which(lat_grid >= lat_min & lat_grid <= lat_max)
  
  lon_grid <- lon_grid[reg_lon]
  lat_grid <- lat_grid[reg_lat]
  
  olr <- olr[reg_lon, reg_lat, ]
  
  return(olr)
}


#get data and time
olr.1999 <- olr_prep(olr_nc_1999, lon.range, lat.range)
time.1999 <- ncvar_get(olr_nc_1999, "time")
time.1999 <- as_datetime("1970-01-01 00:00:00") + days(time.1999-0.5)

olr.2000 <- olr_prep(olr_nc_2000, lon.range, lat.range)
time.2000 <- ncvar_get(olr_nc_2000, "time")
time.2000 <- as_datetime("1970-01-01 00:00:00") + days(time.2000-0.5)

olr.2001 <- olr_prep(olr_nc_2001, lon.range, lat.range)
time.2001 <- ncvar_get(olr_nc_2001, "time")
time.2001 <- as_datetime("1970-01-01 00:00:00") + days(time.2001-0.5)

olr.2002 <- olr_prep(olr_nc_2002, lon.range, lat.range)
time.2002 <- ncvar_get(olr_nc_2002, "time")
time.2002 <- as_datetime("1970-01-01 00:00:00") + days(time.2002-0.5)

olr.2003 <- olr_prep(olr_nc_2003, lon.range, lat.range)
time.2003 <- ncvar_get(olr_nc_2003, "time")
time.2003 <- as_datetime("1970-01-01 00:00:00") + days(time.2003-0.5)

olr.2004 <- olr_prep(olr_nc_2004, lon.range, lat.range)
time.2004 <- ncvar_get(olr_nc_2004, "time")
time.2004 <- as_datetime("1970-01-01 00:00:00") + days(time.2004-0.5)

olr.2005 <- olr_prep(olr_nc_2005, lon.range, lat.range)
time.2005 <- ncvar_get(olr_nc_2005, "time")
time.2005 <- as_datetime("1970-01-01 00:00:00") + days(time.2005-0.5)

olr.2006 <- olr_prep(olr_nc_2006, lon.range, lat.range)
time.2006 <- ncvar_get(olr_nc_2006, "time")
time.2006 <- as_datetime("1970-01-01 00:00:00") + days(time.2006-0.5)

olr.2007 <- olr_prep(olr_nc_2007, lon.range, lat.range)
time.2007 <- ncvar_get(olr_nc_2007, "time")
time.2007 <- as_datetime("1970-01-01 00:00:00") + days(time.2007-0.5)

olr.2008 <- olr_prep(olr_nc_2008, lon.range, lat.range)
time.2008 <- ncvar_get(olr_nc_2008, "time")
time.2008 <- as_datetime("1970-01-01 00:00:00") + days(time.2008-0.5)

olr.2009 <- olr_prep(olr_nc_2009, lon.range, lat.range)
time.2009 <- ncvar_get(olr_nc_2009, "time")
time.2009 <- as_datetime("1970-01-01 00:00:00") + days(time.2009-0.5)

olr.2010 <- olr_prep(olr_nc_2010, lon.range, lat.range)
time.2010 <- ncvar_get(olr_nc_2010, "time")
time.2010 <- as_datetime("1970-01-01 00:00:00") + days(time.2010-0.5)

olr.2011 <- olr_prep(olr_nc_2011, lon.range, lat.range)
time.2011 <- ncvar_get(olr_nc_2011, "time")
time.2011 <- as_datetime("1970-01-01 00:00:00") + days(time.2011-0.5)

olr.2012 <- olr_prep(olr_nc_2012, lon.range, lat.range)
time.2012 <- ncvar_get(olr_nc_2012, "time")
time.2012 <- as_datetime("1970-01-01 00:00:00") + days(time.2012-0.5)

olr.2013 <- olr_prep(olr_nc_2013, lon.range, lat.range)
time.2013 <- ncvar_get(olr_nc_2013, "time")
time.2013 <- as_datetime("1970-01-01 00:00:00") + days(time.2013-0.5)

olr.2014 <- olr_prep(olr_nc_2014, lon.range, lat.range)
time.2014 <- ncvar_get(olr_nc_2014, "time")
time.2014 <- as_datetime("1970-01-01 00:00:00") + days(time.2014-0.5)

olr.2015 <- olr_prep(olr_nc_2015, lon.range, lat.range)
time.2015 <- ncvar_get(olr_nc_2015, "time")
time.2015 <- as_datetime("1970-01-01 00:00:00") + days(time.2015-0.5)

olr.2016 <- olr_prep(olr_nc_2016, lon.range, lat.range)
time.2016 <- ncvar_get(olr_nc_2016, "time")
time.2016 <- as_datetime("1970-01-01 00:00:00") + days(time.2016-0.5)

olr.2017 <- olr_prep(olr_nc_2017, lon.range, lat.range)
time.2017 <- ncvar_get(olr_nc_2017, "time")
time.2017 <- as_datetime("1970-01-01 00:00:00") + days(time.2017-0.5)

olr.2018 <- olr_prep(olr_nc_2018, lon.range, lat.range)
time.2018 <- ncvar_get(olr_nc_2018, "time")
time.2018 <- as_datetime("1970-01-01 00:00:00") + days(time.2018-0.5)

olr.2019 <- olr_prep(olr_nc_2019, lon.range, lat.range)
time.2019 <- ncvar_get(olr_nc_2019, "time")
time.2019 <- as_datetime("1970-01-01 00:00:00") + days(time.2019-0.5)

olr.2020 <- olr_prep(olr_nc_2020, lon.range, lat.range)
time.2020 <- ncvar_get(olr_nc_2020, "time")
time.2020 <- as_datetime("1970-01-01 00:00:00") + days(time.2020-0.5)

olr.2021 <- olr_prep(olr_nc_2021, lon.range, lat.range)
time.2021 <- ncvar_get(olr_nc_2021, "time")
time.2021 <- as_datetime("1970-01-01 00:00:00") + days(time.2021-0.5)


#temp olr for 2020 and 2021 
time.prev <- time.2020
time.current <- time.2021

#get dates from base year
sun.dates.prev <- time.prev[which(wday(time.prev) == 1)]
wed.dates.prev <- time.prev[which(wday(time.prev) == 4)] 
sat.dates.prev <- time.prev[which(wday(time.prev) == 7)]

sun.dates <- time.current[which(wday(time.current) == 1)]
wed.dates <- time.current[which(wday(time.current) == 4)] 
sat.dates <- time.current[which(wday(time.current) == 7)]

#test the final weeks of 2020 and compare to msea_olr.csv
#load in data
setwd("~/CO_AUS/AusCOmodeling") 
#load .csv data
msea_olr.df <- read.csv("Data/msea_olr.csv", header = TRUE, stringsAsFactors = FALSE)

sun.dates.prev[epiweek(sun.dates.prev) == 51] #index 348
wed.dates.prev[epiweek(wed.dates.prev) == 51]
sat.dates.prev[epiweek(sat.dates.prev) == 51] #index 354

time.prev[348:354]



#TODO: get full_agg function working, move 
#goal is to spatial-temporal average and output as wedcen date

#params: 
## time.prev (time0) as previous year
## time.current (time1) as current year

##TODO: test internals (for 2000)

time.prev <- time.1999
time.current <- time.2000

#get dates from base year
epiweek(time.current)

sun.dates <- time.current[which(wday(time.current) == 1)]
wed.dates <- time.current[which(wday(time.current) == 4)] 
sat.dates <- time.current[which(wday(time.current) == 7)]

epiweek(sun.dates)
epiweek(wed.dates)
epiweek(sat.dates)

#wedcen average

