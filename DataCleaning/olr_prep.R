
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
olr_nc_2003 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20030101_20031231.nc') #2003
olr_nc_2004 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20040101_20041231.nc') #2004
olr_nc_2005 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20050101_20051231.nc') #2005
olr_nc_2006 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20060101_20061231.nc') #2006 
olr_nc_2007 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20070101_20071231.nc') #2007
olr_nc_2008 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20080101_20081231.nc') #2008
olr_nc_2009 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20090101_20091231.nc') #2009
olr_nc_2010 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20100101_20101231.nc') #2010
olr_nc_2011 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20110101_20111231.nc') #2011
olr_nc_2012 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20120101_20121231.nc') #2012
olr_nc_2013 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20130101_20131231.nc') #2013
olr_nc_2014 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20140101_20141231.nc') #2014
olr_nc_2015 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20150101_20151231.nc') #2015
olr_nc_2016 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20160101_20161231.nc') #2016
olr_nc_2017 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20170101_20171231.nc') #2017
olr_nc_2018 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20180101_20181231.nc') #2018 
olr_nc_2019 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20190101_20191231.nc') #2019
olr_nc_2020 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20200101_20201231.nc') #2020
olr_nc_2021 <- nc_open('AUS_CO-main/OLR/olr-daily_v01r02_20210101_20211231.nc') #2021

setwd("~/CO_AUS/AusCOmodeling") 
#load internal functions
source("Functions/dataclean_functions.R") # functions 

#lat-lon boundaries
#Daniels et al, 2022 uses MSEA boundary for OLR-MJO
##lat: (10N, 10S); lon: (90, 160)
lat.range <- c(-10, 10) 
lon.range <- c(90, 160)

##TODO: create and test functions for data cleaning
