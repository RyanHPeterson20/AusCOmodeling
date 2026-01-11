#map for MOPITT CO

#libraries
library(ncdf4)
library(fields)

setwd("~/CO_AUS/AusCOmodeling/Data") 

#co_data.nc <- nc_open("MOPITT_peakseason_V9J_2001_2019.he5") #newest data, address later.
co.nc <- nc_open("MOPITT_peakseason_V9J_2002_2019.nc")
co_data.nc <- nc_open("2019-2020_peakseason_V9J.nc")
co_diff.nc <- nc_open("2019-2020_peakseason_V9J_diff.nc")

# column:
## RetrievedCOTotalColumnDay
column <- ncvar_get(co.nc, "HDFEOS/GRIDS/MOP03/Data Fields/RetrievedCOTotalColumnDay")
column.2019 <- ncvar_get(co_data.nc, "HDFEOS/GRIDS/MOP03/Data Fields/RetrievedCOTotalColumnDay") 
column.diff <- ncvar_get(co_diff.nc, "HDFEOS/GRIDS/MOP03/Data Fields/RetrievedCOTotalColumnDay") 

# dryair:
## DryAirColumnDay
dryair <- ncvar_get(co.nc, "HDFEOS/GRIDS/MOP03/Data Fields/DryAirColumnDay")
dryair.2019 <- ncvar_get(co_data.nc, "HDFEOS/GRIDS/MOP03/Data Fields/DryAirColumnDay")
dryair.diff <- ncvar_get(co_diff.nc, "HDFEOS/GRIDS/MOP03/Data Fields/DryAirColumnDay")

# lat, lon
lon.grid <- co_data.nc[["dim"]][["HDFEOS/GRIDS/MOP03/XDim"]][["vals"]]
lat.grid <- co_data.nc[["dim"]][["HDFEOS/GRIDS/MOP03/YDim"]][["vals"]]

