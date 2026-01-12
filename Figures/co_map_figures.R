#map for MOPITT CO

#libraries
library(ncdf4)
library(fields)
library(maps)

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

#vmr (might need transpose)
col_vmr <- (column/dryair)/1e-09
col_vmr.2019 <- (column.2019/dryair.2019)/1e-09
col_vmr.diff <- (column.diff/(dryair.diff+dryair))/1e-09

col_vmr.diff.new <- (col_vmr.2019 - col_vmr)/col_vmr

max(col_vmr, na.rm = TRUE)
max(col_vmr.diff, na.rm = TRUE)
max(col_vmr.diff.new, na.rm = TRUE)

col_vmr <- t(col_vmr)
col_vmr.diff <- t(col_vmr.diff)

#plot setup


#updated lon zero at 35W
lon0 <- (360 - 33) %% 360
lon360 <- lon.grid %% 360

lon.rotate <- lon360
lon.rotate[lon.rotate < lon0] <- lon.rotate[lon.rotate < lon0] + 360

lon.new <- lon.rotate[order(lon.rotate)]
vmr.new <- col_vmr[order(lon.rotate), ]
vmr_diff.new <- col_vmr.diff[order(lon.rotate), ]

#adjustments to world map
w <- map("world", plot = FALSE, fill = FALSE)
x.new <- w$x 
y.new <- w$y

#TODO: clean this up later
is_na <- is.na(x.new) | is.na(y.new)

x2 <- x.new
x2[!is_na] <- x2[!is_na] %% 360
x2[!is_na & x2 < lon0] <- x2[!is_na & x2 < lon0] + 360

x.newest <- x2


#create plots
#mean co vmr
image.plot(
  lon.new, lat.grid, vmr.new,
  col = viridis(128), ylim = c(-60, 20),
  xaxt = "n",
  xlab = "", ylab = "",
  legend.lab = "CO (column average VMR, ppb)"
)
lines(x.newest, y.new, col = "black", lwd = 0.8)


image.plot(
  lon.new, lat.grid, vmr_diff.new,
  col = tim.colors(256), ylim = c(-60, 20),
  xaxt = "n",
  xlab = "", ylab = "",
  legend.lab = "CO (column average VMR, ppb)"
)
lines(x.newest, y.new, col = "black", lwd = 0.8)
