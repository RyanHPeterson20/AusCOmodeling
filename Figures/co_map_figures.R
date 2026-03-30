#map for MOPITT CO

#libraries
library(ncdf4)
library(fields)
library(maps)
library(RColorBrewer)
library(cmocean)

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
col_vmr.diff.new <- t(col_vmr.diff.new)

#plot setup


#updated lon zero at 33W
lon0 <- (360 - 33) %% 360
lon360 <- lon.grid %% 360

lon.rotate <- lon360
lon.rotate[lon.rotate < lon0] <- lon.rotate[lon.rotate < lon0] + 360

lon.new <- lon.rotate[order(lon.rotate)]
vmr.new <- col_vmr[order(lon.rotate), ]
#vmr_diff.new <- col_vmr.diff[order(lon.rotate), ]
vmr_diff.new <- col_vmr.diff.new[order(lon.rotate), ]


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

#updated labels
latNS_ascii <- function(y) {
  y <- as.numeric(y)
  ifelse(y < 0, paste0(abs(y), "S"),
         ifelse(y > 0, paste0(y, "N"), "0"))
}

lonEW_ascii <- function(x) {
  x <- as.numeric(x)
  x360 <- x %% 360
  x180 <- ifelse(x360 > 180, x360 - 360, x360)
  ifelse(x180 < 0, paste0(abs(x180), "W"),
         ifelse(x180 > 0, paste0(x180, "E"), "0"))
}

lon_to_plotx <- function(lon, seam_degW = 33) {
  lon0 <- (360 - seam_degW) %% 360  # 35W -> 325
  x <- lon %% 360
  if (x < lon0) x <- x + 360
  x
}

#reducing lat
lat_min <- -60
lat_max <-  20

lat.index <- which(lat.grid >= lat_min & lat.grid <= lat_max)

lat.sub <- lat.grid[lat.index]
vmr.new.sub <- vmr.new[, lat.index]
vmr_diff.new.sub <- vmr_diff.new[ ,lat.index]

#create plots

yt <- c(-60, -40, -20, 0, 20)
xt <- seq(330, 330 + 360, by = 60)

zmin <- 30
zmax <- 130
ncol <- 256


#setup climate modes
climate_modes <- list(
  nino34 <- list(x1 = -170, x2 = -120, y1 = -5, y2 = 5, lab = "Ni\u00f1o 3.4"),
  tsa <- list(x1 = -30, x2 = 10, y1 = -20, y2 = 0, lab = "TSA"),
  wtio <- list(x1 = 50, x2 = 70, y1 = -10, y2 = 10, lab = "WTIO"),
  etio <- list(x1 = 90, x2 = 110, y1 = -10, y2 = 0, lab = "ETIO")
)
sam <- list(x1 = -32.95, x2 = -33.05, y1 = -60, y2 = -40, lab = "SAM")

# Clip/saturate the data so outside range uses endpoint colors
z_clip <- vmr.new.sub
z_clip[z_clip < zmin] <- zmin
z_clip[z_clip > zmax] <- zmax

cols   <- viridis(ncol)
breaks <- seq(zmin, zmax, length.out = ncol + 1)  # evenly spaced


#mean co vmr

setwd("~/CO_AUS/AusCOmodeling/Figures")
#TODO: test pointsize arg in png()
png(filename = "Fig1a_MeanCO_climatemodes.png",  width = 4145, height = 1220, res = 300)
par(mar = c(3.5, 3.5, 2.0, 1.15))
image.plot(
  lon.new, lat.sub, z_clip,
  #col = cmocean("matter")(55),
  col = cols,
  breaks = breaks,              # forces uniform colorbar bins
  zlim = c(zmin, zmax),         # locks scale
  xaxt = "n", yaxt = "n",
  xlab = "", ylab = "",
  legend.lab = "CO [ppb]",
  legend.line = 2.5,
  legend.mar = 5,
  axis.args = list(tcl = -0.2, mgp = c(2.25, 0.95, 0))
)

# Major axes
axis(1, at = xt, labels = lonEW_ascii(xt), cex.axis = 1.182)
axis(2, at = yt, labels = latNS_ascii(yt), cex.axis = 1.182)
box()

mtext("Longitude", side=1, line=2.25, cex = 1.25) #x-axis
mtext("Latitude",  side=2, line=2.25, cex = 1.25) #y-axis

title("(a) Mean Peak-Season CO 2001 to 2019", adj = 0, cex.main = 1.25)

lines(x.newest, y.new, col = "gray76", lwd = 0.8)

for (i in climate_modes) {
  rect(lon_to_plotx(i$x1), i$y1, lon_to_plotx(i$x2), i$y2, border = "white", lwd = 1.5)
  
  # place label at rectangle midpoint in plot-x coordinates
  xmid <- 0.5 * (lon_to_plotx(i$x1) + lon_to_plotx(i$x2))
  text(xmid, i$y1 + 3.75, i$lab, cex = 1.06, col = "white")
}

rect(lon_to_plotx(sam$x1), sam$y1, lon_to_plotx(sam$x2), sam$y2, border = "white", lwd = 1.1, lty = 2)
xmid <- 0.5 * (lon_to_plotx(sam$x1) + lon_to_plotx(sam$x2))
text(xmid, sam$y1 + 10, sam$lab, cex = 1.12, col = "white")

dev.off()



#response regions
ne.aus <- list(x1 = 134, x2 = 155, y1 = -25, y2 = -10, lab = "NE Aus")
se.aus <- list(x1 = 134, x2 = 155, y1 = -48, y2 = -25, lab = "SE Aus")
  
  
#color adjustments for relative difference
zmax <- 1.2                  
zlim <- c(-zmax, zmax)

z_clip.rel.diff <- pmin(pmax(vmr_diff.new.sub, zlim[1]), zlim[2])

ncol <- 256
cols <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(ncol)

breaks <- seq(zlim[1], zlim[2], length.out = ncol + 1)


#difference for 2019/2020
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "Fig1b_relDiffCO_Aus.png", width = 4145, height = 1220, res = 300)
par(mar = c(3.5, 3.5, 2.0, 1.15))
image.plot(
  lon.new, lat.sub, z_clip.rel.diff,
  #col = cmocean("balance")(101),
  col = cols,
  breaks = breaks,
  zlim = zlim,
  xaxt = "n", yaxt = "n",
  xlab = "", ylab = "",
  legend.lab = "CO (relative difference)",
  legend.line = 2.5,
  legend.mar = 5,
  axis.args = list(tcl = -0.2, mgp = c(2.25, 0.95, 0))
)

# Major axes
axis(1, at = xt, labels = lonEW_ascii(xt), cex.axis = 1.182)
axis(2, at = yt, labels = latNS_ascii(yt), cex.axis = 1.182)
box()

mtext("Longitude", side=1, line=2.25, cex = 1.25) #x-axis
mtext("Latitude",  side=2, line=2.25, cex = 1.25) #y-axis

title("(b) Peak-Season 2019/2020 Relative Difference from Mean CO", adj = 0, cex.main = 1.25)

lines(x.newest, y.new, col = "gray7", lwd = 0.8)

#rect(lon_to_plotx(ne.aus$x1), ne.aus$y1, lon_to_plotx(ne.aus$x2), ne.aus$y2, border = "gray12", lwd = 1.75, lty = 1)
rect(lon_to_plotx(se.aus$x1), se.aus$y1, lon_to_plotx(se.aus$x2), se.aus$y2, border = "gray10", lwd = 1.85, lty = 1)

#xmid <- 0.5 * (lon_to_plotx(ne.aus$x1) + lon_to_plotx(ne.aus$x2))
#text(xmid, ne.aus$y1 + 11, ne.aus$lab, cex = 1.1, col = "gray12")

xmid <- 0.5 * (lon_to_plotx(se.aus$x1) + lon_to_plotx(se.aus$x2))
text(xmid, se.aus$y1 + 3, se.aus$lab, cex = 1.05, col = "gray10")

dev.off()


