
# EOF from interpolated SON SST-anoms

suppressMessages(library(fields))
suppressMessages(library(cmocean))

#TODO: move data over to the correction file locations
#load data & functions
setwd("~/CO_AUS/pIOD_prep") 
load("Data/sst_dataprods.rda") 
load("Data/sst_interp.rda")

#update to: setwd("~/CO_AUS/AusCOmodeling/Functions") and pIOD_functions.R
source("prep_function.R")


#boundary setup
#Indian Ocean region 
#TODO: update here and other files so that we have a slightly larger range (e.g. 20 -> 21, -20 -> -21, etc. )
wide_maxLon <- 121
wide_minLon <- 29
wide_maxLat <- 21
wide_minLat <- -21

#pIOD (40, -5) X (100, 5), equatorial Indian Ocean region
pIOD_maxLon <- 100
pIOD_minLon <- 40 
pIOD_maxLat <- 5
pIOD_minLat <- -5

#Indian Ocean grid
lon.wide <- seq(wide_minLon, wide_maxLon, by = 1)
lat.wide <- seq(wide_minLat, wide_maxLat, by = 1)

#grid list from 
grid.list <- list(x = lon.wide,
                  y = lat.wide)

#DMI (IOD) boundary
wtio.bound <- list(x1 = 50, x2 = 70, y1 = -10, y2 = 10, lab = "WTIO")
etio.bound <- list(x1 = 90, x2 = 110, y1 = -10, y2 = 0, lab = "ETIO")


#location range
lon.values.IOD <- lon.wide[lon.wide <= pIOD_maxLon & lon.wide >= pIOD_minLon]
lon.range.IOD <- range(which(lon.wide <= pIOD_maxLon & lon.wide >= pIOD_minLon))

lat.values.IOD <- lat.wide[lat.wide <= pIOD_maxLat & lat.wide >= pIOD_minLat]
lat.range.IOD <- range(which(lat.wide <= pIOD_maxLat & lat.wide >= pIOD_minLat))

#get average over anoms data prods.
son.GODAS.anom <- son.anom.list$GODAS
son.ORAS.anom <- son.anom.list$ORAS
son.OISST.anom <- son.anom.list$OISST
son.SODA.anom <- son.anom.list$SODA


#multi-product averages
mat_list <- list(son.GODAS.anom, son.ORAS.anom,
                 son.OISST.anom, son.SODA.anom)


array_new <- abind(mat_list, along = 4)

sst.son.anom <- apply(array_new, c(1,2,3), mean, na.rm = TRUE)
dimnames(sst.son.anom) <- NULL

dim(sst.son.anom)



sst.anom <- aperm(sst.son.anom, c(3, 2, 1))

sst.anom.pIOD <- sst.anom[ ,lat.range.IOD[1]:lat.range.IOD[2], lon.range.IOD[1]:lon.range.IOD[2]]

dim(sst.anom.pIOD)


#pca
pca.pIOD <- sst.eof(sst.anom.pIOD, kmode = 2)

pc.std.IOD <- scale(pca.pIOD$PC, center = TRUE, scale = TRUE)

PC1 <- -pc.std.IOD[,1]
PC2 <- pc.std.IOD[,2]

#pIOD indices
s.index <- (PC1 + PC2)/sqrt(2)
m.index <- (PC1 - PC2)/sqrt(2)

D <- pca.pIOD$D

#eof
sst.eof <- pca.pIOD$EOF

#save pca output
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST")
save(pca.pIOD, file = "pIOD_pca.rda")


#TODO: update later
nx <- dim(sst.son.anom)[1]
ny <- dim(sst.son.anom)[2]
nt <- dim(sst.son.anom)[3]

sst.piod.mat <- matrix(sst.son.anom, nrow =  ny * nx, ncol = nt)

pc1.son <- sst.piod.mat %*% PC1
pc2.son <- sst.piod.mat %*% PC2

#TODO: get D[1] or D[2] from above pca/svd
eof1.son <- matrix(pc1.son/D[1], nrow = nx, ncol = ny)
eof2.son <- matrix(pc2.son/D[2], nrow = nx, ncol = ny) #is D[2] correct here?


#spatial eof for strong and weak
#TODO: double check the values here (specifically the /0.5)
strong.son <- (eof1.son + eof2.son)/sqrt(2)
moderate.son <- (eof1.son - eof2.son)/sqrt(2)


yt <- c(-20, -10, 0, 10, 20)
xt <- c(50, 70, 90, 110)

yt.lab <- c("20S", "10S", "0", "10N", "20N")
xt.lab <- c("50E", "70E", "90E", "110E")


#test output figure
eof.absmax <- max(abs(eof1.son), abs(eof2.son), na.rm = TRUE)

image.plot(list(x = lon.wide, y = lat.wide, z = eof1.son), 
      col = cmocean("curl")(15), zlim = c(-1, 1),
      xlab = "Lon", ylab = "Lat")
world(add=TRUE)

image.plot(list(x = lon.wide, y = lat.wide, z = eof2.son), 
           col = cmocean("curl")(15), zlim = c(-1, 1),
           xlab = "Lon", ylab = "Lat")
world(add=TRUE)


#figures: piod indices 
#TODO: update figures to align with other "world" maps
index.max <- max(abs(strong.son), abs(moderate.son), na.rm = TRUE)

#TODO: set cex values here:

#s-index
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SI_pIOD_S_Index.png", width = 3600, height = 2000, res = 275)
par(mar = c(4.5, 4.5, 2, 4))
image.plot(list(x = lon.wide, y = lat.wide, z = strong.son), 
           col = cmocean("balance")(45), zlim = c(-1, 1),
           xlim = c(35, 120), ylim = c(-20,20),
           xaxt = "n", yaxt = "n", cex.lab = 1.25, 
           legend.line = -4, legend.mar = 5,
           legend.args = list(text = "", cex = 1.2), 
           xlab = "Longitude", ylab = "Latitude")
# Major axes
box()
axis(1, at = xt, labels = xt.lab, cex.axis = 1.2)
axis(2, at = yt, labels = yt.lab, cex.axis = 1.2)
world(add=TRUE, lwd = 1.5)
#IOD rect()
rect(wtio.bound$x1, wtio.bound$y1, wtio.bound$x2, wtio.bound$y2, border = "black", lwd = 1.65)
xmid <- 0.5 * (wtio.bound$x1 + wtio.bound$x2)
text(xmid, wtio.bound$y1 - 1.5, wtio.bound$lab, cex = 1.29, col = "black")
rect(etio.bound$x1, etio.bound$y1, etio.bound$x2, etio.bound$y2, border = "black", lwd = 1.65)
xmid <- 0.5 * (etio.bound$x1 + etio.bound$x2)
text(xmid, etio.bound$y1 - 1.5, etio.bound$lab, cex = 1.29, col = "black")
dev.off()

#m-index (projected over Indian Ocean)
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SI_pIOD_M_Index.png", width = 3600, height = 2000, res = 275)
par(mar = c(4.5, 4.5, 2, 4))
image.plot(list(x = lon.wide, y = lat.wide, z = moderate.son), 
           col = cmocean("balance")(45), zlim = c(-1, 1),
           xlim = c(35, 120), ylim = c(-20,20),
           xaxt = "n", yaxt = "n", cex.lab = 1.25, 
           legend.line = -4, legend.mar = 5,
           legend.args = list(text = "", cex = 1.2), 
           xlab = "Longitude", ylab = "Latitude")
# Major axes
box()
axis(1, at = xt, labels = xt.lab, cex.axis = 1.2)
axis(2, at = yt, labels = yt.lab, cex.axis = 1.2)
world(add=TRUE, lwd = 1.5)
rect(wtio.bound$x1, wtio.bound$y1, wtio.bound$x2, wtio.bound$y2, border = "black", lwd = 1.65)
xmid <- 0.5 * (wtio.bound$x1 + wtio.bound$x2)
text(xmid, wtio.bound$y1 - 1.5, wtio.bound$lab, cex = 1.29, col = "black")
rect(etio.bound$x1, etio.bound$y1, etio.bound$x2, etio.bound$y2, border = "black", lwd = 1.65)
xmid <- 0.5 * (etio.bound$x1 + etio.bound$x2)
text(xmid, etio.bound$y1 - 1.5, etio.bound$lab, cex = 1.29, col = "black")
dev.off()
