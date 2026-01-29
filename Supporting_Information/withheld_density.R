
#WIP for working out univariate and bivariate predictor densities.

#potential libraries (review later)
#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(grid)) #table/grid setup and lines between plots
suppressMessages( library(gridExtra))
suppressMessages( library(lubridate))
suppressMessages( library(MASS))
suppressMessages(library(colorspace))
suppressMessages(library(RColorBrewer))
suppressMessages( library(cmocean)) #ocean colors
library(hdf5r)

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
source("Functions/modeling_functions.R")

#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)


#Get visualizations of data

SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

#get lag 1-52 from week 51 and lags 1-3 from week 2 (for the peak group)
SEpreds.peak <- SE.pred$mid
SEpreds.peak51 <- SEAus.lag$`Week  51`
SEpreds.peak2 <- SEAus.lag$`Week  2`

#up to lag 52
SEpreds.peak51.nino <- SEpreds.peak51[ ,3:54]
SEpreds.peak51.wtio <- SEpreds.peak51[ ,107:158]
SEpreds.peak51.etio <- SEpreds.peak51[ ,159:210]
SEpreds.peak51.tsa <- SEpreds.peak51[ ,211:262] 
SEpreds.peak51.aao <- SEpreds.peak51[ ,263:314]
SEpreds.peak51.olr <- SEpreds.peak51[ ,315:366]
#only up to lag 3
SEpreds.peak2.nino <- SEpreds.peak2[ ,3:5]
SEpreds.peak2.wtio <- SEpreds.peak2[ ,107:109]
SEpreds.peak2.etio <- SEpreds.peak2[ ,159:161]
SEpreds.peak2.tsa <- SEpreds.peak2[ ,211:213] 
SEpreds.peak2.aao <- SEpreds.peak2[ ,263:265]
SEpreds.peak2.olr <- SEpreds.peak2[ ,315:317]

SEpreds.peak.nino <- cbind(SEpreds.peak2.nino, SEpreds.peak51.nino)
SEpreds.peak.wtio <- cbind(SEpreds.peak2.wtio, SEpreds.peak51.wtio)
SEpreds.peak.etio <- cbind(SEpreds.peak2.etio, SEpreds.peak51.etio)
SEpreds.peak.tsa <- cbind(SEpreds.peak2.tsa, SEpreds.peak51.tsa)
SEpreds.peak.aao <- cbind(SEpreds.peak2.aao, SEpreds.peak51.aao)
SEpreds.peak.olr <- cbind(SEpreds.peak2.olr, SEpreds.peak51.olr)


#start with frequency histograms
par(mfrow = c(3,2))
hist(as.matrix(SEpreds.peak.nino), freq = FALSE, main = "Nino - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.wtio), freq = FALSE, main = "WTIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.etio), freq = FALSE, main = "ETIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.tsa), freq = FALSE, main = "TSA - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.aao), freq = FALSE, main = "SAM (AAO) - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.olr), freq = FALSE, main = "OLR - Peak Group", xlab = "Anomaly")


#overlayed histograms

#nino
X.nino.1 <- as.matrix(SEpreds.peak.nino)
X.nino.2 <- as.matrix(SEpreds.peak.nino[-19, ]) #without 2019/2020
X.nino.3 <- as.matrix(SEpreds.peak.nino[-c(1,19), ]) #without 2001/2002; 2019/2020
brks <- pretty(range(c(X.nino.1, X.nino.2)), n = 14)
hist(X.nino.1, breaks = brks,
     freq = FALSE, col = "grey80",
     border = "grey40",
     main = "Nino - Peak Group", xlab = "Anomaly")
lines(density(X.nino.1), col = "gray5", lwd = 2)
hist(X.nino.2,
     breaks = brks,
     freq = FALSE,
     col = rgb(1, 0, 0, 0.24),
     border = rgb(1, 0, 0, 0.58),
     add = TRUE)
lines(density(X.nino.2), col = "firebrick", lwd = 2.5, lty = 2)
lines(density(X.nino.3), col = "darkorange2", lwd = 2.65, lty = 2)


sum(density(X.nino.1)$y)
sum(density(X.nino.2)$y)
sum(density(X.nino.3)$y)


#wtio
X.wtio.1 <- as.matrix(SEpreds.peak.wtio)
X.wtio.2 <- as.matrix(SEpreds.peak.wtio[-19, ]) #without 2019/2020
X.wtio.3 <- as.matrix(SEpreds.peak.wtio[-c(1,19), ]) #without 2001/2002; 2019/2020
brks <- pretty(range(c(X.wtio.1, X.wtio.2)), n = 14)
hist(X.wtio.1, breaks = brks,
     freq = FALSE, col = "grey80",
     border = "grey40",
     main = "WTIO - Peak Group", xlab = "Anomaly")
lines(density(X.wtio.1), col = "gray5", lwd = 2)
hist(X.wtio.2,
     breaks = brks,
     freq = FALSE,
     col = rgb(0, 1, 0, 0.24),
     border = rgb(0, 1, 0, 0.58),
     add = TRUE)
lines(density(X.wtio.2), col = "forestgreen", lwd = 2.5, lty = 2)
lines(density(X.wtio.2), col = "chartreuse2", lwd = 2.75, lty = 2)


#etio
X.etio.1 <- as.matrix(SEpreds.peak.etio)
X.etio.2 <- as.matrix(SEpreds.peak.etio[-19, ]) #without 2019/2020
X.etio.3 <- as.matrix(SEpreds.peak.etio[-c(1,19), ]) #without 2001/2002; 2019/2020
brks <- pretty(range(c(X.etio.1, X.etio.2)), n = 14)
hist(X.etio.1, breaks = brks,
     freq = FALSE, col = "grey80",
     border = "grey40",
     main = "ETIO - Peak Group", xlab = "Anomaly")
lines(density(X.etio.1), col = "gray5", lwd = 2)
hist(X.etio.2,
     breaks = brks,
     freq = FALSE,
     col = rgb(0, 0, 1, 0.24),
     border = rgb(0, 0, 1, 0.58),
     add = TRUE)
lines(density(X.etio.2), col = "blue3", lwd = 2.5, lty = 2)
lines(density(X.etio.3), col = "slateblue3", lwd = 2.5, lty = 2)

dens.test <- density(X.etio.2)


#tsa
X.tsa.1 <- as.matrix(SEpreds.peak.tsa)
X.tsa.2 <- as.matrix(SEpreds.peak.tsa[-19, ]) #without 2019/2020
X.tsa.3 <- as.matrix(SEpreds.peak.tsa[-c(1,19), ]) #without 2001/2002; 2019/2020
brks <- pretty(range(c(X.tsa.1, X.tsa.2)), n = 14)
hist(X.tsa.1, breaks = brks,
     freq = FALSE, col = "grey80",
     border = "grey40",
     main = "TSA - Peak Group", xlab = "Anomaly")
lines(density(X.tsa.1), col = "gray5", lwd = 2)
hist(X.tsa.2,
     breaks = brks,
     freq = FALSE,
     col = rgb(1, 0, 1, 0.24),
     border = rgb(1, 0, 1, 0.58),
     add = TRUE)
lines(density(X.etio.2), col = "darkmagenta", lwd = 2.5, lty = 2)
lines(density(X.etio.3), col = "magenta", lwd = 2.5, lty = 2)



## ------ Bivariate Density ----- ##

#data setup (peak lags 1-52)
#wtio
X.wtio.1 <- as.matrix(SEpreds.peak.wtio)
X.wtio.2 <- as.matrix(SEpreds.peak.wtio[-19, ]) #without 2019/2020
#etio
X.etio.1 <- as.matrix(SEpreds.peak.etio)
X.etio.2 <- as.matrix(SEpreds.peak.etio[-19, ]) #without 2019/2020

#full year
#weekly data
nino.std <- scale(pred.matrix[-1, 1:52], center = TRUE, scale = TRUE)
wtio.std <- scale(pred.matrix[-1, 105:156], center = TRUE, scale = TRUE)
etio.std <- scale(pred.matrix[-1, 157:208], center = TRUE, scale = TRUE)
tsa.std <- scale(pred.matrix[-1, 209:260], center = TRUE, scale = TRUE)
aao.std <- scale(pred.matrix[-1, 261:312], center = TRUE, scale = TRUE)
olr.std <- scale(pred.matrix[-1, 313:364], center = TRUE, scale = TRUE)

#son season data (seems to be the most interesting scenario)
sept.ind <- which(month(pred.df$date) == 9)
nov.ind <- which(month(pred.df$date) == 11)
pred.df[sept.ind, ]$week #min ~ 36
pred.df[nov.ind, ]$week #max ~ 48

son.wtio <- as.matrix(wtio.std[ ,36:48])
son.etio <- as.matrix(etio.std[ ,36:48])

#TODO: repeat for other `seasons`

#DMI representation 
wtio.all <- as.numeric(X.wtio.1)
wtio.ordered <- wtio.all[order(wtio.all)]

etio.all <- as.numeric(X.etio.1)
etio.ordered <- etio.all[order(etio.all)]

wtio.range <- range(wtio.all)
etio.range <- range(etio.all)
iod.range <- range(wtio.range, etio.range)


wtio.new <- seq(iod.range[1], iod.range[2], length.out = 500)
etio.new <- seq(iod.range[1], iod.range[2], length.out = 500)

dmi.new <- outer(wtio.new, etio.new, "-")

#finalize and output plot
image.plot(x = wtio.new, y = etio.new, z = dmi.new,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("balance")(69))





#bivariate KDE for WTIO and ETIO (IOD)
IOD.kde <- kde2d(as.numeric(X.wtio.1), as.numeric(X.etio.1), n = 150)


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "IOD_kde.png", width = 2000, height = 1500, res =275)
image.plot(x = IOD.kde$x, y = IOD.kde$y, z = IOD.kde$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
           main = "All Data")
dev.off()

#try 3-d adjust later, move to python
persp(x = IOD.kde$x, y = IOD.kde$y, z = IOD.kde$z,
      theta = 35, phi = 25,
      expand = 0.6,
      xlab = "WTIO", ylab = "ETIO", zlab = "Density",
      ticktype = "detailed",
      main = "Bivariate KDE surface"
)


#bivariate KDE for WTIO and ETIO, without 2019/2020
IOD.kde.2 <- kde2d(as.numeric(X.wtio.2), as.numeric(X.etio.2), n = 150)


image.plot(x = IOD.kde.2$x, y = IOD.kde.2$y, z = IOD.kde.2$z,
           xlab = "WTIO", ylab = "ETIO", col = rev(cmocean("matter")(36)))


#differencing
iod_diff <- IOD.kde.2
iod_diff$z <- IOD.kde$z - IOD.kde.2$z

m <- max(abs(iod_diff$z), na.rm = TRUE)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "IOD_kde_2019diff.png", width = 2000, height = 1500, res =275)
image.plot(x = iod_diff$x, y = iod_diff$y, z = iod_diff$z, zlim = c(-m, m), 
           xlab = "WTIO", ylab = "ETIO", col =  rev(cmocean("tarn")(31)),
           main = "All Data - Withheld 2019/2020")
dev.off()

#try 3-d adjust later
persp(x = iod_diff$x, y = iod_diff$y, z = iod_diff$z,
      theta = 35, phi = 25,
      expand = 0.6,
      xlab = "WTIO", ylab = "ETIO", zlab = "Density",
      ticktype = "detailed"
)


#bivariate KDE for WTIO and ETIO, without 2019/2020 and 2001/2002
IOD.kde.3 <- kde2d(as.numeric(X.wtio.3), as.numeric(X.etio.3), n = 150)

image.plot(x = IOD.kde.3$x, y = IOD.kde.3$y, z = IOD.kde.3$z,
           xlab = "WTIO", ylab = "ETIO", col = rev(cmocean("haline")(36)))

#differencing
iod_diff.2 <- IOD.kde.2
iod_diff.2$z <- IOD.kde.2$z - IOD.kde.3$z

m <- max(abs(iod_diff$z), abs(iod_diff.2$z), na.rm = TRUE)

image.plot(x = iod_diff.2$x, y = iod_diff.2$y, z = iod_diff.2$z, zlim = c(-m, m), 
           xlab = "WTIO", ylab = "ETIO", col = rev(cmocean("tarn")(31)))


#TODO: formalize KDE for all withheld variations




#SON - bivariate KDE for WTIO and ETIO 
#setup for limits
etio.lim <- range(etio.std, na.rm = TRUE) #y axis
wtio.lim <- range(wtio.std, na.rm = TRUE) #x axis

son.kde <- kde2d(as.numeric(son.wtio), as.numeric(son.etio), 
                lims = c(wtio.lim, etio.lim), n = 200)

#setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
#png(filename = "IOD_kde.png", width = 2000, height = 1500, res =275)
image.plot(x = son.kde$x, y = son.kde$y, z = son.kde$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
           main = "SON - All Data")


#without 2019/2020
son.kde.1 <- kde2d(as.numeric(son.wtio[-19, ]), as.numeric(son.etio[-19,]), 
                   lims = c(wtio.lim, etio.lim), n = 200)

#setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
#png(filename = "IOD_kde.png", width = 2000, height = 1500, res =275)
image.plot(x = son.kde.1$x, y = son.kde.1$y, z = son.kde.1$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
           main = "SON - 2019/2020 Withheld")

#differencing
iod_diff <- son.kde.1
iod_diff$z <- son.kde.1$z - son.kde$z

m <- max(abs(iod_diff$z), na.rm = TRUE)

#setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
#png(filename = "IOD_kde_2019diff.png", width = 2000, height = 1500, res =275)
image.plot(x = iod_diff$x, y = iod_diff$y, z = iod_diff$z, zlim = c(-m, m), 
           xlab = "WTIO", ylab = "ETIO", col =  rev(cmocean("tarn")(31)),
           main = "Withheld 2019/2020 - All Data")
#dev.off()


#without 2001/2002 and 2019/2020
son.kde.2001 <- kde2d(as.numeric(son.wtio[-c(1,19), ]), as.numeric(son.etio[-c(1,19),]), 
                   lims = c(wtio.lim, etio.lim), n = 200)

#setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
#png(filename = "IOD_kde.png", width = 2000, height = 1500, res =275)
image.plot(x = son.kde.2001$x, y = son.kde.2001$y, z = son.kde.2001$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
           main = "SON : 2001/2002  (& 2019/2020) Withheld")

iod_diff.2 <- son.kde.2001
iod_diff.2$z <- son.kde.2001$z - son.kde.1$z

m <- max(abs(iod_diff$z), na.rm = TRUE)

#setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
#png(filename = "IOD_kde_2019diff.png", width = 2000, height = 1500, res =275)
image.plot(x = iod_diff.2$x, y = iod_diff.2$y, z = iod_diff.2$z, zlim = c(-m, m), 
           xlab = "WTIO", ylab = "ETIO", col =  rev(cmocean("tarn")(31)),
           main = "2001/2002 - Only 2019/2020 Withheld")






# temp hdf5 save function
write_kde_h5 <- function(k, file = "kde_iod.he5",
                         group = "/KDE",
                         overwrite = TRUE) {
  
  if (overwrite && file.exists(file)) file.remove(file)
  
  h5 <- H5File$new(file, mode = "w")
  on.exit(h5$close_all(), add = TRUE)
  
  g <- h5$create_group(group)
  
  # Datasets
  g[["x"]] <- k$x
  g[["y"]] <- k$y
  g[["z"]] <- k$z
  
  # Write attributes using create_attr (safe in hdf5r)
  g$create_attr("description", "Bivariate KDE grid exported from R (HDF5 container).")
  g$create_attr("nx", length(k$x))
  g$create_attr("ny", length(k$y))
  g$create_attr("z_dim", as.integer(dim(k$z)))  # stores c(nx, ny)
  
  invisible(TRUE)
}


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information")
write_kde_h5(IOD.kde.2, "kde_IOD_wo2019.he5")
write_kde_h5(IOD.kde, "kde_IOD.he5")


