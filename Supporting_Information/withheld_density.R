
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
suppressMessages( library(rgl))

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

#DMI representation 
wtio.all <- as.numeric(X.wtio.1)
wtio.ordered <- wtio.all[order(wtio.all)]

etio.all <- as.numeric(X.etio.1)
etio.ordered <- etio.all[order(etio.all)]

dmi.all <- outer(wtio.ordered, etio.ordered, "-")

image.plot(x = wtio.ordered, y = etio.ordered, z = dmi.all,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("curl")(36))

#alternative DMI setup
wtio.range <- range(wtio.all)
etio.range <- range(etio.all)

wtio.new <- seq(wtio.range[1], wtio.range[2], length.out = 500)
etio.new <- seq(etio.range[1], etio.range[2], length.out = 500)

dmi.new <- outer(wtio.new, etio.new, "-")

image.plot(x = wtio.new, y = etio.new, z = dmi.new,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("delta")(68))



k <- kde2d(x = wtio.all, y = etio.all,
           n = 500, lims = c(range(wtio.all), range(etio.all)))

wtio.new <- k$x
etio.new <- k$y
dmi.new  <- outer(wtio.new, etio.new, "-") 
z.surface <- k$z
#z.surface <- z.surface/max(z.surface)

target.dmi <- 1
tol <- max(diff(wtio.new)[1], diff(etio.new)[1])  # ~ one step

index.dmi <- which(abs(dmi.new - target.dmi) <= tol, arr.ind = TRUE)

length(index.dmi)

wtio.slice <- wtio.new[index.dmi[, 1]]
etio.slice <- etio.new[index.dmi[, 2]]
dmi.slice  <- dmi.new[index.dmi]

#z.slice <- z.surface[index.dmi] 
## TODO: review the next 2 lines
z.slice <- z.surface
z.slice[!index.dmi] <- NA_real_


# Optional: order along the diagonal so 1D plots look clean
slice.order <- order(wtio.slice)
wtio.slice <- wtio.slice[slice.order]
etio.slice <- etio.slice[slice.order]
z.slice    <- z.slice[slice.order]

plot(wtio.slice, z.slice,
     pch = 16, cex = 0.5,
     xlab = "WTIO (along WTIO - ETIO ≈ 1.5)",
     ylab = "KDE density",
     main = "Density values along the diagonal band")
sp <- smooth.spline(wtio.slice, z.slice, spar = 0.6)
lines(sp, lwd = 2, col = "forestgreen")


plot(etio.slice, z.slice,
     pch = 16, cex = 0.5,
     xlab = "ETIO (along WTIO - ETIO ≈ 1.5)",
     ylab = "KDE density",
     main = "Density values along the diagonal band")
sp <- smooth.spline(etio.slice, z.slice, spar = 0.6)
lines(sp, lwd = 2, col = "forestgreen")


#TODO: create an overlap between the above plots


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
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36))

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

# temp hdf5 save function

