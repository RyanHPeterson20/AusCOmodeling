## work on... (see notes)

#1. Get model info (terms and coefficients) for the "interesting" seasons 
#2....


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
source("Functions/coef_int_functions.R")
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

#get single season models for peak 2019/2020 with additional withheld:
## 2001/2002, 2010/2011, and 2011/2012 (along with 2005/2006 and 2016/2017)

#peak all-data models
SE2.lm <- SEmodels[[2]] 



#model for predicting 2019/2020 (generalize as needed)
SE.lm.2019 <- SErefit.wo.years$SE.vary.lm$`2019-2020`
#get all peak models
SE2.lm.2019 <- lapply(SE.lm.2019, function(z) z[[2]])

#get only 2019/2020 withheld
SE.lm.wo2019 <- SE2.lm.2019[[19]]


## ---- Coef/Interaction Figures ---- ##
## setup
#SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
#SE3.coef <- coef(SE3.lm)


#peak models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    #const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(SE2.lm.2019[[i]])  
  )
  
  png(filename = paste0("SEcoefs_2019peak_withheld_", season.years[i], ".png"), width = 3400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak 2019/2020 Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="darkorange2", vary="royalblue3"))
  dev.off()
}


#get tables
card1 <- lm_card_grob(SE2.lm, border = "forestgreen", fill = alpha("springgreen4", 0.2))
card2 <- lm_card_grob(SE.lm.wo2019, border = "darkorange2", fill = alpha("orange3", 0.2))

grid.arrange(card1, card2, ncol = 2)


#peak plot
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  png(filename = paste0("new_model_tables_", season.years[i], ".png"), width = 3500, height = 1500, res = 275)
  
  card2.vary.wo2019 <- lm_card_grob(SE2.lm.2019[[i]], border = "royalblue3", fill = alpha("steelblue4", 0.2))

  
  grid.arrange(card1, card2, card2.vary.wo2019, ncol = 3)
  
  dev.off()
}

#TODO: isolate the weeks that each model looks at when "training" so that we can look at everything.
#note, get each year (52 weeks preceding a group) at a time then combine them later.



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


#use the above dmi plot to grab slice of the ETIO/WTIO density
#find a diagonal slice of equivalent DMI

k <- kde2d(x = wtio.all, y = etio.all,
  n = 500, lims = c(range(wtio.all), range(etio.all)))

wtio.new <- k$x
etio.new <- k$y
dmi.new  <- outer(wtio.new, etio.new, "-") 
z.surface <- k$z
z.surface <- z.surface/max(z.surface)

target.dmi <- 1
tol <- max(diff(wtio.new)[1], diff(etio.new)[1])  # ~ one step

index.dmi <- which(abs(dmi.new - target.dmi) <= tol, arr.ind = TRUE)

length(index.dmi)

wtio.slice <- wtio.new[index.dmi[, 1]]
etio.slice <- etio.new[index.dmi[, 2]]
dmi.slice  <- dmi.new[index.dmi]

#z.slice <- z.surface[index.dmi] 
z.slice <- z.surface
z.slice[!index.dmi] <- NA_real_

open3d()
bg3d("white")

# rgl wants x and y as vectors and z as a matrix
surface3d(wtio.new, etio.new, z.slice, back = "lines", color = rev(cmocean("matter")(36)))
axes3d()
title3d(xlab = "WTIO", ylab = "ETIO", zlab = "Density")


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


#now for the arc-lengths slice
u <- (wtio.slice + etio.slice) / sqrt(2)
o <- order(u)

x <- wtio.slice[o]
y <- etio.slice[o]
z <- z.slice[o]


image.plot(k$x, k$y, k$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("tempo")(36))
lines(x, y,  lwd = 2)






#bivariate KDE for WTIO and ETIO (IOD)
IOD.kde <- kde2d(as.numeric(X.wtio.1), as.numeric(X.etio.1), n = 150)


image.plot(x = IOD.kde$x, y = IOD.kde$y, z = IOD.kde$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
           main = "All Data")


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "IOD_kde.png", width = 2000, height = 1500, res =275)
image.plot(x = IOD.kde$x, y = IOD.kde$y, z = IOD.kde$z,
  xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36), 
  main = "All Data")
dev.off()







#try 3-d adjust later
persp(x = IOD.kde$x, y = IOD.kde$y, z = IOD.kde$z,
  theta = 35, phi = 25,
  expand = 0.6,
  xlab = "WTIO", ylab = "ETIO", zlab = "Density",
  ticktype = "detailed",
  main = "Bivariate KDE surface"
)


#bivariate KDE for WTIO and ETIO, without 2019/2020
IOD.kde.2 <- kde2d(as.numeric(X.wtio.2), as.numeric(X.etio.2), n = 150)


cols.prgn <- colorRampPalette(brewer.pal(11, "Oranges"))(32)


image.plot(x = IOD.kde.2$x, y = IOD.kde.2$y, z = IOD.kde.2$z,
           xlab = "WTIO", ylab = "ETIO", col = cmocean("dense")(36))

#differencing
iod_diff <- IOD.kde.2
iod_diff$z <- IOD.kde$z - IOD.kde.2$z


cols.ryb <- rev(colorRampPalette(brewer.pal(11, "RdBu"))(21))
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

           
           
#check for only 2019/2020
X.wtio.i <- as.matrix(SEpreds.peak.wtio[c(19), ]) #only 2019/2020 and season[i]
X.etio.i <- as.matrix(SEpreds.peak.etio[c(19), ]) #only 2019/2020 and season[i]

IOD.kde.i <- kde2d(as.numeric(X.wtio.i), as.numeric(X.etio.i), n = 150)

image.plot(x = IOD.kde.i$x, y = IOD.kde.i$y, z = IOD.kde.i$z,
           xlab = "WTIO", ylab = "ETIO", col = tim.colors(128))


#repeat for other years, get multiple outputs
i <- 6
X.wtio.i <- as.matrix(SEpreds.peak.wtio[-c(i,19), ]) #without 2019/2020 and season[i]
X.etio.i <- as.matrix(SEpreds.peak.etio[-c(i,19), ]) #without 2019/2020 and season[i]

IOD.kde.i <- kde2d(as.numeric(X.wtio.i), as.numeric(X.etio.i), n = 150)

image.plot(x = IOD.kde.i$x, y = IOD.kde.i$y, z = IOD.kde.i$z,
           xlab = "WTIO", ylab = "ETIO", col = tim.colors(128))




#differencing
iod_diff.i <- IOD.kde.2
iod_diff.i$z <- IOD.kde.2$z - IOD.kde.i$z

cols.rb <- colorRampPalette(c("blue3", "white", "red3"))(89)
m <- max(abs(iod_diff$z), abs(iod_diff.i$z), na.rm = TRUE)

image.plot(x = iod_diff.i$x, y = iod_diff.i$y, z = iod_diff.i$z, zlim = c(-m, m), 
           xlab = "WTIO", ylab = "ETIO", col = cols.rb)


#time series plots for interesting years 2001, 2010, & 2011 (and 2005, 2015)

#predictor colors
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"


#for 2001/2002 peak season
lag1.date <- pred.df[pred.df$week == 1 & pred.df$year == 2002, ]$date
lag.date <- rev(pred.df[pred.df$year == 2001, ]$date)
lag52.date <- rev(pred.df[pred.df$week %in% 51:52 & pred.df$year == 2000, ]$date)

dates.2001 <- c(lag1.date, lag.date, lag52.date)

length(dates.2001)
length(SEpreds.peak.nino[1,])
peak.nino2001.df <- data.frame(nino = rev(as.numeric(SEpreds.peak.nino[1,])), date = rev(dates.2001))
peak.wtio2001.df <- data.frame(wtio = rev(as.numeric(SEpreds.peak.wtio[1,])), date = rev(dates.2001))


#ts plot setup
pred.time <- peak.nino2001.df$date
#pred.week <- pred.raw$week
pred.time.range <- range(pred.time)

#yearly ticks
month(pred.time.range[1])
month(pred.time.range[2])

x.ticks.pred <- seq(month(pred.time.range[1]), month(pred.time.range[2]), by = -1)
x.ticks.pred <- ymd(paste0("2001-", x.ticks.pred, "-01"))
x.pred.reduced <- x.ticks.pred #[1:20]

time.pred.plot <- as.Date(pred.time)

#TODO: setup y-axis



nino.2001 <- peak.nino2001.df$nino

#envelope plot setup
#nino:
over.nino <- nino.2001 >= 0
nino.top <- nino.2001
nino.top[!over.nino] <- 0
nino.bot <- nino.2001
nino.bot[over.nino] <-0

#TODO: fix the envelope plot

plot(time.pred.plot, nino.2001, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     #xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
    bty = "n", cex.lab = 1,  xpd = NA)
#axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
#     col = NA, line = 0,
#     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
text(x = x.pred.reduced,
     y = range(nino.2001)[1],
     labels = paste0(month(x.pred.reduced), "-", year(x.pred.reduced)),
     cex = 1, col = "black", xpd = NA)




wtio.2001 <- peak.wtio2001.df$wtio

#envelope plot setup
#nino:
over.wtio <- wtio.2001 >= 0
wtio.top <- wtio.2001
wtio.top[!over.wtio] <- 0
wtio.bot <- wtio.2001
wtio.bot[over.wtio] <-0

#TODO: fix the envelope plot

plot(time.pred.plot, wtio.2001, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     #xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     bty = "n", cex.lab = 1,  xpd = NA)
#axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
#     col = NA, line = 0,
#     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
text(x = x.pred.reduced,
     y = range(wtio.2001)[1],
     labels = paste0(month(x.pred.reduced), "-", year(x.pred.reduced)),
     cex = 1, col = "black", xpd = NA)



