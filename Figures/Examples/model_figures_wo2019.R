
#include "yellow" and "blue" models
## "yellow" models = w/o single year
## "blue" models = w/o single year and w/o 2019/2020


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data


#season years/weeks
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)


SE.preds <- SEvalid$preds
SE.wo2019.preds <- SErefit.wo2019$preds

true.vals <- lapply(SE.wo2019.preds, function(z) z$true)
#full model
base.preds <- lapply(SE.preds, function(z) z$base.fit)
base.upr <- lapply(SE.preds, function(z) z$base.upr)
base.lwr <- lapply(SE.preds, function(z) z$base.lwr)
#varying
vary.preds <- lapply(SE.preds, function(z) z$vary.fit)
vary.upr <- lapply(SE.preds, function(z) z$vary.upr)
vary.lwr <- lapply(SE.preds, function(z) z$vary.lwr)
#varying wo 2019
vary.wo2019.preds <- lapply(SE.wo2019.preds, function(z) z$vary.fit)
vary.wo2019.upr <- lapply(SE.wo2019.preds, function(z) z$vary.upr)
vary.wp2019.lwr <- lapply(SE.wo2019.preds, function(z) z$vary.lwr)



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds1_wo2019.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 1:10) {
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds2_wo2019.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 11:20) {
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


#let's try to get everything into a single plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_wo2019_full.png", width = 5000, height = 4000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()

#combined varying terms 

#TODO: add in these *varying w/ and w/o 2019* stacked (Alternating rows)

#add different for loops eg. for(1:4), for(5:8)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_full.png", width = 5000, height = 4000, res = 275)
par(mfrow = c(4, 5), oma = c(2, 2, 2, 1))
for (i in 1:5) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
}  
for (i in 1:5) {
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
}
for (i in 6:10) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
}  
for (i in 6:10) {
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
}


dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_all1.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 1:5) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  
}
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_all2.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 6:10) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_all3.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 11:15) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds_all3.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(4, 2),oma = c(2, 2, 2, 1))
for (i in c(16:18, 20)) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  
}
dev.off()



#just peak preds
#true values
true.peak <- lapply(true.vals, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
#varying model
vary.peak.preds <- lapply(vary.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.peak.upr <-  lapply(vary.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.peak.lwr <-  lapply(vary.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
#varying wo 2019
vary.wo2019.peak.preds <- lapply(vary.wo2019.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.wo2019.peak.upr <-  lapply(vary.wo2019.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.wo2019.peak.lwr <-  lapply(vary.wp2019.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "varyWo2019_peak_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), vary.wo2019.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_peak_predsAll1.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:10) {
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), vary.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.peak.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.peak.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  

  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), vary.wo2019.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_peak_predsAll2.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in c(11:18, 20)) {
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), vary.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.peak.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.peak.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(paste0(seasons[i], " Withheld"), adj = 0, cex.main = 1.25)
  
  
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), vary.wo2019.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = vary.wo2019.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = vary.wo2019.peak.preds[[i]],
               col = alpha("steelblue3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(paste0(seasons[i], " and ", seasons[19], " Withheld"), adj = 0, cex.main = 1.25)
}
dev.off()


#repeat for all years

#add in some other stuff


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
for (i in 1:20) {
  png(filename =  paste0("new_preds_", season.years[i], ".png"), width = 5000, height = 2500, res = 300)
  par(mfrow = c(3, 1),oma = c(2, 2, 2, 1))
  par(mar = c(3, 2, 2, 1))
  #plot
  plot(1:29, base.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "forestgreen",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = base.upr[[i]],
               x2 = c(1:29),
               y2 = base.preds[[i]],
               col = alpha("springgreen3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = base.lwr[[i]],
               x2 = c(1:29),
               y2 = base.preds[[i]],
               col = alpha("springgreen3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title("", adj = 0, cex.main = 1.25)
  
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.upr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.preds[[i]],
               col = alpha("orange2", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title("", adj = 0, cex.main = 1.25)
  
  par(mar = c(3, 2, 2, 1)) 
  #plot
  plot(1:29, vary.wo2019.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "royalblue3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wo2019.upr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = vary.wp2019.lwr[[i]],
               x2 = c(1:29),
               y2 = vary.wo2019.preds[[i]],
               col = alpha("steelblue3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title("", adj = 0, cex.main = 1.25)
  title(seasons[i], adj = 0.05, cex.main = 2.5, outer = TRUE)
  
  dev.off()
  
}



