
#new figures, because why not


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)


load("Data/rmse.rda") #RMSE 

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

true.vals <- lapply(SE.preds, function(z) z$true)
#full model
base.preds <- lapply(SE.preds, function(z) z$base.fit)
base.upr <- lapply(SE.preds, function(z) z$base.upr)
base.lwr <- lapply(SE.preds, function(z) z$base.lwr)
#constant
const.preds <- lapply(SE.preds, function(z) z$const.fit)
const.upr <- lapply(SE.preds, function(z) z$const.upr)
const.lwr <- lapply(SE.preds, function(z) z$const.lwr)
#varying
vary.preds <- lapply(SE.preds, function(z) z$vary.fit)
vary.upr <- lapply(SE.preds, function(z) z$vary.upr)
vary.lwr <- lapply(SE.preds, function(z) z$vary.lwr)






setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "full_preds1.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 1:10) {
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
  title(seasons[i], adj = 0, cex.main = 1.25)

}

dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "full_preds2.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 11:20) {
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
  title(seasons[i], adj = 0, cex.main = 1.65)
  
}
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "const_preds1.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 1:10) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, const.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "magenta3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = const.upr[[i]],
               x2 = c(1:29),
               y2 = const.preds[[i]],
               col = alpha("orchid3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = const.lwr[[i]],
               x2 = c(1:29),
               y2 = const.preds[[i]],
               col = alpha("orchid3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "const_preds2.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 11:20) {
  par(mar = c(3, 2, 2, 1))  
  #plot
  plot(1:29, const.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
       lwd = 3, lty = 2, col = "magenta3",
       ylab = "", xlab = "", xlim = c(1.95, 28.05))
  box()
  axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  #upper bound
  envelopePlot(x1 = c(1:29),
               y1 = const.upr[[i]],
               x2 = c(1:29),
               y2 = const.preds[[i]],
               col = alpha("orchid3", 0.2),
               lineCol = NA)
  #lower bound
  envelopePlot(x1 = c(1:29),
               y1 = const.lwr[[i]],
               x2 = c(1:29),
               y2 = const.preds[[i]],
               col = alpha("orchid3", 0.2),
               lineCol = NA)
  lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
  text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds1.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 1:10) {
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
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_preds2.png", width = 3000, height = 3500, res = 300)
par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
for (i in 11:20) {
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
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


#for each group (subset of fire-seasons)
#true values
true.peak <- lapply(true.vals, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
#base (full) model
base.peak.preds <- lapply(base.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
base.peak.upr <-  lapply(base.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
base.peak.lwr <-  lapply(base.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
#constant model
const.peak.preds <- lapply(const.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
const.peak.upr <-  lapply(const.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
const.peak.lwr <-  lapply(const.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
#varying model
vary.peak.preds <- lapply(vary.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.peak.upr <-  lapply(vary.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
vary.peak.lwr <-  lapply(vary.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "base_peak_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), base.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "forestgreen",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = base.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = base.peak.preds[[i]],
               col = alpha("springgreen3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = base.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = base.peak.preds[[i]],
               col = alpha("springgreen3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "const_peak_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1))  
  plot(c(0.5, 1:4, 4.5), const.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "magenta3",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = const.peak.upr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = const.peak.preds[[i]],
               col = alpha("orchid3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(0.5, 1:4, 4.5),
               y1 = const.peak.lwr[[i]],
               x2 = c(0.5, 1:4, 4.5),
               y2 = const.peak.preds[[i]],
               col = alpha("orchid3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_peak_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
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
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


#early 
true.early <- lapply(true.vals, function(z) c(z[1:13], mean(z[13:14])))
#base (full) model
base.early.preds <- lapply(base.preds, function(z) c(z[1:13], mean(z[13:14])))
base.early.upr <-  lapply(base.upr, function(z) c(z[1:13], mean(z[13:14])))
base.early.lwr <-  lapply(base.lwr, function(z) c(z[1:13], mean(z[13:14])))
#constant model
const.early.preds <- lapply(const.preds, function(z) c(z[1:13], mean(z[13:14])))
const.early.upr <-  lapply(const.upr, function(z) c(z[1:13], mean(z[13:14])))
const.early.lwr <-  lapply(const.lwr, function(z) c(z[1:13], mean(z[13:14])))
#varying model
vary.early.preds <- lapply(vary.preds, function(z) c(z[1:13], mean(z[13:14])))
vary.early.upr <-  lapply(vary.upr, function(z) c(z[1:13], mean(z[13:14])))
vary.early.lwr <-  lapply(vary.lwr, function(z) c(z[1:13], mean(z[13:14])))



setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "base_early_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1)) 
  plot(c(1:13, 13.5), base.early.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 13.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "forestgreen",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[1:13], at = 1:13, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(1:13, 13.5), true.early[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = base.early.upr[[i]],
               x2 = c(1:13, 13.5),
               y2 = base.early.preds[[i]],
               col = alpha("springgreen3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = base.early.lwr[[i]],
               x2 = c(1:13, 13.5),
               y2 = base.early.preds[[i]],
               col = alpha("springgreen3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "const_early_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1)) 
  plot(c(1:13, 13.5), const.early.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 13.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "magenta3",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[1:13], at = 1:13, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(1:13, 13.5), true.early[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = const.early.upr[[i]],
               x2 = c(1:13, 13.5),
               y2 = const.early.preds[[i]],
               col = alpha("orchid3", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = const.early.lwr[[i]],
               x2 = c(1:13, 13.5),
               y2 = const.early.preds[[i]],
               col = alpha("orchid3", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "vary_early_preds.png", width = 4000, height = 5000, res = 300)
par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
for (i in 1:20) {
  par(mar = c(3, 2, 2, 1)) 
  plot(c(1:13, 13.5), vary.early.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 13.25),
       axes = FALSE, 
       lwd = 3, lty = 2, col = "darkorange2",
       ylab = "", xlab = "")
  box()
  axis(1, labels = season.weeks[1:13], at = 1:13, cex.axis = 1.45)
  axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
  lines(c(1:13, 13.5), true.early[[i]], lty = 1, lwd = 2, col = "grey5")
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = vary.early.upr[[i]],
               x2 = c(1:13, 13.5),
               y2 = vary.early.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  envelopePlot(x1 = c(1:13, 13.5),
               y1 = vary.early.lwr[[i]],
               x2 = c(1:13, 13.5),
               y2 = vary.early.preds[[i]],
               col = alpha("orange2", 0.25),
               lineCol = NA)
  abline(h=0, lty =3, col = "gray15", lwd = 2)
  title(seasons[i], adj = 0, cex.main = 1.25)
}
dev.off()


