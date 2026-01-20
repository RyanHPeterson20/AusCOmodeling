##repeated prediction outputs for several different withheld years

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data

#setup (seasons)
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#
SE.preds <- SErefit.wo.years$preds

#get color scale for various blues
cols.line <- colorRampPalette(c("royalblue2", "royalblue4"))(20)
cols.envs <- colorRampPalette(c("steelblue1", "steelblue4"))(20)


#year preds 
for (j in 1:length(seasons)) {
  
  true.vals <- lapply(SE.preds[[j]], function(z) z$true)
  vary.preds <- lapply(SE.preds[[j]], function(z) z$vary.fit)
  vary.upr <- lapply(SE.preds[[j]], function(z) z$vary.upr)
  vary.lwr <- lapply(SE.preds[[j]], function(z) z$vary.lwr)
  
  setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/alt_withheld")
  png(filename = paste0("vary_", season.years[j], "_preds1.png"), width = 3000, height = 3500, res = 300)
  par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
  for (i in 1:10) {
    par(mar = c(3, 2, 2, 1))
    #plot
    plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
         lwd = 3, lty = 2, col = cols.line[[i]],
         ylab = "", xlab = "", xlim = c(1.95, 28.05))
    box()
    axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
    axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
    #upper bound
    envelopePlot(x1 = c(1:29),
                 y1 = vary.upr[[i]],
                 x2 = c(1:29),
                 y2 = vary.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.2),
                 lineCol = NA)
    #lower bound
    envelopePlot(x1 = c(1:29),
                 y1 = vary.lwr[[i]],
                 x2 = c(1:29),
                 y2 = vary.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.2),
                 lineCol = NA)
    lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
    abline(h=0, lty =3, col = "gray15", lwd = 2)
    abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
    text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
    title(seasons[i], adj = 0, cex.main = 1.25)
  }
  title(seasons[j], adj = 0.05, cex.main = 2.25, outer = TRUE)
  dev.off()
  
  
  setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/alt_withheld")
  png(filename = paste0("vary_", season.years[j], "_preds2.png"), width = 3000, height = 3500, res = 300)
  par(mfrow = c(5, 2),oma = c(2, 2, 2, 1))
  for (i in 11:20) {
    par(mar = c(3, 2, 2, 1))
    #plot
    plot(1:29, vary.preds[[i]], type = "l", ylim = c(-50,50), axes = FALSE, 
         lwd = 3, lty = 2, col = cols.line[[i]],
         ylab = "", xlab = "", xlim = c(1.95, 28.05))
    box()
    axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
    axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
    #upper bound
    envelopePlot(x1 = c(1:29),
                 y1 = vary.upr[[i]],
                 x2 = c(1:29),
                 y2 = vary.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.2),
                 lineCol = NA)
    #lower bound
    envelopePlot(x1 = c(1:29),
                 y1 = vary.lwr[[i]],
                 x2 = c(1:29),
                 y2 = vary.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.2),
                 lineCol = NA)
    lines(1:29, true.vals[[i]], lty = 1, lwd = 2, col = "grey5")
    abline(h=0, lty =3, col = "gray15", lwd = 2)
    abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
    text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)
    title(seasons[i], adj = 0, cex.main = 1.25)
  }
  title(seasons[j], adj = 0.05, cex.main = 2.25, outer = TRUE)
  dev.off()
}



#repeat for peak (early and late)

for (j in 1:20) {
    
  true.vals <- lapply(SE.preds[[j]], function(z) z$true)
  vary.preds <- lapply(SE.preds[[j]], function(z) z$vary.fit)
  vary.upr <- lapply(SE.preds[[j]], function(z) z$vary.upr)
  vary.lwr <- lapply(SE.preds[[j]], function(z) z$vary.lwr)
  
  
  #true values
  true.peak <- lapply(true.vals, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
  #varying model
  vary.peak.preds <- lapply(vary.preds, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
  vary.peak.upr <-  lapply(vary.upr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
  vary.peak.lwr <-  lapply(vary.lwr, function(z) c(mean(z[13:14]), z[14:17], mean(z[17:18])))
  
  setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/alt_withheld")
  png(filename = paste0("peak_", season.years[j], "_preds.png"), width = 4000, height = 5000, res = 300)
  par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
  for (i in 1:20) {
    par(mar = c(3, 2, 2, 1))  
    plot(c(0.5, 1:4, 4.5), vary.peak.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 4.25),
         axes = FALSE, 
         lwd = 3, lty = 2, col = cols.line[[i]],
         ylab = "", xlab = "")
    box()
    axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = 1.45)
    axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
    lines(c(0.5, 1:4, 4.5), true.peak[[i]], lty = 1, lwd = 2, col = "grey5")
    envelopePlot(x1 = c(0.5, 1:4, 4.5),
                 y1 = vary.peak.upr[[i]],
                 x2 = c(0.5, 1:4, 4.5),
                 y2 = vary.peak.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    envelopePlot(x1 = c(0.5, 1:4, 4.5),
                 y1 = vary.peak.lwr[[i]],
                 x2 = c(0.5, 1:4, 4.5),
                 y2 = vary.peak.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    abline(h=0, lty =3, col = "gray15", lwd = 2)
    title(seasons[i], adj = 0, cex.main = 1.25)
  }
  #TODO: add in outer title
  dev.off()
  
  #early 
  true.early <- lapply(true.vals, function(z) c(z[1:13], mean(z[13:14])))
  #varying model
  vary.early.preds <- lapply(vary.preds, function(z) c(z[1:13], mean(z[13:14])))
  vary.early.upr <-  lapply(vary.upr, function(z) c(z[1:13], mean(z[13:14])))
  vary.early.lwr <-  lapply(vary.lwr, function(z) c(z[1:13], mean(z[13:14])))
  
  
  setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/alt_withheld")
  png(filename = paste0("early_", season.years[j], "_preds.png"), width = 4000, height = 5000, res = 300)
  par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
  for (i in 1:20) {
    par(mar = c(3, 2, 2, 1))  
    plot(c(1:13, 13.5), vary.early.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.75, 13.25),
         axes = FALSE, 
         lwd = 3, lty = 2, col = cols.line[[i]],
         ylab = "", xlab = "")
    box()
    axis(1, labels = season.weeks[1:13], at = 1:13, cex.axis = 1.45)
    axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
    lines(c(1:13, 13.5), true.early[[i]], lty = 1, lwd = 2, col = "grey5")
    envelopePlot(x1 = c(1:13, 13.5),
                 y1 = vary.early.upr[[i]],
                 x2 = c(1:13, 13.5),
                 y2 = vary.early.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    envelopePlot(x1 = c(1:13, 13.5),
                 y1 = vary.early.lwr[[i]],
                 x2 = c(1:13, 13.5),
                 y2 = vary.early.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    abline(h=0, lty =3, col = "gray15", lwd = 2)
    title(seasons[i], adj = 0, cex.main = 1.25)
  }
  #TODO: add in outer title
  dev.off()
  
  
  #late fire season
  true.late <- lapply(true.vals, function(z) c(mean(z[17:18]), z[18:29]))
  #varying model
  vary.late.preds <- lapply(vary.preds, function(z) c(mean(z[17:18]), z[18:29]))
  vary.late.upr <-  lapply(vary.upr, function(z) c(mean(z[17:18]), z[18:29]))
  vary.late.lwr <-  lapply(vary.lwr, function(z)c(mean(z[17:18]), z[18:29]))

  
  
  setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/alt_withheld")
  png(filename = paste0("late_", season.years[j], "_preds.png"), width = 4000, height = 5000, res = 300)
  par(mfrow = c(5, 4),oma = c(2, 2, 2, 1))
  for (i in 1:20) {
    par(mar = c(3, 2, 2, 1))  
    plot(c(0.5, 1:12), vary.late.preds[[i]], type = "l", ylim = c(-50, 50), xlim = c(0.25, 12.25),
         axes = FALSE, 
         lwd = 3, lty = 2, col = cols.line[[i]],
         ylab = "", xlab = "")
    box()
    axis(1, labels = season.weeks[18:29], at = 1:12, cex.axis = 1.45)
    axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
    lines(c(0.5, 1:12), true.late[[i]], lty = 1, lwd = 2, col = "grey5")
    envelopePlot(x1 = c(0.5, 1:12),
                 y1 = vary.late.upr[[i]],
                 x2 = c(0.5, 1:12),
                 y2 = vary.late.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    envelopePlot(x1 = c(0.5, 1:12),
                 y1 = vary.late.lwr[[i]],
                 x2 = c(0.5, 1:12),
                 y2 = vary.late.preds[[i]],
                 col = alpha(cols.envs[[i]], 0.25),
                 lineCol = NA)
    abline(h=0, lty =3, col = "gray15", lwd = 2)
    title(seasons[i], adj = 0, cex.main = 1.25)
  }
  #TODO: add in outer title
  dev.off()
}
