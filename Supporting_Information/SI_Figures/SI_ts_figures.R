
#for SI only time series figures

#currently includes:
## SI Figure 1 (predictor time series)

#TODO: move over stuff from ts_figures.R and pred_term_ts.R as needed


#libraries
suppressMessages( library(fields)) #envelope plot
suppressMessages( library(scales)) #for alpha()
suppressMessages( library(lubridate)) #for temporal data
suppressMessages( library(rcartocolor)) #color choices

#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda")
#load .csv data
pred.raw <- read.csv("Data/pred_anoms.csv", header = TRUE, stringsAsFactors = FALSE)

#color setup
#response colors
top.col.resp <- "#C71C1C"
bot.col.resp <- "#2A5674FF"

#predictor colors
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"


## predictor time series set-up
pred.raw <- pred.raw[pred.raw$date >= "2001-01-03" & pred.raw$date <= "2021-01-06", ]

## predictor anoms (std)
nino.anom.std <- scale(pred.raw$nino.anom)
wtio.anom.std <- scale(pred.raw$wtio.anom)
etio.anom.std <- scale(pred.raw$etio.anom)
tsa.anom.std <- scale(pred.raw$tsa.anom)
aao.anom.std <- scale(pred.raw$aao.anom)
olr.anom.std <- scale(pred.raw$olr.anom)


#preds:
pred.time <- pred.raw$date
pred.week <- pred.raw$week
pred.time.range <- range(pred.time)

#yearly ticks
x.ticks.pred <- seq(year(pred.time.range[1]), year(pred.time.range[2]), by = 1)
x.ticks.pred <- ymd(paste0(x.ticks.pred, "01", "01"))
x.pred.reduced <- x.ticks.pred[1:20]

time.pred.plot <- as.Date(pred.time)


#predictors setup:
y.nino.max <- max(round(range(nino.anom.std)))
y.nino.ticks <- seq(-y.nino.max, y.nino.max, by = 1)

y.wtio.max <- max(round(range(wtio.anom.std)))
y.wtio.ticks <- seq(-y.wtio.max, y.wtio.max, by = 1)

#uses abs max
y.etio.max <- max(abs(round(range(etio.anom.std))))
y.etio.ticks <- seq(-y.etio.max, y.etio.max, by = 1)

y.tsa.max <- max(round(range(tsa.anom.std)))
y.tsa.ticks <- seq(-y.tsa.max, y.tsa.max, by = 1)

#uses abs max
y.aao.max <- max(abs(round(range(aao.anom.std))))
y.aao.ticks <- seq(-y.aao.max, y.aao.max, by = 1)

y.olr.max <- max(abs(round(range(olr.anom.std))))
y.olr.ticks <- seq(-y.olr.max, y.olr.max, by = 1)

#finalize preds with common range
y.tick.max <- max(y.nino.max, y.wtio.max, y.etio.max, y.tsa.max, y.aao.max)
y.tick.steps <- y.tick.max/2
y.tick.seq <- seq(y.tick.steps, y.tick.max-y.tick.steps, by = y.tick.steps)
y.tick.lab <- c(-rev(y.tick.seq), 0, y.tick.seq)

#preds
#nino:
over.nino <- nino.anom.std >= 0
nino.top <- nino.anom.std
nino.top[!over.nino] <- 0
nino.bot <- nino.anom.std
nino.bot[over.nino] <-0
#wtio:
over.wtio <- wtio.anom.std >= 0
wtio.top <- wtio.anom.std
wtio.top[!over.wtio] <- 0
wtio.bot <- wtio.anom.std
wtio.bot[over.wtio] <-0
#etio:
over.etio <- etio.anom.std >= 0
etio.top <- etio.anom.std
etio.top[!over.etio] <- 0
etio.bot <- etio.anom.std
etio.bot[over.etio] <-0
#tsa:
over.tsa <- tsa.anom.std >= 0
tsa.top <- tsa.anom.std
tsa.top[!over.tsa] <- 0
tsa.bot <- tsa.anom.std
tsa.bot[over.tsa] <-0
#aao
over.aao <- aao.anom.std >= 0
aao.top <- aao.anom.std
aao.top[!over.aao] <- 0
aao.bot <- aao.anom.std
aao.bot[over.aao] <-0
#olr
over.olr <- olr.anom.std >= 0
olr.top <- olr.anom.std
olr.top[!over.olr] <- 0
olr.bot <- olr.anom.std
olr.bot[over.olr] <-0


lab.cex <- 3.05
legend.cex <- 3.25

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")

png(filename = "SI_pred_ts.png", width = 5000, height = 6000, res = 250)
par(mfrow = c(6, 1))

par(mar = c(0, 5, 0, 0))
par(oma = c(2, 3.5, 1, 0))
par(mgp = c(4,1,0))

#predictor data figures
#nino sub-figure
plot(time.pred.plot, nino.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.nino.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
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
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "Ni\u00f1o 3.4",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)



#etio sub-figure
plot(time.pred.plot, etio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.etio.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "ETIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)

#wtio sub-figure
plot(time.pred.plot, wtio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.wtio.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
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
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "WTIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)


#tsa sub-figure
plot(time.pred.plot, tsa.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.tsa.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "TSA",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)

#aao (sam) sub-figure
plot(time.pred.plot, aao.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.aao.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "SAM",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)


#olr sub-figure
plot(time.pred.plot, olr.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.olr.ticks), bty = "n", cex.lab = lab.cex,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
     col = NA, line = 0,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = olr.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(olr.top)),
             col = alpha(top.col.pred, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = olr.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(olr.bot)),
             col = alpha(bot.col.pred, 0.67),
             lineCol = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(4, 4),
       legend = "OLR",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey30", cex = legend.cex)

text(x = x.pred.reduced + months(6),
     y = range(y.olr.ticks)[1]-0.5,
     labels = year(x.pred.reduced),
     cex = 3, col = "black", xpd = NA)

dev.off()



