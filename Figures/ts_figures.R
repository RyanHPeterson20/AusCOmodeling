##time series figures 

#create ts sub-figures for figure 1

#libraries
suppressMessages( library(fields)) #envelope plot
suppressMessages( library(scales)) #for alpha()
suppressMessages( library(lubridate)) #for temporal data
suppressMessages( library(rcartocolor)) #color choices
#TODO: add in libraries as needed


#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda")
#load .csv data
pred.raw <- read.csv("Data/pred_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
resp.raw  <- read.csv("Data/resp_anoms.csv", header = TRUE, stringsAsFactors = FALSE)
resp.alt.raw  <- read.csv("Data/resp_alt_anoms.csv", header = TRUE, stringsAsFactors = FALSE)


#setup (As needed)
#color setup
top.color <- "orange" #change to more red
bottom.color <- "cyan" #change to more blue

top.col <- "tomato2" #new red test
bot.col <- "steelblue2" #new blue test

#alternate colors (hex codes from rcartocolor: peach/teal)
top.col <- "#EF6A4CFF"
bot.col <- "#4F90A6FF"

#response ts
resp.raw <- resp.raw[resp.raw$date <= "2021-01-06", ]
#resp.raw <- resp.raw[resp.raw$year <= 2020, ]

## response anoms
NE.anom <- resp.raw$NEAus.anom
SE.anom <- resp.raw$SEAus.anom

#test scale anoms
test.for.zero(scale(NE.anom), (NE.anom - mean(NE.anom))/sd(NE.anom))
test.for.zero(scale(SE.anom), (SE.anom - mean(SE.anom))/sd(SE.anom))

#anom scales
NE.anom.std <- scale(NE.anom)
SE.anom.std <- scale(SE.anom)

#temporal setup
resp.time <- resp.raw$date
resp.week <- resp.raw$week

resp.time.range <- range(resp.time)


#yearly ticks
x.ticks <- seq(year(resp.time.range[1]), year(resp.time.range[2]), by = 1)
x.ticks <- ymd(paste0(x.ticks, "01", "01"))
x.ticks.reduced <- x.ticks[1:20]

#TODO: expand to both regions when single figure is finalized
#figure test for a single region

time.plot <- as.Date(resp.time)

#get y.ticks and labels
y.tick.max <- max(round(range(NE.anom.std)))
y.ticks <- seq(-y.tick.max, y.tick.max, by = 1)

y.tick.steps <- y.tick.max/4
y.tick.seq <- seq(y.tick.steps, y.tick.max-y.tick.steps, by = y.tick.steps)
y.tick.lab <- c(-rev(y.tick.seq), 0, y.tick.seq)

#get envelope plot data
over <- NE.anom.std >= 0
resp.top <- NE.anom.std
resp.top[!over] <- 0
resp.bot <- NE.anom.std
resp.bot[over] <- 0

#TODO: update these to align with the primary sub-figures
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "NEonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0,4,0,0))
par(oma = c(1.5,2,0,0))
par(mgp = c(4,1,0))

plot(time.plot, NE.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly CO", col.lab = "black", 
     xlim = c(as.Date(resp.time.range[1]) + months(7), as.Date(resp.time.range[2]) - months(7)),
     ylim = range(y.ticks), bty = "n", cex.lab = 1.25,  xpd = NA)
#bottom figure labels
text(x = x.ticks.reduced + months(6),
     y = range(y.ticks)[1]-0.7,
     labels = year(x.ticks.reduced),
     cex = 1.25, col = "black", xpd = NA)
abline(v = x.ticks[1:(length(x.ticks))],
       lty = 2, col = "grey", lwd = 2)
axis(side = 2, at = y.tick.lab, labels = y.tick.lab, cex.lab = 2, 
     col = NA, cex = 1.75,
     col.ticks = "black", col.axis = "black", las =1)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
#envelope plots
envelopePlot(x1 = time.plot,
             y1 = resp.top,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.plot,
             y1 = resp.bot,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
dev.off()



#SE Aus plot
y.tick.max <- max(round(range(SE.anom.std)))
y.ticks <- seq(-y.tick.max, y.tick.max, by = 1)

y.tick.steps <- y.tick.max/4
y.tick.seq <- seq(y.tick.steps, y.tick.max-y.tick.steps, by = y.tick.steps)
y.tick.lab <- c(-rev(y.tick.seq), 0, y.tick.seq)

#get envelope plot data
over <- SE.anom.std >= 0
resp.top <- SE.anom.std
resp.top[!over] <- 0
resp.bot <- SE.anom.std
resp.bot[over] <- 0

#output figures
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "SEonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.plot, SE.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly CO", col.lab = "black",
     xlim = c(as.Date(resp.time.range[1]) + months(7), as.Date(resp.time.range[2]) - months(7)),
     ylim = range(y.ticks), bty = "n", cex.lab = 1.25,  xpd = NA)
#bottom figure labels
text(x = x.ticks.reduced + months(6),
     y = range(y.ticks)[1]-0.7,
     labels = year(x.ticks.reduced),
     cex = 1.1, col = "black", xpd = NA)
abline(v = x.ticks[1:(length(x.ticks))],
       lty = 2, col = "grey", lwd = 2)
axis(side = 2, at = y.tick.lab, labels = y.tick.lab, col = NA, cex = 1.1,
     col.ticks = "black", col.axis = "black", las = 1)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
#envelope plots
envelopePlot(x1 = time.plot,
             y1 = resp.top,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.plot,
             y1 = resp.bot,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
dev.off()


#combined response time series

#perform for "base" and standardized response data

#base: using NE.anom and SE.anom
#TODO: remove when done
range(NE.anom)
range(SE.anom)

#TODO: get this working
#get y.ticks and labels (set-up by 10s)
y.NE.max <- round(max(NE.anom)/10)*10
y.SE.max <-  round(max(SE.anom)/10)*10

y.NE.min <- round(min(NE.anom)/10)*10
y.SE.min <- round(min(SE.anom)/10)*10

y.NE.ticks <- seq(y.NE.min, y.NE.max, by = 10)
y.SE.ticks <- seq(y.SE.min, y.SE.max, by = 10)

#get envelope setup
over.NE <- NE.anom >= 0
NE.top <- NE.anom
NE.top[!over.NE] <- 0
NE.bot <- NE.anom
NE.bot[over.NE] <-0

over.SE <- SE.anom >= 0
SE.top <- SE.anom
SE.top[!over.SE] <- 0
SE.bot <- SE.anom
SE.bot[over.SE] <-0


#response figure output:
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "response_ts.png", width = 4800, height = 2400, res = 250)
par(mfrow = c(2, 1))
par(mar = c(0,4,0,0), oma = c(1.5,2,0,0), mgp = c(4,1,0))

#NE Aus plot
plot(time.plot, NE.anom, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "CO Anomaly [ppb]", col.lab = "black",
     xlim = c(as.Date(resp.time.range[1]) + months(7), as.Date(resp.time.range[2]) - months(7)),
     ylim = range(y.NE.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.NE.ticks, cex.axis = 1.67, 
    col = NA, line = 0.5,
    col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks[1:(length(x.ticks))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.plot,
             y1 = NE.top,
             x2 = time.plot,
             y2 = rep(0, length(NE.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.plot,
             y1 = NE.bot,
             x2 = time.plot,
             y2 = rep(0, length(NE.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(45,40),
       legend = "NE Aus",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.5)

#SE Aus plot
par(mar = c(0,4,0.5,0))
plot(time.plot, SE.anom, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "CO Anomaly [ppb]", col.lab = "black",
     xlim = c(as.Date(resp.time.range[1]) + months(7), as.Date(resp.time.range[2]) - months(7)),
     ylim = range(y.SE.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.NE.ticks, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks[1:(length(x.ticks))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.plot,
             y1 = SE.top,
             x2 = time.plot,
             y2 = rep(0, length(SE.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.plot,
             y1 = SE.bot,
             x2 = time.plot,
             y2 = rep(0, length(SE.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
#full plot
text(x = x.ticks.reduced + months(6),
     y = range(y.SE.ticks)[1]-3,
     labels = year(x.ticks.reduced),
     cex = 1.67, col = "black", xpd = NA)
legend(x = c(ymd("2000-11-01"), ymd("2002-01-01")),
       y = c(45,40),
       legend = "SE Aus",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.5)
dev.off()



#predictor time series
pred.raw <- pred.raw[pred.raw$date <= "2021-01-06", ]

## pred anoms
nino.anom <- pred.raw$nino.anom
dmi.anom <- pred.raw$dmi.anom
wtio.anom <- pred.raw$wtio.anom
etio.anom <- pred.raw$etio.anom
tsa.anom <- pred.raw$tsa.anom
aao.anom <- pred.raw$aao.anom

#anom scales
nino.anom.std <- scale(nino.anom)
dmi.anom.std <- scale(dmi.anom)
wtio.anom.std <- scale(wtio.anom)
etio.anom.std <- scale(etio.anom)
tsa.anom.std <- scale(tsa.anom)
aao.anom.std <- scale(aao.anom)

#temporal setup
pred.time <- pred.raw$date
pred.week <- pred.raw$week

pred.time.range <- range(pred.time)

#yearly ticks
x.ticks.pred <- seq(year(pred.time.range[1]), year(pred.time.range[2]), by = 1)
x.ticks.pred <- ymd(paste0(x.ticks.pred, "01", "01"))
x.pred.reduced <- x.ticks.pred[1:21]

time.pred.plot <- as.Date(pred.time)

#get y.ticks and labels
#TODO: update y.ticks for each pred
y.nino.max <- max(round(range(nino.anom.std)))
y.nino.ticks <- seq(-y.nino.max, y.nino.max, by = 1)

y.dmi.max <- max(round(range(dmi.anom.std)))
y.dmi.ticks <- seq(-y.dmi.max, y.dmi.max, by = 1)

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

#finalize with respect to a common range
y.tick.max <- max(y.nino.max, y.dmi.max, y.wtio.max, y.etio.max, y.tsa.max, y.aao.max)
y.tick.steps <- y.tick.max/2
y.tick.seq <- seq(y.tick.steps, y.tick.max-y.tick.steps, by = y.tick.steps)
y.tick.lab <- c(-rev(y.tick.seq), 0, y.tick.seq)

#envelope plot setup
#nino:
over.nino <- nino.anom.std >= 0
nino.top <- nino.anom.std
nino.top[!over.nino] <- 0
nino.bot <- nino.anom.std
nino.bot[over.nino] <-0

#dmi:
over.dmi <- dmi.anom.std >= 0
dmi.top <- dmi.anom.std
dmi.top[!over.dmi] <- 0
dmi.bot <- dmi.anom.std
dmi.bot[over.dmi] <-0

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

#TODO: add olr

#individual plots
setwd("~/CO_AUS/AusCOmodeling/Figures")

#nino plot
png(filename = "NINOonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, nino.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.nino.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.nino.ticks)[1]-0.1,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures")

#dmi plot
png(filename = "DMIonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, dmi.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.dmi.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = dmi.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(dmi.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = dmi.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(dmi.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.dmi.ticks)[1]-0.1,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures")

#wtio plot
png(filename = "WTIOonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, wtio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.wtio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.wtio.ticks)[1]-0.1,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures")
#etio plot
png(filename = "ETIOonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, etio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.etio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.etio.ticks)[1]-0.2,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures")
#tsa plot
png(filename = "TSAonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, tsa.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.tsa.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.tsa.ticks)[1]-0.2,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Figures")
#aao plot
png(filename = "AAOonly_ts.png", width = 4800, height = 1200, res = 250)
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, aao.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.aao.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
text(x = x.pred.reduced + months(6),
     y = range(y.aao.ticks)[1]-0.2,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()

#TODO: add olr plot



#combined plots:

setwd("~/CO_AUS/AusCOmodeling/Figures")
##IOD plot for SI
png(filename = "IODpred_ts.png", width = 4800, height = 3600, res = 250)
#DMI, WTIO, ETIO in a single plot
par(mfrow = c(3, 1))
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

plot(time.pred.plot, dmi.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.dmi.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = dmi.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(dmi.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = dmi.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(dmi.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-10-01"), ymd("2000-01-01")),
       y = c(5, 4.5),
       legend = "DMI",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.5)

plot(time.pred.plot, wtio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.wtio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4.5, 4),
       legend = "WTIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.5)

plot(time.pred.plot, etio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.etio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4.5, 3.5),
       legend = "ETIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.5)
text(x = x.pred.reduced + months(6),
     y = range(y.etio.ticks)[1]-0.5,
     labels = year(x.pred.reduced),
     cex = 1.67, col = "black", xpd = NA)
dev.off()



##full pred plot: Nino, WTIO, ETIO, TSA, AAO (and OLR)
setwd("~/CO_AUS/AusCOmodeling/Figures")
#full pred ts
png(filename = "predictor_ts.png", width = 4800, height = 6000, res = 250)
par(mfrow = c(5, 1))
par(mar = c(0, 4, 0, 0))
par(oma = c(1.5, 2, 0, 0))
par(mgp = c(4, 1, 0))

#nino
plot(time.pred.plot, nino.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.nino.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = nino.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(nino.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4, 4),
       legend = "NINO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.75)

#wtio
plot(time.pred.plot, wtio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.wtio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = wtio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(wtio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4, 4),
       legend = "WTIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.75)

#etio
plot(time.pred.plot, etio.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.etio.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = etio.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(etio.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4, 3.5),
       legend = "ETIO",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.75)

#tsa
plot(time.pred.plot, tsa.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly (\u00B0C)", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.tsa.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = tsa.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(tsa.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4, 4),
       legend = "TSA",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.75)

#aao

plot(time.pred.plot, aao.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly", col.lab = "black",
     xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     ylim = range(y.aao.ticks), bty = "n", cex.lab = 1.75,  xpd = NA)
axis(side = 2, at = y.tick.lab, cex.axis = 1.67, 
     col = NA, line = 0.5,
     col.ticks = "black", col.axis = "black", las =1)
abline(v = x.ticks.pred[1:(length(x.ticks.pred))],
       lty = 2, col = "grey", lwd = 2)
abline(h = 0, lty = 1, col = "grey", lwd = 1)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.top,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.top)),
             col = alpha(top.col, 0.67),
             lineCol = NA)
envelopePlot(x1 = time.pred.plot,
             y1 = aao.bot,
             x2 = time.pred.plot,
             y2 = rep(0, length(aao.bot)),
             col = alpha(bot.col, 0.67),
             lineCol = NA)
legend(x = c(ymd("1999-09-01"), ymd("2000-01-01")),
       y = c(4, 4),
       legend = "SAM",
       box.col = NA, bg = NA,
       xpd = NA, text.col = "grey40", cex = 2.75)

text(x = x.pred.reduced + months(6),
     y = range(y.aao.ticks)[1]-0.5,
     labels = year(x.pred.reduced),
     cex = 2, col = "black", xpd = NA)
dev.off()



