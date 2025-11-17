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

#alternate colors (hex codes also in rcartocolor: peach/teal)
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
       box.col = "white", bg = "white",
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
       box.col = "white", bg = "white",
       xpd = NA, text.col = "grey40", cex = 2.5)
dev.off()



#old, delete below

#TODO: envelope Plot tests
#get color palettes/gradients to work in envelopePlot so that the color changes within the polygon
x1 = time.plot
y1 = resp.top
x2 = time.plot
y2 = rep(0, length(resp.time))
polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = top.col, border = NA)





ylab.vals <- c("Anomaly CO",
                 "Anomaly CO")



