##time series figures 

#create ts sub-figures for figure 1

#libraries
suppressMessages( library(fields)) #envelope plot
suppressMessages( library(scales)) #for alpha()
suppressMessages( library(lubridate)) #for temporal data
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

#response ts

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

#TODO: check on response ranges

#yearly ticks
x.ticks <- seq(year(resp.time.range[1]), year(resp.time.range[2]), by = 1)
x.ticks <- ymd(paste0(x.ticks, "01", "01"))


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

#TODO: finalize the plot (cut off )
par(mfrow = c(2, 1))

par(mar = c(0,2,0,0))
par(oma = c(1.5,2,0,0))
par(mgp = c(2.75,1,0))

plot(time.plot, NE.anom.std, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly CO", col.lab = "black",
     xlim = c(as.Date(resp.time.range[1]) + months(7), as.Date(resp.time.range[2]) - months(7)),
     ylim = range(y.ticks), bty = "n", cex.lab = 1.25,  xpd = NA)
#bottom figure labels
text(x = x.ticks + months(6),
     y = range(y.ticks)[1]-0.7,
     labels = year(x.ticks),
     cex = 1.1, col = "black", xpd = NA)
abline(v = x.ticks[1:(length(x.ticks))],
       lty = 2, col = "grey", lwd = 2)
axis(side = 2, at = y.tick.lab, labels = y.tick.lab, col = NA, cex = 1.1,
     col.ticks = "black", col.axis = "black")
abline(h = 0, lty = 1, col = "grey", lwd = 1)
#envelope plots
envelopePlot(x1 = time.plot,
             y1 = resp.top,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(top.col, 0.7),
             lineCol = NA)
envelopePlot(x1 = time.plot,
             y1 = resp.bot,
             x2 = time.plot,
             y2 = rep(0, length(resp.time)),
             col = alpha(bot.col, 0.7),
             lineCol = NA)


#TODO: envelope Plot tests
#get color palettes/gradients to work in envelopePlot so that the color changes within the polygon
x1 = time.plot
y1 = resp.top
x2 = time.plot
y2 = rep(0, length(resp.time))
polygon(c(x1, rev(x2)), c(y1, rev(y2)), col = top.col, border = NA)





ylab.vals <- c("Anomaly CO",
                 "Anomaly CO")



