
#positive IOD index from Cai et al (2021)

#libraries
suppressMessages( library(lubridate))

## import the appropriate data (weekly pca and iod)
#weekly PCA OISST data
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/pIOD/Data_SST")
load("pIODweekly_pca.rda")

#load in wtio/etio data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data

#TODO: find the old code (correlation_iod.rmd) for the time series and correlation plots 
#modified from correlation_iod.rmd
son.ind <- which(month(pred.df$date) %in% c(9,10,11))

son.pred.df <- pred.df[son.ind, ]

pIOD.start.ind <- which(son.pIOD.df$time == as_date(son.pred.df$date[1]))
pIOD.df <- son.pIOD.df[pIOD.start.ind:length(son.pIOD.df$index), ] 

pIOD.df$s.index <- scale(pIOD.df$s.index)
pIOD.df$m.index <- scale(pIOD.df$m.index)

colnames(pIOD.df) <- colnames(son.pIOD.df)

pc1.son <- pIOD.df$PC1
pc2.son <- pIOD.df$PC2

#scaled dmi (wtio/etio)
s.index.son  <- pIOD.df$s.index
etio.son <- scale(son.pred.df$etio.anom)
strong.range <- range(etio.son, s.index.son)

m.index.son  <- pIOD.df$m.index
wtio.son <- scale(son.pred.df$wtio.anom)
mod.range <- range(wtio.son, m.index.son)


#fig setup
years.pIOD <- 2000:2020

#son setup
n.son <- length(son.ind)
#length(s.index.son)
son.div <- which(diff(pIOD.df$year) !=0 ) + 0.5
son.div.full <-  c(0.5, son.div, n.son+0.5)

year.loc.son <- (head(son.div.full, -1) + tail(son.div.full, -1))/2

#cor setup
#get correlation values and plots
wtio.son.cor <- cor(wtio.son, m.index.son)
etio.son.cor <- cor(etio.son, s.index.son)

#get 2019 indices
ind.2019 <- which(pIOD.df$year == 2019)
#ind.2019 <- which(son.pred.df$year == 2019)


#full range from strong and moderate indices and manually adjusted range.
full.range <- range(strong.range, mod.range)
full.range.manual <- c(-3.75, 5.25) 

#cex setup
line.width <- 2.05 #primary ts lines
subtitle.cex <- 1.85
ylab.cex <- 1.75 
yaxis.cex <- 1.55
xlab.cex <- 1.75

#etio figures


#SI figure a 
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")

png(filename = "SI_fig3a_strongetio_ts.png", width = 3600, height = 2000, res = 275) 
par(mar = c(4.2, 4.25, 3, 1))
plot(1:length(s.index.son), s.index.son, type = "l",  col = "firebrick", 
     ylim = full.range.manual, xlim = c(11, 263), cex = 1.25,
     ylab = "", xlab = "", lwd = line.width, lty = 1, 
     axes = FALSE)
axis(1, at = year.loc.son, labels = years.pIOD, tick = FALSE, cex.axis = 1.10)
axis(2, at = seq(-2,4, by = 2), cex.axis = yaxis.cex, las = 1)
title("(a) Weekly S-Index and ETIO", adj = 0, cex.main = subtitle.cex)
mtext("Index", side = 2, cex = ylab.cex, line = 2.75) #y-axis
mtext("Year", side = 1, cex = xlab.cex, line = 2.75) #x-axis
box()
lines(1:length(etio.son), scale(-etio.son), col = "royalblue4", lwd = line.width, lty = 1)
abline(h = 0, lty = 1, lwd = 0.65, col = "gray23")
abline(v=son.div.full, lty = 1, lwd = 0.75, col = "gray45")

#bottom-right legend
legend("bottomright",
       legend = c("S-Index", "-ETIO"),
       lty    = c(1, 1),                 # line type
       lwd    = line.width,                 # line width
       col    = c( "firebrick", "royalblue4"),
       #bty    = "n",               # no box; remove if you want a box
       #inset  = c(-0.152, 0 ),
       cex = 1.5, 
       xpd = TRUE)
dev.off()


png(filename = "SI_fig3b_strongetio_cor.png", width = 2000, height = 2000, res = 275)
par(mar = c(4.2, 4.25, 3, 1))
plot(etio.son, s.index.son, pch = 19, cex = 1.25, 
     xlab = "", ylab = "",
     ylim = full.range.manual, 
     axes = FALSE)
axis(1, cex.axis = yaxis.cex)
axis(2, at = seq(-2,4, by = 2), cex.axis = yaxis.cex, las = 1)
title("(b) Correlation Weekly S-Index and ETIO", adj = 0, cex.main = subtitle.cex)
mtext("S-Index", side = 2, cex = ylab.cex, line = 2.75) #y-axis
mtext("ETIO", side = 1, cex = xlab.cex, line = 2.75) #x-axis
box()
points(etio.son[ind.2019], s.index.son[ind.2019], pch = 23, cex = 1.5, col = "gray3", bg = "red1", lwd = 1.25)
#text(-1.75, -1.5, paste0("Cor = ", round(etio.son.cor, 3)), adj = 0 )
legend("bottomleft",
       legend = c("2019"),
       pch = 23,
       col = "gray3", pt.bg = "red1", pt.lwd = 1.25,
       pt.cex = 1.5,
       #bty    = "n",               # no box; remove if you want a box
       cex = 1.65, 
       xpd = TRUE)
dev.off()



#wtio figures

png(filename = "SI_fig3c_modwtio_ts.png", width = 3600, height = 2000, res = 275) 
par(mar = c(4.2, 4.25, 3, 1))
plot(1:length(m.index.son), m.index.son, type = "l",  col = "forestgreen", 
     ylim = full.range.manual, xlim = c(11, 263), cex = 1.25,
     ylab = "", xlab = "", lwd = line.width, lty = 1, 
     axes = FALSE)
axis(1, at = year.loc.son, labels = years.pIOD, tick = FALSE, cex.axis = 1.10)
axis(2, at = seq(-2,4, by = 2), cex.axis = yaxis.cex, las = 1)
title("(a) Weekly M-Index and WTIO", adj = 0, cex.main = subtitle.cex)
mtext("Index", side = 2, cex = ylab.cex, line = 2.75) #y-axis
mtext("Year", side = 1, cex = xlab.cex, line = 2.75) #x-axis
box()
lines(1:length(wtio.son), scale(wtio.son), col = "magenta4", lwd = line.width, lty = 1)
abline(h = 0, lty = 1, lwd = 0.65, col = "gray23")
abline(v=son.div.full, lty = 1, lwd = 0.75, col = "gray45")
#top-right legend
legend("topright",
       legend = c("M-Index", "WTIO"),
       lty    = c(1, 1),                 # line type
       lwd    = line.width,                 # line width
       col    = c( "forestgreen", "magenta4"),
       #bty    = "n",               # no box; remove if you want a box
       #inset  = c(-0.152, 0 ),
       cex = 1.5, 
       xpd = TRUE)

dev.off()


png(filename = "SI_fig3d_modwtio_cor.png", width = 2000, height = 2000, res = 275)
par(mar = c(4.5, 4.25, 3, 1))
plot(wtio.son, m.index.son, pch = 19, cex = 1.25, 
     xlab = "", ylab = "",
     ylim = full.range.manual, 
     axes = FALSE)
axis(1, cex.axis = yaxis.cex)
axis(2, at = seq(-2,4, by = 2), cex.axis = yaxis.cex, las = 1)
title("(b) Correlation Weekly M-Index and WTIO", adj = 0, cex.main = subtitle.cex)
mtext("M-Index", side = 2, cex = ylab.cex, line = 2.75) #y-axis
mtext("WTIO", side = 1, cex = xlab.cex, line = 2.75) #x-axis
box()
points(wtio.son[ind.2019], m.index.son[ind.2019], pch = 23, cex = 1.5, col = "gray23", bg = "red1", lwd = 1.25)
legend("topleft",
       legend = c("2019"),
       pch = 23,
       col = "gray23", pt.bg = "red1", pt.lwd = 1.25,
       pt.cex = 1.5,
       #bty    = "n",               # no box; remove if you want a box
       cex = 1.65, 
       xpd = TRUE)
dev.off()

