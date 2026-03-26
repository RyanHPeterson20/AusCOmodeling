
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
full.range.manual <- c(-3.5, 5.25) 

#cex setup
line.width <- 1.75 #primary ts lines
subtitle.cex <- 1.5

#etio figures


#SI figure a 
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SI_fig3a_strongetio_ts.png", width = 3600, height = 2000, res = 275) 
par(mar = c(4.2, 4.25, 3, 1))
plot(1:length(s.index.son), s.index.son, type = "l",  col = "firebrick", 
     ylim = full.range.manual, xlim = c(11, 263), cex = 1.25,
     ylab = "Index", xlab = "Year", cex.lab = 1.25, lwd = line.width,
     axes = FALSE)
axis(1, at = year.loc.son, labels = years.pIOD, tick = FALSE, cex.axis = 1.05)
axis(2, at = seq(-2,4, by = 2), cex.axis = 1.2)
title("(a) Weekly S-Index and ETIO", adj = 0, cex.main = subtitle.cex)
box()
lines(1:length(etio.son), scale(-etio.son), col = "royalblue4", lwd = line.width)
abline(h = 0, lty = 2, lwd = 0.95, col = "gray23")
abline(v=son.div.full, lty = 1, lwd = 0.75, col = "gray45")

#TODO: move inside (bottom-right)
legend("bottomright",
       legend = c("S-Index", "-ETIO"),
       lty    = c(1, 1),                 # line type
       lwd    = line.width,                 # line width
       col    = c( "firebrick", "royalblue4"),
       #bty    = "n",               # no box; remove if you want a box
       #inset  = c(-0.152, 0 ),
       cex = 1.25, 
       xpd = TRUE)
dev.off()


png(filename = "SI_fig3b_strongetio_cor.png", width = 2000, height = 2000, res = 275)
par(mar = c(4.2, 4.25, 3, 1))
plot(etio.son, s.index.son, pch = 19, cex = 1.25,
     xlab = "ETIO", ylab = "S-Index",
     ylim = full.range.manual,
     cex.lab = 1.25, 
     axes = FALSE)
axis(1, cex.axis = 1.2)
axis(2, at = seq(-2,4, by = 2), cex.axis = 1.2)
title("(b) Correlation Weekly S-Index and ETIO", adj = 0, cex = subtitle.cex)
box()
points(etio.son[ind.2019], s.index.son[ind.2019], pch = 19, cex = 1.25, col = "firebrick")
#text(-1.75, -1.5, paste0("Cor = ", round(etio.son.cor, 3)), adj = 0 )
legend("bottomleft",
       legend = c("2019"),
       pch = 19,
       col = "firebrick",
       #bty    = "n",               # no box; remove if you want a box
       cex = 1.25, 
       xpd = TRUE)
dev.off()


#TODO: finish up here....
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SON_mod_wtio.png", width = 3000, height = 1500, res = 250)
par(mar = c(4.5, 4.5, 3, 7.65))
plot(1:length(m.index.son), m.index.son, type = "l",  col = "forestgreen", 
     ylim=mod.range, xlim = c(11, 263), 
     ylab = "Index", xlab = "Year", cex.lab = 1.15, lwd = 1.5,
     axes = FALSE)
axis(1, at = year.loc.son, labels = years.pIOD, tick = FALSE, cex.axis = 0.85)
axis(2)                      
box()
lines(1:length(wtio.son), wtio.son, col = "magenta4",  lwd = 1.5)
abline(h = 0, lty = 2, lwd = 0.95)
abline(v=son.div.full, lty = 3, lwd = 0.95, col = "gray30")
title("SON Weekly : M-Index & WTIO", adj = 0, cex = 1.1)
legend("topright",
       legend = c("M-Index", "WTIO"),
       lty    = c(1,1),                 # line type
       lwd    = 1.5,                 # line width
       col    = c( "forestgreen", "magenta4"),
       #bty    = "n",               # no box; remove if you want a box
       inset  = c(-0.152, 0 ),
       cex = 1.1, 
       xpd = TRUE)
dev.off()




png(filename = "SI_fig3d_modwtio_cor.png", width = 2000, height = 2000, res = 275)
par(mar = c(4.5, 4.25, 3, 1))
plot(wtio.son, m.index.son, pch = 19, cex = 0.89,
     xlab = "WTIO", ylab = "M-Index",
     cex.lab = 1.15, 
     axes = FALSE)
axis(1, cex.axis = 1.05)
axis(2, cex.axis = 1.05) 
title("SON: WTIO and M-Index", adj = 0, cex.main = subtitle.cex)
box()
points(wtio.son[ind.2019], m.index.son[ind.2019], pch = 19, cex = 0.89, col = "firebrick")
#text(-0.45, 2, paste0("Cor = ", round(wtio.son.cor, 3)), adj = 0 )

legend("bottomright",
       legend = c("2019"),
       pch = 19,
       col = "firebrick",
       #bty    = "n",               # no box; remove if you want a box
       cex = 0.89, 
       xpd = TRUE)
dev.off()




#correlation figures

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")

png(filename = "wtio_mindexSON.png", width = 2000, height = 1200, res = 250)
par(mar = c(4.25, 4.25, 3, 2))
plot(wtio.son, m.index.son, pch = 19, cex = 0.89,
     xlab = "WTIO", ylab = "M-Index",
     cex.lab = 1.15, 
     axes = FALSE)
axis(1, cex.axis = 1.05)
axis(2, cex.axis = 1.05)                      
box()
points(wtio.son[ind.2019], m.index.son[ind.2019], pch = 19, cex = 0.89, col = "firebrick")
#text(-0.45, 2, paste0("Cor = ", round(wtio.son.cor, 3)), adj = 0 )
title("SON: WTIO and M-Index", adj = 0, cex = 1.1)
legend("bottomright",
       legend = c("2019"),
       pch = 19,
       col = "firebrick",
       #bty    = "n",               # no box; remove if you want a box
       cex = 0.89, 
       xpd = TRUE)
dev.off()


