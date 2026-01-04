
#model figures

#figure order:
#1. prediction (2019-2020)
#2. Coefficient/interaction plots (moved to coef_int_figures.R)
#3. lag/son plots


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot


#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/preds_2019.rda") #2019 predictions 

load("Data/rmse.rda") #RMSE 

#setup
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

#setup from NEmodels, SEmodels
NE1.lm <- NEmodels[[1]]
NE2.lm <- NEmodels[[2]]
NE3.lm <- NEmodels[[3]]

SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#from NEmodels.loyo, SEmodels.loyo
NE.const.LM <- NEmodels.loyo[[2]]
NE.vary.LM <- NEmodels.loyo[[3]]

SE.const.LM <- SEmodels.loyo[[2]]
SE.vary.LM <- SEmodels.loyo[[3]]

#model review
#NE Aus Early (Group 1)
summary(NE1.lm)
summary(NE.const.LM$`2019-2020`[[1]])
summary(NE.vary.LM$`2019-2020`[[1]])

#NE Aus Middle (Group 2)
summary(NE2.lm)
summary(NE.const.LM$`2019-2020`[[2]])
summary(NE.vary.LM$`2019-2020`[[2]])

#NE Aus Late (Group 3)
summary(NE3.lm)
summary(NE.const.LM$`2019-2020`[[3]])
summary(NE.vary.LM$`2019-2020`[[3]])

#SE Aus Early (Group 1)
summary(SE1.lm)
summary(SE.const.LM$`2019-2020`[[1]])
summary(SE.vary.LM$`2019-2020`[[1]])

#SE Aus Middle (Group 2)
summary(SE2.lm)
summary(SE.const.LM$`2019-2020`[[2]])
summary(SE.vary.LM$`2019-2020`[[2]])

#SE Aus Late (Group 3)
summary(SE3.lm)
summary(SE.const.LM$`2019-2020`[[3]])
summary(SE.vary.LM$`2019-2020`[[3]])

#preds setup
pred.base.early <- preds.2019.base[[1]]
pred.base.mid <- preds.2019.base[[2]]
pred.base.late <- preds.2019.base[[3]]

pred.const.early <- preds.2019.const[[1]]
pred.const.mid <- preds.2019.const[[2]]
pred.const.late <- preds.2019.const[[3]]

pred.vary.early <- preds.2019.vary[[1]]
pred.vary.mid <- preds.2019.vary[[2]]
pred.vary.late <- preds.2019.vary[[3]]

## --- Model Predictions --- ##

#get 2019/2020 SE Aus prediction and 95% PI for base, const, and vary (3 figures)
#setup
pred.base.fit <- c(pred.base.early$fit[,1], pred.base.mid$fit[,1], pred.base.late$fit[,1])
pred.base.lwr <- c(pred.base.early$fit[,2], pred.base.mid$fit[,2], pred.base.late$fit[,2])
pred.base.upr <- c(pred.base.early$fit[,3], pred.base.mid$fit[,3], pred.base.late$fit[,3])

pred.const.fit <- c(pred.const.early$fit[,1], pred.const.mid$fit[,1], pred.const.late$fit[,1])
pred.const.lwr <- c(pred.const.early$fit[,2], pred.const.mid$fit[,2], pred.const.late$fit[,2])
pred.const.upr <- c(pred.const.early$fit[,3], pred.const.mid$fit[,3], pred.const.late$fit[,3])

pred.vary.fit <- c(pred.vary.early$fit[,1], pred.vary.mid$fit[,1], pred.vary.late$fit[,1])
pred.vary.lwr <- c(pred.vary.early$fit[,2], pred.vary.mid$fit[,2], pred.vary.late$fit[,2])
pred.vary.upr <- c(pred.vary.early$fit[,3], pred.vary.mid$fit[,3], pred.vary.late$fit[,3])

base.range <- range(SE.2019.true, pred.base.fit, pred.base.lwr, pred.base.upr)
const.range <- range(SE.2019.true, pred.const.fit, pred.const.lwr, pred.const.upr)
vary.range <- range(SE.2019.true, pred.vary.fit, pred.vary.lwr, pred.vary.upr)
all.range <- range(base.range, const.range, vary.range)


#updated predictions for variations and groups
#groups for true SE 2019 values
SE2019.early <- c(SE.2019.true[1:13], mean(SE.2019.true[13:14]))
SE2019.mid <- c(mean(SE.2019.true[13:14]), SE.2019.true[14:17], mean(SE.2019.true[17:18]))
SE2019.late <- c(mean(SE.2019.true[17:18]),  SE.2019.true[18:29])

#base group preds
base.upr.early <- c(pred.base.upr[1:13], mean(pred.base.upr[13:14]))
base.upr.mid <- c(mean(pred.base.upr[13:14]), pred.base.upr[14:17], mean(pred.base.upr[17:18]))
base.upr.late <- c(mean(pred.base.upr[17:18]),  pred.base.upr[18:29])

base.fit.early <- c(pred.base.fit[1:13], mean(pred.base.fit[13:14]))
base.fit.mid <- c(mean(pred.base.fit[13:14]), pred.base.fit[14:17], mean(pred.base.fit[17:18]))
base.fit.late <- c(mean(pred.base.fit[17:18]),  pred.base.fit[18:29])

base.lwr.early <- c(pred.base.lwr[1:13], mean(pred.base.lwr[13:14]))
base.lwr.mid <- c(mean(pred.base.lwr[13:14]), pred.base.lwr[14:17], mean(pred.base.lwr[17:18]))
base.lwr.late <- c(mean(pred.base.lwr[17:18]),  pred.base.lwr[18:29])

#constant group preds
const.upr.early <- c(pred.const.upr[1:13], mean(pred.const.upr[13:14]))
const.upr.mid <- c(mean(pred.const.upr[13:14]), pred.const.upr[14:17], mean(pred.const.upr[17:18]))
const.upr.late <- c(mean(pred.const.upr[17:18]),  pred.const.upr[18:29])

const.fit.early <- c(pred.const.fit[1:13], mean(pred.const.fit[13:14]))
const.fit.mid <- c(mean(pred.const.fit[13:14]), pred.const.fit[14:17], mean(pred.const.fit[17:18]))
const.fit.late <- c(mean(pred.const.fit[17:18]),  pred.const.fit[18:29])

const.lwr.early <- c(pred.const.lwr[1:13], mean(pred.const.lwr[13:14]))
const.lwr.mid <- c(mean(pred.const.lwr[13:14]), pred.const.lwr[14:17], mean(pred.const.lwr[17:18]))
const.lwr.late <- c(mean(pred.const.lwr[17:18]),  pred.const.lwr[18:29])

#varying group preds
vary.upr.early <- c(pred.vary.upr[1:13], mean(pred.vary.upr[13:14]))
vary.upr.mid <- c(mean(pred.vary.upr[13:14]), pred.vary.upr[14:17], mean(pred.vary.upr[17:18]))
vary.upr.late <- c(mean(pred.vary.upr[17:18]),  pred.vary.upr[18:29])

vary.fit.early <- c(pred.vary.fit[1:13], mean(pred.vary.fit[13:14]))
vary.fit.mid <- c(mean(pred.vary.fit[13:14]), pred.vary.fit[14:17], mean(pred.vary.fit[17:18]))
vary.fit.late <- c(mean(pred.vary.fit[17:18]),  pred.vary.fit[18:29])

vary.lwr.early <- c(pred.vary.lwr[1:13], mean(pred.vary.lwr[13:14]))
vary.lwr.mid <- c(mean(pred.vary.lwr[13:14]), pred.vary.lwr[14:17], mean(pred.vary.lwr[17:18]))
vary.lwr.late <- c(mean(pred.vary.lwr[17:18]),  pred.vary.lwr[18:29])


#updated to include RMSE for SE Aus:
#only preds for peak group of 2019/2020, to reflect the behavior of the first part of fig 2. 

#fig 2b, predictions and intervals.
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "SEpreds_2019_fig2b_newest.png", width = 3000, height = 3500, res = 300)

par(mfrow = c(3, 1), oma = c(3, 3.5, 2, 1), mar = c(3, 2, 2, 1))
#update prediction figure (full model)

plot(1:29, pred.base.fit, type = "l", ylim = c(-50,50), axes = FALSE, 
     lwd = 3, lty = 2, col = "forestgreen",
     ylab = "", xlab = "", xlim = c(1.95, 28.05))
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
#upper bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = base.upr.early,
             x2 = c(1:13, 13.5),
             y2 = base.fit.early,
             col = alpha("springgreen3", 0.2),
             lineCol = NA)
#different color for peak group
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = base.upr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = base.fit.mid,
             col = alpha("springgreen4", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = base.upr.late,
             x2 = c(17.5, 18:29),
             y2 = base.fit.late,
             col = alpha("springgreen3", 0.2),
             lineCol = NA)
#lines(1:29, pred.base.upr, lty = 4, lwd = 2, col = alpha("forestgreen", 0.9))
#lower bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = base.lwr.early,
             x2 = c(1:13, 13.5),
             y2 = base.fit.early,
             col = alpha("springgreen3", 0.2),
             lineCol = NA)
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = base.lwr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = base.fit.mid,
             col = alpha("springgreen4", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = base.lwr.late,
             x2 = c(17.5, 18:29),
             y2 = base.fit.late,
             col = alpha("springgreen3", 0.2),
             lineCol = NA)
#lines(1:29, pred.base.lwr, lty = 4, lwd = 2, col = alpha("forestgreen", 0.9))
lines(1:29, SE.2019.true, lty = 1, lwd = 2, col = "grey5")
abline(h=0, lty =3, col = "gray15", lwd = 2)
abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
legend("topright", 
       legend = c("Observed",
                  "Model Predictions",
                  "95% Pred. Interval"),
       lty = c(1, 2, 1), 
       lwd = c(1.75, 1.75, 10 ),
       cex = 1.75,
       col = c("grey5", 
               "forestgreen",
               alpha("springgreen3", 0.3)),
       xpd = TRUE)
title("Full model", adj = 0, cex.main = 1.65)
text(x= 3, y = -46, labels = "RMSE: 5.95", col = "gray35", cex = 1.5)
text(x=15.5, y = -46, labels = "RMSE: 3.85", col = "gray35", cex = 1.5)
text(x=20, y = -46, labels = "RMSE: 2.74", col = "gray35", cex = 1.5)
text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)


#update prediction figure (fixed model)
plot(1:29, pred.const.fit, type = "l", ylim = c(-50,50), axes = FALSE, 
     lwd = 3, lty = 2, col = "magenta3",
     ylab = "", xlab = "", xlim = c(1.95, 28.05))
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
axis(2, at = c(-50, -25,  0, 25, 50),  cex.axis = 1.45)
#upper bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = const.upr.early,
             x2 = c(1:13, 13.5),
             y2 = const.fit.early,
             col = alpha("orchid3", 0.2),
             lineCol = NA)
#different color for peak group
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = const.upr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = const.fit.mid,
             col = alpha("orchid4", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = const.upr.late,
             x2 = c(17.5, 18:29),
             y2 = const.fit.late,
             col = alpha("orchid3", 0.2),
             lineCol = NA)
#lines(1:29, pred.const.upr, lty = 4, lwd = 2, col = alpha("magenta3", 0.9))
#lower bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = const.lwr.early,
             x2 = c(1:13, 13.5),
             y2 = const.fit.early,
             col = alpha("orchid3", 0.2),
             lineCol = NA)
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = const.lwr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = const.fit.mid,
             col = alpha("orchid4", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = const.lwr.late,
             x2 = c(17.5, 18:29),
             y2 = const.fit.late,
             col = alpha("orchid3", 0.2),
             lineCol = NA)
#lines(1:29, pred.const.lwr, lty = 4, lwd = 2, col = alpha("magenta3", 0.9))
lines(1:29, SE.2019.true, lty = 1, lwd = 2, col = "grey5")
abline(h=0, lty =3, col = "gray15", lwd = 2)
abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)
legend("topright", 
       legend = c("Observed",
                  "Model Predictions",
                  "95% Pred. Interval"),
       lty = c(1, 2, 1), 
       lwd = c(1.75, 1.75, 10 ),
       cex = 1.75,
       col = c("grey5",
               "magenta3",
               alpha("orchid3", 0.3)),
       xpd = TRUE)
title("Fixed Model", adj = 0, cex.main = 1.65)

text(x= 3, y = -46, labels = "RMSE: 6.98", col = "gray35", cex = 1.5)
text(x=15.5, y = -46, labels = "RMSE: 6.82", col = "gray35", cex = 1.5)
text(x=20, y = -46, labels = "RMSE: 3.08", col = "gray35", cex = 1.5)
text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)


#update prediction figure (non-fixed model)
plot(1:29, pred.vary.fit, type = "l", ylim = c(-50,50), axes = FALSE, 
     lwd = 3, lty = 2, col = "darkorange2",
     ylab = "", xlab = "", xlim = c(1.95, 28.05))
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.45)
axis(2, at = c(-50, -25, 0, 25, 50),  cex.axis = 1.45)
#upper bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = vary.upr.early,
             x2 = c(1:13, 13.5),
             y2 = vary.fit.early,
             col = alpha("orange2", 0.2),
             lineCol = NA)
#different color for peak group
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = vary.upr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = vary.fit.mid,
             col = alpha("orange3", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = vary.upr.late,
             x2 = c(17.5, 18:29),
             y2 = vary.fit.late,
             col = alpha("orange2", 0.2),
             lineCol = NA)
#lines(1:29, pred.vary.upr, lty = 4, lwd = 2, col = alpha("darkorange2", 0.9))
#lower bound
envelopePlot(x1 = c(1:13, 13.5),
             y1 = vary.lwr.early,
             x2 = c(1:13, 13.5),
             y2 = vary.fit.early,
             col = alpha("orange2", 0.2),
             lineCol = NA)
envelopePlot(x1 = c(13.5, 14:17, 17.5),
             y1 = vary.lwr.mid,
             x2 = c(13.5, 14:17, 17.5),
             y2 = vary.fit.mid,
             col = alpha("orange3", 0.33),
             lineCol = NA)
envelopePlot(x1 = c(17.5, 18:29),
             y1 = vary.lwr.late,
             x2 = c(17.5, 18:29),
             y2 = vary.fit.late,
             col = alpha("orange2", 0.2),
             lineCol = NA)
#lines(1:29, pred.vary.lwr, lty = 4, lwd = 2, col = alpha("darkorange2", 0.9))
lines(1:29, SE.2019.true, lty = 1, lwd = 2, col = "grey5")
abline(h=0, lty =3, col = "gray15", lwd = 2)
abline(v = c(13.5, 17.5), lty = 3, col = "gray24", lwd = 1.5)

legend("topright", 
       legend = c("Observed",
                  "Model Predictions",
                  "95% Pred. Interval"),
       lty = c(1, 2, 1), 
       lwd = c(1.75, 1.75, 10 ),
       cex = 1.75,
       col = c("grey5",  
               "darkorange2",
               alpha("orange2", 0.3)),
       xpd = TRUE)
title("Non-fixed Model", adj = 0, cex.main = 1.65)

text(x= 3, y = -46, labels = "RMSE: 9.61", col = "gray35", cex = 1.5)
text(x=15.5, y = -46, labels = "RMSE: 8.15", col = "gray35", cex = 1.5)
text(x=20, y = -46, labels = "RMSE: 4.68", col = "gray35", cex = 1.5)
text(x= c(7, 15.5, 23), y = -17, labels = c("Early", "Peak", "Late"), col = "gray35", cex = 1.65)

mtext("CO Anomaly [ppb]", side = 2, outer = TRUE, padj = 0.5, cex = 1.25, line = 2)
mtext("Week", side = 1, outer = TRUE, adj = 0.5, cex = 1.25, line = 1)

dev.off()



#alternative Fig 2b
#setup cex
cex.num <- 2.25 #cex.axis = 1.45
cex.label <- 2.5 #cex.lab = 1.75
cex.subtile <- 2 #cex = 1.65
#cex.title
line.width <- 2.5 #lwd = 2


setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "SEpreds2019_peak_fig2b.png", width = 3000, height = 1750, res = 300)
#peak group only (weeks 51, 52, 1, 2)
par(mfrow = c(1, 3), oma = c(1, 1, 3.5, 1))

#full model (base)
par(mar = c(4.5, 5, 1, 2))
plot(c(0.5, 1:4, 4.5), base.fit.mid, type = "l", ylim = c(-8, 55), xlim = c(0.75, 4.25),
     axes = FALSE, 
     lwd = 3, lty = 2, col = "forestgreen",
     ylab = "CO Anomaly [ppb]", xlab = "", cex.lab = cex.label)
box()
axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = cex.num)
axis(2, at = c(-25, 0, 25, 50),  cex.axis = cex.num)
title("Peak Group 2019/2020 Wildfire Season", adj = 0.12, cex.main = 2.25, line = 0, xpd = TRUE, outer = TRUE)
#true line
lines(c(0.5, 1:4, 4.5), SE2019.mid, lty = 1, lwd = 2, col = "grey5")
#upper bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = base.upr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = base.fit.mid,
             col = alpha("springgreen3", 0.25),
             lineCol = NA)
#lower bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = base.lwr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = base.fit.mid,
             col = alpha("springgreen3", 0.25),
             lineCol = NA)
abline(h=0, lty =3, col = "gray15", lwd = 2)
text(x= 1.65, y = 54, labels = "Full Model", col = "gray25", cex = 1.85)

#fixed model (constant)
par(mar = c(4.5, 2, 1, 2))
plot(c(0.5, 1:4, 4.5), const.fit.mid, type = "l", ylim = c(-8, 55), xlim = c(0.75, 4.25),
     axes = FALSE, 
     lwd = 3, lty = 2, col = "magenta3",
     ylab = "", xlab = "Week", cex.lab = cex.label)
box()
axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = cex.num)
axis(2, at = c(-25, 0, 25, 50),  cex.axis = cex.num)
#true line
lines(c(0.5, 1:4, 4.5), SE2019.mid, lty = 1, lwd = 2, col = "grey5")
#upper bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = const.upr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = const.fit.mid,
             col = alpha("orchid3", 0.25),
             lineCol = NA)
#lower bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = const.lwr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = const.fit.mid,
             col = alpha("orchid3", 0.25),
             lineCol = NA)
abline(h=0, lty =3, col = "gray15", lwd = 2)
text(x = 1.75, y = 54, labels = "Fixed Model", col = "gray25", cex = 1.85)

#non-fixed model (varying)
par(mar = c(4.5, 2, 1, 1))
plot(c(0.5, 1:4, 4.5), vary.fit.mid, type = "l", ylim = c(-8, 55), xlim = c(0.75, 4.25),
     axes = FALSE, 
     lwd = 3, lty = 2, col = "darkorange2",
     ylab = "", xlab = "")
box()
axis(1, labels = season.weeks[14:17], at = 1:4, cex.axis = cex.num)
axis(2, at = c(-25, 0, 25, 50),  cex.axis = cex.num)
#true line
lines(c(0.5, 1:4, 4.5), SE2019.mid, lty = 1, lwd = 2, col = "grey5")
#upper bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = vary.upr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = vary.fit.mid,
             col = alpha("orange2", 0.25),
             lineCol = NA)
#lower bound
envelopePlot(x1 = c(0.5, 1:4, 4.5),
             y1 = vary.lwr.mid,
             x2 = c(0.5, 1:4, 4.5),
             y2 = vary.fit.mid,
             col = alpha("orange2", 0.25),
             lineCol = NA)
abline(h=0, lty =3, col = "gray15", lwd = 2)
text(x= 1.95, y = 54, labels = "Non-Fixed Model", col = "gray25", cex = 1.85)

#TODO: add in a legend to the right and expand the right margin

#old labels
#mtext("CO Anomaly [ppb]", side = 2, outer = TRUE, padj = 0.5, cex = 1.25, line = 1)
#mtext("Week", side = 1, outer = TRUE, adj = 0.5, cex = 1.25, line = 1)



dev.off()




#alternative Fig 2c
#TODO: include RMSE for all groups and variations (as boxplot?)
##RMSE following earlier plots
early.rmse <- data.frame(base = base.rmse.df$early, 
                         const = const.rmse.df$early,
                         vary = vary.rmse.df$early)
mid.rmse <- data.frame(base = base.rmse.df$mid, 
                       const = const.rmse.df$mid,
                       vary = vary.rmse.df$mid)
late.rmse <- data.frame(base = base.rmse.df$late, 
                        const = const.rmse.df$late,
                        vary = vary.rmse.df$late)
max.rmse <- max(early.rmse, mid.rmse, late.rmse)


#seasonal boxplots
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "SEpreds2019_rmse_fig2c.png", width = 3000, height = 1500, res = 300)

par(mar = c(2, 4.5, 2, 1))
boxplot(cbind(early.rmse, mid.rmse, late.rmse), 
        ylim = c(0, max.rmse), xlim = c(0.85, 9.15),
        col =  rep(c(alpha("forestgreen", 0.5),
                 alpha("magenta3", 0.5),
                 alpha("darkorange2", 0.5)),3), pch = 16,
        axes = FALSE, ylab = "RMSE", cex.lab = 1.15)
box()
axis(2, cex.axis = 1.15)
abline(v=c(3.5, 6.5), lty = 2, col = "gray24", lwd = 1.5)
legend("topright",
       title = "Model Variation", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = 22,
       col = "grey4",
       pt.bg = c(alpha("forestgreen", 0.65),
               alpha("magenta3", 0.65),
               alpha("darkorange2", 0.65)),
       pt.cex = 2)
text(x = c(2,5,8), y = 0, labels = c("Early", "Peak", "Late"), col = "gray20", cex = 1.3)

dev.off()



## lag-overlap plot
##SE Only

#setup
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#ETIO
SE2.coef[5:6]
SE3.coef[4:5]

SEmid.lag7 <- sapply(SE.mid - 7, function(x) ifelse(x <=0, x + 52, x)) 
SEmid.lag33 <- sapply(SE.mid - 33, function(x) ifelse(x <=0, x + 52, x)) 

SElate.lag16 <- sapply(SE.late - 16 , function(x) ifelse(x <=0, x + 52, x)) 
SElate.lag33 <- sapply(SE.late - 33 , function(x) ifelse(x <=0, x + 52, x)) 

etio.lag.min <- c(min(SElate.lag33), min(SElate.lag16), min(SEmid.lag33), min(SEmid.lag7))
etio.lag.max <- c(max(SElate.lag33), max(SElate.lag16), max(SEmid.lag33), max(SEmid.lag7))

#varying; non-fixed 
SE1.varycoef[3:4]
SE2.varycoef[5]
SE3.varycoef[7:8]

SEearly.lag2.vary <- sapply(SE.early - 2, function(x) ifelse(x <=0, x + 52, x)) 
SEearly.lag42.vary <- sapply(SE.early - 42, function(x) ifelse(x <=0, x + 52, x))

SEmid.lag8.vary <- sapply(SE.mid - 8, function(x) ifelse(x <=0, x + 52, x)) 

SElate.lag16.vary <- sapply(SE.late - 16 , function(x) ifelse(x <=0, x + 52, x)) 
SElate.lag19.vary <- sapply(SE.late - 19 , function(x) ifelse(x <=0, x + 52, x))

#WTIO
SE1.coef[3]
SE2.coef[3:4]

#varying; non-fixed 
SE2.varycoef[2]
SE3.varycoef[5:6]


SEearly.lag5 <- sapply(SE.early - 5, function(x) ifelse(x <=0, x + 52, x)) 

SEmid.lag14 <- sapply(SE.mid - 14, function(x) ifelse(x <=0, x + 52, x))
SEmid.lag46 <- sapply(SE.mid - 46, function(x) ifelse(x <=0, x + 52, x))

wtio.lag.min <- c(min(SEearly.lag5), min(SEmid.lag14), min(SEmid.lag46))
wtio.lag.max <- c(max(SEearly.lag5), max(SEmid.lag14), max(SEmid.lag46))

etio.lag <- c("Lag 7", "Lag 33", "Lag 16", "Lag 33")
wtio.lag <- c("Lag 5", "Lag 14", "Lag 46")


#coef magnitude
etio.coef <- c(SE2.coef[5:6], SE3.coef[4:5])
wtio.coef <- c(SE1.coef[3], SE2.coef[3:4])

etio.mag <- ceiling(abs(etio.coef*3))
wtio.mag <- ceiling(abs(wtio.coef*3))
wtio.mag[2] <- 2

#updated figure
setwd("~/CO_AUS/AusCOmodeling/Figures")

png(filename = "IODlag_fig3a.png", width = 3000, height = 2400, res = 300)
par(mfrow = c(2, 1), oma = c(2.5, 1, 1, 1), mar = c(2.5, 3, 1, 1))
plot(NULL, xlim = c(2.4, 64.3), ylim = c(0.60, 3.40),
     yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "", bty = "l")
#abline(h = 1:3, lty = 3, col = "gray70") #temp line guide
rect(0, 0.5, 67, 1.5, col = alpha("gray75",0.5), border = NA)
rect(0, 2.5, 67, 3.5, col = alpha("gray75",0.5), border = NA)
axis(2, at = 1:3, labels = c("Late", "Peak", "Early"), las = 1)
#axis(1, at = 1:66, labels = c(1:52, 1:14), cex.axis = 0.76)
segments(x0 = etio.lag.min, y0 = c(1,1,2,2),
         x1 = etio.lag.max, y1 = c(1,1,2,2), 
         lwd = rev(etio.mag), col = "royalblue3", lend = 1)
text(x=c(45.5, 19.5, 44.5, 27.5), y=c(2.17, 2.14, 1.12, 1.1), labels = etio.lag, col = "gray24", cex = 1)
#early
segments(x0 = min(SE.early), y0 = 3.1,
         x1 = max(SE.early), y1 = 3.1, 
         lwd = 2, lty =2, col = "gray12", lend = 1)
arrows(x0 = c(min(SE.early)+0.5, max(SE.early)-0.5), y0 = 3.1, 
       x1 = c(min(SE.early)-0.125, max(SE.early)+0.125), y1 = 3.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#peak
segments(x0 = 51, y0 = 2.1,
         x1 = 54, y1 = 2.1, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(51.5, 53.5), y0 = 2.1, 
       x1 = c(51-0.125, 54+0.125), y1 = 2.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#late
segments(x0 = 55, y0 = 1.1,
         x1 = 66, y1 = 1.1, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(55+0.5, 66-0.5), y0 = 1.1, 
       x1 = c(55-0.125, 66+0.125), y1 = 1.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
abline(v = c(9.5, 22.5, 35.5, 48.5, 61.5), lty = 2, col = "gray38")
text(x =c(4.75, 16, 29, 42, 55 ), y = 0.65,  labels = c("DJF", "MAM", "JJA", "SON", "DJF" ), col = "gray36")
text(x = 3, y = 3.25, labels = "ETIO", col = "gray24", cex = 1.25)

plot(NULL, xlim = c(2.4, 64.3), ylim = c(0.60, 3.40),
     yaxt = "n", xaxt = "n", xlab = "", ylab = "", main = "", bty = "l")
#abline(h = 1:3, lty = 3, col = "gray70") #temp line guide
rect(0, 0.5, 67, 1.5, col = alpha("gray75",0.5), border = NA)
rect(0, 2.5, 67, 3.5, col = alpha("gray75",0.5), border = NA)
axis(2, at = 1:3, labels = c("Late", "Peak", "Early"), las = 1)
#axis(1, at = 1:66, labels = c(1:52, 1:14), cex.axis = 0.76)
segments(x0 = wtio.lag.min, y0 = c(3,2,2),
         x1 = wtio.lag.max, y1 = c(3,2,2), 
         lwd = wtio.mag, col = c("firebrick1", "firebrick1", "royalblue3"), lend = 1)
text(x=c(38.5, 38.5, 6.5), y=c(2.88, 1.89, 1.85), labels = wtio.lag, col = "gray24", cex = 1)
#early
segments(x0 = min(SE.early), y0 = 3.1,
         x1 = max(SE.early), y1 = 3.1, 
         lwd = 2, lty =2, col = "gray12", lend = 1)
arrows(x0 = c(min(SE.early)+0.5, max(SE.early)-0.5), y0 = 3.1, 
       x1 = c(min(SE.early)-0.125, max(SE.early)+0.125), y1 = 3.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#peak
segments(x0 = 51, y0 = 2.1,
         x1 = 54, y1 = 2.1, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(51.5, 53.5), y0 = 2.1, 
       x1 = c(51-0.125, 54+0.125), y1 = 2.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#late
segments(x0 = 55, y0 = 1.1,
         x1 = 66, y1 = 1.1, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(55+0.5, 66-0.5), y0 = 1.1, 
       x1 = c(55-0.125, 66+0.125), y1 = 1.1, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
abline(v = c(9.5, 22.5, 35.5, 48.5, 61.5), lty = 2, col = "gray38")
text(x =c(4.75, 16, 29, 42, 55 ), y = 0.65,  labels = c("DJF", "MAM", "JJA", "SON", "DJF" ), col = "gray36")
text(x = 3, y = 3.25, labels = "WTIO", col = "gray24", cex = 1.25)
mtext("Week", side = 1, outer = TRUE, adj = 0.5, cex = 1.25, line = 1)
dev.off()


#alternate plot
##single line for each model
setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "IOD_lag2.png", width = 3000, height = 2500, res = 300)
par(mfrow = c(2, 1), oma = c(2.5, 1, 1, 1), mar = c(2, 3, 2.5, 2))

#ETIO plot
plot(NULL, xlim = c(1,66), ylim = c(0.5, 3.5),
     yaxt = "n", xaxt = "n", xlab = "Week", ylab = "", main = "", bty = "l")
axis(2, at = 1:3, labels = c("Late", "Peak", "Early"), las = 1)
axis(1, at = 1:66, labels = c(1:52, 1:14), cex.axis = 0.85)
segments(x0 = etio.lag.min, y0 = c(1,1,2,2),
         x1 = etio.lag.max, y1 = c(1,1,2,2), 
         lwd = rev(etio.mag), col = "royalblue3", lend = 1)
#early
segments(x0 = min(SE.early), y0 = 2.9,
         x1 = max(SE.early), y1 = 2.9, 
         lwd = 2, lty =2, col = "gray12", lend = 1)
arrows(x0 = c(min(SE.early)+0.5, max(SE.early)-0.5), y0 = 2.9, 
       x1 = c(min(SE.early)-0.125, max(SE.early)+0.125), y1 = 2.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#peak
segments(x0 = 51, y0 = 1.9,
         x1 = 54, y1 = 1.9, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(51.5, 53.5), y0 = 1.9, 
       x1 = c(51-0.125, 54+0.125), y1 = 1.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#late
segments(x0 = 55, y0 = 0.9,
         x1 = 66, y1 = 0.9, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(55+0.5, 66-0.5), y0 = 0.9, 
       x1 = c(55-0.125, 66+0.125), y1 = 0.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
text(x=c(45.5, 19.5, 44.5, 27.5), y=c(2.15, 2.12, 1.09, 1.09), labels = etio.lag, col = "gray24", cex = 1)
abline(h = 1:3, lty = 3, col = "gray70")
abline(v = c(9.5, 22.5, 35.5, 48.5, 61.5), lty = 2, col = "gray48")
text(x =c(4.75, 16, 29, 42, 55 ), y = 0.5,  labels = c("DJF", "MAM", "JJA", "SON", "DJF" ), col = "gray36")
text(x = 2, y = 3.5, labels = "ETIO", col = "gray36", cex = 1.25)
#TODO: add in legend (custom legend, not legend())
#legend (manual)
x0 <- 57
y_vals <- c(3.5, 3.3, 3.1, 2.9, 2.7, 2.5, 2.3)
arrows(c(x0, x0 + 3), y_vals[1], c(x0-0.1, x0 + 3.1), y_vals[1],
       length = 0.125, lty= 1, col = "gray12", lwd = 2)
segments(x0, y_vals[1], x0 + 3, y_vals[1], 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
text(x0 + 5, y_vals[1], "Response", adj = 0)
#add in pos/neg (red/blue)
segments(x0, c(y_vals[2], y_vals[3]), x0 + 3, c(y_vals[2], y_vals[3]), 
         lwd = 3.5, col = c("firebrick1", "royalblue3"), lend = 1)
text(x0 + 5, c(y_vals[2], y_vals[3]), c("Positive Coef.", "Negative Coef."), adj = 0, xpd = TRUE)
#add in coeff magnitude
segments(x0, c(y_vals[4], y_vals[5], y_vals[6], y_vals[7]), 
         x0 + 3, c(y_vals[4], y_vals[5], y_vals[6], y_vals[7]), 
         lwd = c(2,4,6,15), col = "gray48", lend = 1)
text(x0 + 5, c(y_vals[4], y_vals[5], y_vals[6], y_vals[7]), 
     c("|Coef.| < 0.67", "|Coef.| < 1.33", "|Coef.| < 2", "|Coef.| < 5"), adj = 0, xpd = TRUE)

#legend("topright", legend = c("Positive", "Negative"),
#       lwd = 2, col = c("firebrick1", "royalblue3"), 
#       title = "Coefficient")
#legend("right", legend = c("2", "4", "6", "15"),
#       lwd = c(2,4,6,15), col = "gray36", 
#       title = "Magnitude ")


#WTIO plot
plot(NULL, xlim = c(1,66), ylim = c(0.5, 3.5),
     yaxt = "n", xaxt = "n", xlab = "Week", ylab = "", main = "", bty = "l")
axis(2, at = 1:3, labels = c("Late", "Peak", "Early"), las = 1)
axis(1, at = 1:66, labels = c(1:52, 1:14), cex.axis = 0.85)
segments(x0 = wtio.lag.min, y0 = c(3,2,2),
         x1 = wtio.lag.max, y1 = c(3,2,2), 
         lwd = wtio.mag, col = c("firebrick1", "firebrick1", "royalblue3"), lend = 1)
#early
segments(x0 = min(SE.early), y0 = 2.9,
         x1 = max(SE.early), y1 = 2.9, 
         lwd = 2, lty =2, col = "gray12", lend = 1)
arrows(x0 = c(min(SE.early)+0.5, max(SE.early)-0.5), y0 = 2.9, 
       x1 = c(min(SE.early)-0.125, max(SE.early)+0.125), y1 = 2.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#peak
segments(x0 = 51, y0 = 1.9,
         x1 = 54, y1 = 1.9, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(51.5, 53.5), y0 = 1.9, 
       x1 = c(51-0.125, 54+0.125), y1 = 1.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
#late
segments(x0 = 55, y0 = 0.9,
         x1 = 66, y1 = 0.9, 
         lwd = 2, lty = 2, col = "gray12", lend = 1)
arrows(x0 = c(55+0.5, 66-0.5), y0 = 0.9, 
       x1 = c(55-0.125, 66+0.125), y1 = 0.9, 
       length = 0.125, lwd = 2, col = "gray12", lend = 1)
text(x=c(38.5, 38.5, 6.5), y=c(3.09, 2.09, 2.09), labels = wtio.lag, col = "gray24", cex = 1)
abline(h = 1:3, lty = 3, col = "gray70")
abline(v = c(9.5, 22.5, 35.5, 48.5, 61.5), lty = 2, col = "gray48")
text(x =c(4.75, 16, 29, 42, 55 ), y = 0.5,  labels = c("DJF", "MAM", "JJA", "SON", "DJF" ), col = "gray36")
text(x = 2, y = 3.5, labels = "WTIO", col = "gray36", cex = 1.25)
mtext("Week", side = 1, outer = TRUE, adj = 0.5)

dev.off()

