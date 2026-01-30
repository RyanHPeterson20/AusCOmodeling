## work on... (see notes)

#1. Get model info (terms and coefficients) for the "interesting" seasons 
#2....


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(grid)) #table/grid setup and lines between plots
suppressMessages( library(gridExtra))
suppressMessages( library(lubridate))
suppressMessages( library(MASS))
suppressMessages(library(colorspace))
suppressMessages(library(RColorBrewer))
suppressMessages( library(cmocean)) #ocean colors
suppressMessages( library(rgl))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
source("Functions/coef_int_functions.R")
source("Functions/modeling_functions.R")

#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#get single season models for peak 2019/2020 with additional withheld:
## 2001/2002, 2010/2011, and 2011/2012 (along with 2005/2006 and 2016/2017)

#peak all-data models
SE2.lm <- SEmodels[[2]] 


#explore the models
for (i in 1:20) {
  print(seasons[i])
  print(summary(SE2.lm.2019[[i]]))
}


#model for predicting 2019/2020 (generalize as needed)
SE.lm.2019 <- SErefit.wo.years$SE.vary.lm$`2019-2020`
#get all peak models
SE2.lm.2019 <- lapply(SE.lm.2019, function(z) z[[2]])

#get only 2019/2020 withheld
SE.lm.wo2019 <- SE2.lm.2019[[19]]


## ---- Coef/Interaction Figures ---- ##
## setup
#SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
#SE3.coef <- coef(SE3.lm)


#peak models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    #const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(SE2.lm.2019[[i]])  
  )
  
  png(filename = paste0("SEcoefs_2019peak_withheld_", season.years[i], ".png"), width = 3400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak 2019/2020 Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="darkorange2", vary="royalblue3"))
  dev.off()
}


#get tables
card1 <- lm_card_grob(SE2.lm, border = "forestgreen", fill = alpha("springgreen4", 0.2))
card2 <- lm_card_grob(SE.lm.wo2019, border = "darkorange2", fill = alpha("orange3", 0.2))

grid.arrange(card1, card2, ncol = 2)


#peak plot
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  png(filename = paste0("new_model_tables_", season.years[i], ".png"), width = 3500, height = 1500, res = 275)
  
  card2.vary.wo2019 <- lm_card_grob(SE2.lm.2019[[i]], border = "royalblue3", fill = alpha("steelblue4", 0.2))

  
  grid.arrange(card1, card2, card2.vary.wo2019, ncol = 3)
  
  dev.off()
}

#TODO: isolate the weeks that each model looks at when "training" so that we can look at everything.
#note, get each year (52 weeks preceding a group) at a time then combine them later.




#time series plots for interesting years 2001, 2010, & 2011 (and 2005, 2015)

#predictor colors
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"


#for 2001/2002 peak season
lag1.date <- pred.df[pred.df$week == 1 & pred.df$year == 2002, ]$date
lag.date <- rev(pred.df[pred.df$year == 2001, ]$date)
lag52.date <- rev(pred.df[pred.df$week %in% 51:52 & pred.df$year == 2000, ]$date)

dates.2001 <- c(lag1.date, lag.date, lag52.date)

length(dates.2001)
length(SEpreds.peak.nino[1,])
peak.nino2001.df <- data.frame(nino = rev(as.numeric(SEpreds.peak.nino[1,])), date = rev(dates.2001))
peak.wtio2001.df <- data.frame(wtio = rev(as.numeric(SEpreds.peak.wtio[1,])), date = rev(dates.2001))


#ts plot setup
pred.time <- peak.nino2001.df$date
#pred.week <- pred.raw$week
pred.time.range <- range(pred.time)

#yearly ticks
month(pred.time.range[1])
month(pred.time.range[2])

x.ticks.pred <- seq(month(pred.time.range[1]), month(pred.time.range[2]), by = -1)
x.ticks.pred <- ymd(paste0("2001-", x.ticks.pred, "-01"))
x.pred.reduced <- x.ticks.pred #[1:20]

time.pred.plot <- as.Date(pred.time)

#TODO: setup y-axis



nino.2001 <- peak.nino2001.df$nino

#envelope plot setup
#nino:
over.nino <- nino.2001 >= 0
nino.top <- nino.2001
nino.top[!over.nino] <- 0
nino.bot <- nino.2001
nino.bot[over.nino] <-0

#TODO: fix the envelope plot

plot(time.pred.plot, nino.2001, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     #xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
    bty = "n", cex.lab = 1,  xpd = NA)
#axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
#     col = NA, line = 0,
#     col.ticks = "black", col.axis = "black", las =1)
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
text(x = x.pred.reduced,
     y = range(nino.2001)[1],
     labels = paste0(month(x.pred.reduced), "-", year(x.pred.reduced)),
     cex = 1, col = "black", xpd = NA)




wtio.2001 <- peak.wtio2001.df$wtio

#envelope plot setup
#nino:
over.wtio <- wtio.2001 >= 0
wtio.top <- wtio.2001
wtio.top[!over.wtio] <- 0
wtio.bot <- wtio.2001
wtio.bot[over.wtio] <-0

#TODO: fix the envelope plot

plot(time.pred.plot, wtio.2001, type = "l", col = "black", lwd = 2,
     xaxt = "n", xlab = "",
     yaxt = "n", ylab = "Anomaly [W/m^2]", col.lab = "black",
     #xlim = c(as.Date(pred.time.range[1]) + months(7), as.Date(pred.time.range[2]) - months(7)),
     bty = "n", cex.lab = 1,  xpd = NA)
#axis(side = 2, at = y.tick.lab, cex.axis = 2.25, 
#     col = NA, line = 0,
#     col.ticks = "black", col.axis = "black", las =1)
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
text(x = x.pred.reduced,
     y = range(wtio.2001)[1],
     labels = paste0(month(x.pred.reduced), "-", year(x.pred.reduced)),
     cex = 1, col = "black", xpd = NA)



