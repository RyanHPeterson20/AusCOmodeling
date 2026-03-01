#new work on RMSE differences, etc


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(Metrics)) #measurement metrics
suppressMessages( library(cmocean)) #ocean colors


#data
#import models and data
#import data and models
setwd("~/CO_AUS/AusCOmodeling")
load("Data/matrixdata.rda") #data as matrix
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/modeldata.rda") #resp/pred data
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#load functions
source("Functions/modeling_functions.R")


#updated heatmap with rwb
heatmap_fields_cols <- function(z, x = NULL, y = NULL,
                                zlim = NULL,            # if NULL, uses symmetric max(abs(z))
                                nlevels =49,
                                cols = NULL,
                                main = NULL,
                                xlab = NULL,
                                ylab = NULL,
                                legend.lab = NULL,
                                xlas = 2,               # 2 = vertical tick labels
                                ylas = 1,
                                draw_zero_contour = FALSE,
                                x_n = 20, y_n = 20,
                                ...) {
  
  if (is.null(x)) x <- seq_len(ncol(z))
  if (is.null(y)) y <- seq_len(nrow(z))
  
  # symmetric limits around zero
  if (is.null(zlim)) {
    m <- max(abs(z), na.rm = TRUE)
    zlim <- c(-m, m)
  } else {
    # enforce symmetry if user passes one number
    if (length(zlim) == 1) zlim <- c(-abs(zlim), abs(zlim))
  }
  
  
  # choose tick positions (indices)
  xi <- unique(round(seq(1, ncol(z), length.out = x_n)))
  yi <- unique(round(seq(1, nrow(z), length.out = y_n)))
  
  x_at <- x[xi]
  y_at <- y[yi]
  
  x_lab <- rev(colnames(z)[xi])
  y_lab <- rev(rownames(z)[yi])
  
  # evenly spaced breaks, zero centered
  breaks <- seq(zlim[1], zlim[2], length.out = nlevels + 1)
  
  # Red -> White -> Blue, with white at 0
  # We explicitly build a diverging palette
  #cols <- colorRampPalette(c("blue3", "white", "red3"))(nlevels)
  
  # margins: leave room for vertical x labels
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(6.5, 4.5, 3, 5))  # bottom, left, top, right
  
  image.plot(x, y, z,
             col = cols,
             breaks = breaks,
             zlim = zlim,
             xaxt = "n", yaxt = "n",
             main = main,
             xlab = "", ylab = "",
             legend.lab = legend.lab,
             ...)
  
  # custom axes so we can rotate labels
  axis(1, at = x_at, labels = x_lab, las = xlas)
  axis(2, at = y_at, labels = y_lab, las = ylas)
  
  if (!is.null(xlab)) mtext(xlab, side = 1, line = 4.5)
  if (!is.null(ylab)) mtext(ylab, side = 2, line = 3)
  
  # optional: emphasize zero line on the map
  if (isTRUE(draw_zero_contour)) {
    contour(x, y, z, levels = 0, add = TRUE, lwd = 1.2)
  }
  
  invisible(NULL)
}



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

SE.rmse <- SEvalid$rmse
SE.rmse.wo <- SErefit.wo.years$rmse


i <- 19 #2019/2020
rmse.peak <- SE.rmse[[i]][2,]
rmse.peak.wo2019 <- lapply(SE.rmse.wo[[i]], function(x)  x[2,])

rmse.wo2019 <- as.numeric(rmse.peak.wo2019)
seasons[order(rmse.wo2019)]
rmse.wo2019[order(rmse.wo2019)]

ordered.2019.rmse <- data.frame(seasons[order(rmse.wo2019)], rmse.wo2019[order(rmse.wo2019)])


i <- 15 #2015/2016
rmse.peak <- SE.rmse[[i]][2,]
rmse.peak.wo2015 <- lapply(SE.rmse.wo[[i]], function(x)  x[2,])

rmse.wo2015 <- as.numeric(rmse.peak.wo2015)
seasons[order(rmse.wo2015)]
rmse.wo2015[order(rmse.wo2015)]


#updated RMSE (for single withheld figures)
base.rmse <- lapply(SE.rmse, function(x)  x[,1]) #get base pred rmse

#get model data

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)

#extracting a single year (validation data)
SE.pred.valid <- list()
SE.resp.valid <- list()
for (k in 1:length(seasons)) {
  #SE Aus
  SE.pred.valid[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = c(k))
  SE.resp.valid[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = c(k)) 
}


#get linear models (for varying (single withheld))
SE.rmse.single <- NULL
SE.predict.single <- NULL
 #withheld year (model year)
for (i in 1:20) {
    
  
  temp.lm.list <- SErefit.new[[3]][[i]]
  #summary(temp.lm.list[[1]]) #early
  #summary(temp.lm.list[[2]]) #peak
  #summary(temp.lm.list[[3]]) #late
  
  
  SErmse.yearly <- matrix(NA, ncol = 3)
  colnames(SErmse.yearly) <- c("early", "peak", "late")
  #SEpredict.yearly <- matrix(NA, ncol = 3)
  #colnames(SEpredict.yearly) <- c("early", "peak", "late")
  
  SEpredict.yearly <- NULL
  for (k in 1:20) {  #prediction year
    
    #data w/ season (test/validation)
    valid.resp <- SE.resp.valid[[k]]
    valid.pred <- SE.pred.valid[[k]] 
    
    #by groups
    #early
    y.valid.early <- as.numeric(valid.resp$early)
    X.valid.early <- valid.pred$early[ ,c(1:52, 105:364)]
    
    pred.early <- predict(temp.lm.list[[1]], X.valid.early, se.fit = TRUE)
    
    rmse.early <- rmse(y.valid.early, pred.early$fit)
    
    
    #peak
    y.valid.mid <- as.numeric(valid.resp$mid)
    X.valid.mid <- valid.pred$mid[ ,c(1:52, 105:364)]
    
    pred.mid <- predict(temp.lm.list[[2]], X.valid.mid, se.fit = TRUE)
    
    rmse.mid <- rmse(y.valid.mid, pred.mid$fit)
    
    
    #late
    y.valid.late <- as.numeric(valid.resp$late)
    X.valid.late <- valid.pred$late[ ,c(1:52, 105:364)]
    
    pred.late <- predict(temp.lm.list[[3]], X.valid.late, se.fit = TRUE)
    
    rmse.late <- rmse(y.valid.late, pred.late$fit)
    
    SEpredict.yearly[[seasons[k]]] <- list(early = pred.early$fit, peak = pred.mid$fit, late = pred.late$fit)
    SErmse.yearly <- rbind(SErmse.yearly, cbind(rmse.early, rmse.mid, rmse.late))
  }
  SE.rmse.single[[seasons[i]]] <- as.data.frame(SErmse.yearly[-1, ])
  SE.predict.single[[seasons[i]]] <- SEpredict.yearly
} 


#plot rmse figures (rmse diff)
rmse.mat.early <- matrix(NA, nrow = 20)
rmse.mat.peak <- matrix(NA, nrow = 20)
rmse.mat.late <- matrix(NA, nrow = 20)
for (j in 1:20) {
  
  rmse.mat.early <- cbind(rmse.mat.early, SE.rmse.single[[j]]$early)
  rmse.mat.peak <- cbind(rmse.mat.peak, SE.rmse.single[[j]]$peak)
  rmse.mat.late <- cbind(rmse.mat.late, SE.rmse.single[[j]]$late)
  
}

rmse.mat.early <- rmse.mat.early[,-1]
rmse.mat.peak <- rmse.mat.peak[,-1]
rmse.mat.late <- rmse.mat.late[,-1]

rownames(rmse.mat.early) <- seasons
colnames(rmse.mat.early) <- seasons
rownames(rmse.mat.peak) <- seasons
colnames(rmse.mat.peak) <- seasons
rownames(rmse.mat.late) <- seasons
colnames(rmse.mat.late) <- seasons

z.early.single <- t(rmse.mat.early[20:1, ])
z.peak.single <- t(rmse.mat.peak[20:1, ])
z.late.single <- t(rmse.mat.late[20:1, ])



setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_early_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.early.single, zlim = c(0, max(z.early.single)),
                    cols = cmocean("deep")(49),
                    main = " Early Fire-Season: RMSE Withheld-Season" )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_peak_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.peak.single, zlim =  c(0, max(z.peak.single)),
                    cols = cmocean("deep")(49),
                    main = "Peak Fire-Season: RMSE Withheld-Season" )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season ",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_late_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.late.single, zlim =  c(0, max(z.late.single)),
                    cols = cmocean("deep")(49),
                    main = "Late Fire-Season: RMSE Withheld-Season" )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)
dev.off()



#all data RMSE
base.rmse.early <- matrix(rep(as.numeric(sapply(base.rmse, function(x) x[1])), 20), ncol = 20)
base.rmse.peak <- matrix(rep(as.numeric(sapply(base.rmse, function(x) x[2])), 20), ncol = 20)
base.rmse.late <- matrix(rep(as.numeric(sapply(base.rmse, function(x) x[3])), 20), ncol = 20)

rmse.diff.early <- rmse.mat.early - base.rmse.early
rmse.diff.peak <- rmse.mat.peak - base.rmse.peak
rmse.diff.late <- rmse.mat.late - base.rmse.late

rownames(rmse.diff.early) <- seasons
colnames(rmse.diff.early) <- seasons
rownames(rmse.diff.peak) <- seasons
colnames(rmse.diff.peak) <- seasons
rownames(rmse.diff.late) <- seasons
colnames(rmse.diff.late) <- seasons


z.early <- t(rmse.diff.early[20:1, ])
z.peak <- t(rmse.diff.peak[20:1, ])
z.late <- t(rmse.diff.late[20:1, ])

#TODO: update
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_early_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.early, cols = cmocean("balance")(49),
                    main = expression(" Early Fire-Season: "~Delta*"RMSE (Withheld-Season-All-data)") )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_peak_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.peak, cols = cmocean("balance")(49),
                    main = expression(" Peak Fire-Season: "~Delta*"RMSE (Withheld-Season-All-data)") )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_late_single.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.late, cols = cmocean("balance")(49),
                    main = expression(" Late Fire-Season: "~Delta*"RMSE (Withheld-Season-All-data)") )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)
dev.off()



#TODO: double withhold, update for 2019/2020, 2006/2007, and 2015/2016


SE.rmse.early <- matrix(NA, ncol = 20)
SE.rmse.peak <- matrix(NA, ncol = 20)
SE.rmse.late <- matrix(NA, ncol = 20)
for (j in 1:20) {
  
  SE.rmse.early <- rbind(SE.rmse.early,  sapply(SE.rmse.wo[[j]], function(x) x[1,]))
  SE.rmse.peak <- rbind(SE.rmse.peak, sapply(SE.rmse.wo[[j]], function(x) x[2,]))
  SE.rmse.late <- rbind(SE.rmse.late, sapply(SE.rmse.wo[[j]], function(x) x[3,]))
  
}

SE.rmse.early <- SE.rmse.early[-1,]
SE.rmse.peak <- SE.rmse.peak[-1,]
SE.rmse.late <- SE.rmse.late[-1,]

rownames(SE.rmse.early) <- seasons
colnames(SE.rmse.early) <- seasons
rownames(SE.rmse.peak) <- seasons
colnames(SE.rmse.peak) <- seasons
rownames(SE.rmse.late) <- seasons
colnames(SE.rmse.late) <- seasons

z.early.dbl <- t(SE.rmse.early[20:1, ])
z.peak.dbl <- t(SE.rmse.peak[20:1, ])
z.late.dbl <- t(SE.rmse.late[20:1, ])



setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_early_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.early.dbl,zlim = c(0, max(z.early.dbl)),
                    cols = cmocean("deep")(49),
                    main = " Early Fire-Season: RMSE Double Withheld-Season" )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_peak_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.peak.dbl,zlim =  c(0, max(z.peak.dbl)),
                    cols = cmocean("deep")(49),
                    main = "Peak Fire-Season: RMSE Double Withheld-Season" )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmse_late_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.late.dbl,zlim =  c(0, max(z.late.dbl)),
                    cols = cmocean("deep")(49),
                    main = "Late Fire-Season: RMSE Double Withheld-Season" )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()




SE.rmse.diff.early <- SE.rmse.early - base.rmse.early
SE.rmse.diff.peak <- SE.rmse.peak - base.rmse.peak
SE.rmse.diff.late <- SE.rmse.late - base.rmse.late

rownames(SE.rmse.diff.early) <- seasons
colnames(SE.rmse.diff.early) <- seasons
rownames(SE.rmse.diff.peak) <- seasons
colnames(SE.rmse.diff.peak) <- seasons
rownames(SE.rmse.diff.late) <- seasons
colnames(SE.rmse.diff.late) <- seasons

z.early.diff <- t(SE.rmse.diff.early[20:1, ])
z.peak.diff <- t(SE.rmse.diff.peak[20:1, ])
z.late.diff <- t(SE.rmse.diff.late[20:1, ])

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_early_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.early.diff, cols = cmocean("balance")(49),
                    main = expression(" Early Fire-Season: "~Delta*"RMSE (Double Withheld-Season - All-data)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_peak_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.peak.diff, cols = cmocean("balance")(49),
                    main = expression(" Peak Fire-Season: "~Delta*"RMSE (Double Withheld-Season - All-data)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_late_double.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.late.diff, cols = cmocean("balance")(49),
                    main = expression(" Late Fire-Season: "~Delta*"RMSE (Double Withheld-Season - All-data)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()


diag(rmse.mat.peak)
diag(SE.rmse.peak)

test.for.zero(diag(rmse.mat.peak), diag(SE.rmse.peak))

SE.rmse.diff2.early <- SE.rmse.early - diag(SE.rmse.early)
SE.rmse.diff2.peak <- SE.rmse.peak - diag(SE.rmse.peak)
SE.rmse.diff2.late <- SE.rmse.late - diag(SE.rmse.late)

rownames(SE.rmse.diff2.early) <- seasons
colnames(SE.rmse.diff2.early) <- seasons
rownames(SE.rmse.diff2.peak) <- seasons
colnames(SE.rmse.diff2.peak) <- seasons
rownames(SE.rmse.diff2.late) <- seasons
colnames(SE.rmse.diff2.late) <- seasons


z.early.diff2 <- t(SE.rmse.diff2.early[20:1, ])
z.peak.diff2 <- t(SE.rmse.diff2.peak[20:1, ])
z.late.diff2 <- t(SE.rmse.diff2.late[20:1, ])

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_early_double2.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.early.diff2, cols = cmocean("balance")(49),
                    main = expression(" Early Fire-Season: "~Delta*"RMSE (Double Withheld-Season - Single Withheld-Season)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_peak_double2.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.peak.diff2, cols = cmocean("balance")(49),
                    main = expression(" Peak Fire-Season: "~Delta*"RMSE (Double Withheld-Season - Single Withheld-Season)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "rmsediff_late_double2.png", width = 3500, height = 3000, res = 300)
par(oma = c(4, 4, 2, 3))  
heatmap_fields_cols(z = z.late.diff2, cols = cmocean("balance")(49),
                    main = expression(" Late Fire-Season: "~Delta*"RMSE (Double Withheld-Season - Single Withheld-Season)") )
mtext("Second Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season (& First Withheld)",  side=2, line=6.0, cex = 1.25)
dev.off()





#compare pred lines
#(we need to show how far the 2019/2020 season is)
#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)
SEresp.peak <- SEresp.mat[ ,14:17]
SEresp.peak.wide <- SEresp.mat[ ,13:18]

#actual data (max)
which.max(SEresp.peak[-19, ])
max(SEresp.peak[-19, ])

#get predictions
preds.2019 <- lapply(SE.predict.single, function(x) x[[19]])

#get double withheld preds
preds.wo.2019 <- lapply(SErefit.wo.years$preds[["2019-2020"]], function(x) x$vary.fit)

#`best`
preds.peak.wo2019 <- preds.wo.2019$`2019-2020`[14:17]
preds.peak.wo2001 <- preds.wo.2019$`2001-2002`[14:17]
preds.peak.wo2016 <- preds.wo.2019$`2016-2017`[14:17]
preds.peak.wo2010 <- preds.wo.2019$`2010-2011`[14:17]
preds.peak.wo2011 <- preds.wo.2019$`2011-2012`[14:17]
preds.peak.wo2005 <- preds.wo.2019$`2005-2006`[14:17]


#`worst`
preds.peak.wo2006 <- preds.wo.2019$`2006-2007`[14:17]
preds.peak.wo2018 <- preds.wo.2019$`2018-2019`[14:17]
preds.peak.wo2004 <- preds.wo.2019$`2004-2005`[14:17]
preds.peak.wo2003 <- preds.wo.2019$`2003-2004`[14:17]
preds.peak.wo2013 <- preds.wo.2019$`2013-2014`[14:17]

#plot comparisons
ylim.resp <- c(-13, 20)
y.resp.lab.alt <- c(-12, -6, 0, 6, 12, 18)



setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_comp.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "l", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.alt, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons", adj = 0, cex.main = 2)

#other season lines
#lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
#lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016

#pred lintes
lines(1:4, preds.peak.wo2019, lwd = 2, col = "darkorange3" )
lines(1:4, preds.peak.wo2001, lwd = 2, lty = 2, col = "royalblue3")
lines(1:4, preds.peak.wo2016, lwd = 2, lty = 3, col = "royalblue4" )
lines(1:4, preds.peak.wo2010, lwd = 2, lty = 4, col = "magenta4" )
#lines(1:4, preds.peak.wo2011, lwd = 2, lty = 5 )
#lines(1:4, preds.peak.wo2005, lwd = 2, lty = 6 )

legend("topright", inset=c(-0.26,0), cex = 1.5,
       title = "2019/2020 Preds.",
       legend=c("w/o 2019/2020","& w/o 2001/2002",
                "& w/o 2016/2017", "& w/o 2010/2011"),
       col = c("darkorange3","royalblue3","royalblue3","royalblue4"),
       lty=1:4, lwd=2, xpd=NA)

legend("topright", inset=c(-0.22, 0.3), cex = 1.5,
       title = "Other Season Data",
       legend=c("2005/2006", "2006/2007","2015/2016"),
       col = "firebrick",
       lty=c(4,1,5), lwd=2, xpd=NA)
dev.off()


#plot comparisons
ylim.resp.2019 <- c(-13, 50)
y.resp.lab.2019 <- c(-12,  0, 12, 24, 36, 48)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_comp_2019.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "l", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp.2019,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.2019, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons", adj = 0, cex.main = 2)

lines(1:4, SEresp.peak[19,], col = "magenta3", lwd = 2.5, lty = 1 )

#other season lines
#lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
#lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016

#pred lintes
lines(1:4, preds.peak.wo2019, lwd = 2, col = "darkorange3" )
lines(1:4, preds.peak.wo2001, lwd = 2, lty = 2, col = "royalblue3")
lines(1:4, preds.peak.wo2016, lwd = 2, lty = 3, col = "royalblue4" )
lines(1:4, preds.peak.wo2010, lwd = 2, lty = 4, col = "magenta4" )
#lines(1:4, preds.peak.wo2011, lwd = 2, lty = 5 )
#lines(1:4, preds.peak.wo2005, lwd = 2, lty = 6 )

legend("topright", inset=c(-0.26,0), cex = 1.5,
       title = "2019/2020 Predictions",
       legend=c("w/o 2019/2020","& w/o 2001/2002",
                "& w/o 2016/2017", "& w/o 2010/2011"),
       col = c("darkorange3","royalblue3","royalblue3","royalblue4"),
       lty=1:4, lwd=2, xpd=NA)

legend("topright", inset=c(-0.22, 0.3), cex = 1.5,
       title = "Peak Season Data",
       legend=c("2019/2020", "2005/2006", "2006/2007","2015/2016"),
       col = c("magenta4", rep("firebrick", 3)),
       lty=c(1, 4,1,5), lwd=2, xpd=NA)
dev.off()





#plot comparisons
ylim.resp.2019 <- c(-13, 50)
y.resp.lab.2019 <- c(-12,  0, 12, 24, 36, 48)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_pred_2019.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "n", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp.2019,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.2019, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons (Top 5)", adj = 0, cex.main = 2)

lines(1:4, SEresp.peak[19,], col = "magenta3", lwd = 2.5, lty = 1 )

#other season lines
#lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
#lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
#lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
#lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016

#pred lintes
lines(1:4, preds.peak.wo2019, lwd = 2, col = "darkorange3" )
lines(1:4, preds.peak.wo2001, lwd = 2, lty = 2, col = "royalblue3")
lines(1:4, preds.peak.wo2016, lwd = 2, lty = 3, col = "royalblue4" )
lines(1:4, preds.peak.wo2010, lwd = 2, lty = 4, col = "magenta4" )
lines(1:4, preds.peak.wo2011, lwd = 2, lty = 3, col = "magenta4"  ) 
lines(1:4, preds.peak.wo2005, lwd = 2, lty = 2, col = "cyan4"  )

legend("topright", inset=c(-0.28,0), cex = 1.5,
       title = "2019/2020 Predictions",
       legend=c("w/o 2019/2020","& w/o 2001/2002",
                "& w/o 2016/2017", "& w/o 2010/2011", 
                "& w/o 2011/2012", "& w/o 2005/2006"),
       col = c("darkorange3","royalblue3","royalblue4", "magenta4", "magenta4", "cyan4"),
       lty=c(1:4, 3,2), lwd=2, xpd=NA)

legend("topright", inset=c(-0.21, 0.34), cex = 1.5,
       title = "Peak Season Data",
       legend=c("2019/2020"),
       col = c("magenta3"),
       lty=c(1), lwd=2, xpd=NA)
dev.off()





#plot comparisons
ylim.resp.2019 <- c(-13, 50)
y.resp.lab.2019 <- c(-12,  0, 12, 24, 36, 48)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_pred2_2019.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "n", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp.2019,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.2019, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons (Bottom 5)", adj = 0, cex.main = 2)

lines(1:4, SEresp.peak[19,], col = "magenta3", lwd = 2.5, lty = 1 )

#other season lines
#lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
#lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
#lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
#lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016

#pred lintes
lines(1:4, preds.peak.wo2019, lwd = 2, col = "darkorange3" )
lines(1:4, preds.peak.wo2006, lwd = 2, lty = 2, col = "coral3")
lines(1:4, preds.peak.wo2018, lwd = 2, lty = 3, col = "coral4" )
lines(1:4, preds.peak.wo2004, lwd = 2, lty = 4, col = "chartreuse3" )
lines(1:4, preds.peak.wo2003, lwd = 2, lty = 3, col = "chartreuse3"  ) 
lines(1:4, preds.peak.wo2013, lwd = 2, lty = 2, col = "midnightblue"  )

legend("topright", inset=c(-0.28,0), cex = 1.5,
       title = "2019/2020 Predictions",
       legend=c("w/o 2019/2020","& w/o 2006/2007",
                "& w/o 2018/2019", "& w/o 2004/2005", 
                "& w/o 2003/2004", "& w/o 2013/2014"),
       col = c("darkorange3","coral3","coral4", "chartreuse3", "chartreuse3", "midnightblue"),
       lty=c(1:4, 3,2), lwd=2, xpd=NA)

legend("topright", inset=c(-0.21, 0.34), cex = 1.5,
       title = "Peak Season Data",
       legend=c("2019/2020"),
       col = c("magenta3"),
       lty=c(1), lwd=2, xpd=NA)
dev.off()



#plot comparisons
ylim.resp.2019 <- c(-13, 50)
y.resp.lab.2019 <- c(-12,  0, 12, 24, 36, 48)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_w2019.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "l", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp.2019,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.2019, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons", adj = 0, cex.main = 2)

lines(1:4, SEresp.peak[19,], col = "magenta3", lwd = 2.5, lty = 1 )

#other season lines
lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016


legend("topright", inset=c(-0.22, 0.3), cex = 1.5,
       title = "Peak Season Data",
       legend=c("2019/2020", "2002/2003", "2003/2004",  "2005/2006", "2006/2007","2015/2016"),
       col = c("magenta4", rep("firebrick", 5)),
       lty=c(1, 2,3,4,1,5), lwd=2, xpd=NA)
dev.off()



#plot comparisons
ylim.resp.2019 <- c(-13, 50)
y.resp.lab.2019 <- c(-12,  0, 12, 24, 36, 48)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = "peak_resp_comp_2019_alt.png", width = 3600, height = 2400, res = 275)
par(mar = c(5, 5, 4, 14))
plot(1:4, SEresp.peak[6,], type = "l", col = "firebrick", lwd = 2, #2006/2007
     xaxt = "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp.2019,  bty = "n",
     cex.lab = 2, xpd = NA)
axis(side = 2, at = y.resp.lab.2019, cex.axis = 2,
     col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2), cex.axis = 2)
abline(h =0, lty = 2, lwd = 2, col = "grey35")
#abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)
title("Peak Season Comparisons", adj = 0, cex.main = 2)

lines(1:4, SEresp.peak[19,], col = "magenta3", lwd = 2.5, lty = 1 )

#other season lines
#lines(1:4, SEresp.peak[2,], col = "firebrick", lwd = 2, lty = 2 ) #2002/2003
#lines(1:4, SEresp.peak[3,], col = "firebrick", lwd = 2, lty = 3 ) #2003/2004
lines(1:4, SEresp.peak[5,], col = "firebrick", lwd = 2, lty = 4 ) #2005/2006
lines(1:4, SEresp.peak[15,], col = "firebrick", lwd = 2, lty = 5 ) #2015/2016



legend("topright", inset=c(-0.22, 0.3), cex = 1.5,
       title = "Peak Season Data",
       legend=c("2019/2020", "2005/2006", "2006/2007","2015/2016"),
       col = c("magenta4", rep("firebrick", 3)),
       lty=c(1, 4,1,5), lwd=2, xpd=NA)
dev.off()



#single year predictions
# from: SE.predict.single 
#outer (first list) is the withheld year
#inner (second list) is the prediction year

#for 2002/03
SE.preds.2002 <- lapply(SE.predict.single, function(x) x[[2]])

SEpreds2002.wo2019 <- c(SE.preds.2002$`2019-2020`$early, SE.preds.2002$`2019-2020`$peak, SE.preds.2002$`2019-2020`$late)
SEpreds2002.wo2002 <- c(SE.preds.2002$`2002-2003`$early, SE.preds.2002$`2002-2003`$peak, SE.preds.2002$`2002-2003`$late)
SEresp.2002 <- c(SE.resp.valid$`2002-2003`$early, SE.resp.valid$`2002-2003`$mid, SE.resp.valid$`2002-2003`$late)

pred.range <- range(SEresp.2002, SEpreds2002.wo2002, SEpreds2002.wo2019)

plot(1:29, SEresp.2002, type = "l", ylim = pred.range,  main = "2002/2003")
lines(1:29, SEpreds.wo2002, col = "firebrick", lty = 2)
lines(1:29, SEpreds2002.wo2019, col = "firebrick4", lty = 4)
abline(h = 0, lty = 3, col = "grey30")
abline(v = c(13.5, 17.5), lty = 2, col = "grey30")




#for 2003/04
SE.preds.2003 <- lapply(SE.predict.single, function(x) x[[3]])

SEpreds2003.wo2019 <- c(SE.preds.2003$`2019-2020`$early, SE.preds.2003$`2019-2020`$peak, SE.preds.2003$`2019-2020`$late)
SEpreds2003.wo2003 <- c(SE.preds.2003$`2003-2004`$early, SE.preds.2003$`2003-2004`$peak, SE.preds.2003$`2003-2004`$late)
SEresp.2003 <- c(SE.resp.valid$`2003-2004`$early, SE.resp.valid$`2003-2004`$mid, SE.resp.valid$`2003-2004`$late)

pred.range <- range(SEresp.2003, SEpreds2003.wo2003, SEpreds2003.wo2019)

plot(1:29, SEresp.2003, type = "l", ylim = pred.range, main = "2003/2004")
lines(1:29, SEpreds2003.wo2003, col = "firebrick", lty = 2)
lines(1:29, SEpreds2003.wo2019, col = "firebrick4", lty = 4)
abline(h = 0, lty = 3, col = "grey30")
abline(v = c(13.5, 17.5), lty = 2, col = "grey30")



#for 2005/06
SE.preds.2005 <- lapply(SE.predict.single, function(x) x[[5]])

SEpreds2005.wo2019 <- c(SE.preds.2005$`2019-2020`$early, SE.preds.2005$`2019-2020`$peak, SE.preds.2005$`2019-2020`$late)
SEpreds2005.wo2005 <- c(SE.preds.2005$`2005-2006`$early, SE.preds.2005$`2005-2006`$peak, SE.preds.2005$`2005-2006`$late)
SEresp.2005 <- c(SE.resp.valid$`2005-2006`$early, SE.resp.valid$`2005-2006`$mid, SE.resp.valid$`2005-2006`$late)

pred.range <- range(SEresp.2005, SEpreds2005.wo2005, SEpreds2005.wo2019)

plot(1:29, SEresp.2005, type = "l", ylim = pred.range, main = "2005/2006")
lines(1:29, SEpreds2005.wo2005, col = "firebrick", lty = 2)
lines(1:29, SEpreds2005.wo2019, col = "firebrick4", lty = 4)
abline(h = 0, lty = 3, col = "grey30")
abline(v = c(13.5, 17.5), lty = 2, col = "grey30")


#look at rmse for important years
#single withheld RMSE:


