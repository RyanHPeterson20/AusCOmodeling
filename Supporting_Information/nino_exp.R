#exploration of nino effects in pos peak seasons

#seasons of interest: 2002/03, 2003/04, 2005/06, 2006/07, 2015/16

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


#season index
pos.ind <- c(2,3,5,6,15)

#plot rmse figures (rmse diff)
rmse.mat.early <- matrix(NA, nrow = 20)
rmse.mat.peak <- matrix(NA, nrow = 20)
rmse.mat.late <- matrix(NA, nrow = 20)
for (j in 1:20) {
  
  rmse.mat.early <- cbind(rmse.mat.early, SE.rmse.single[[j]]$early)
  rmse.mat.peak <- cbind(rmse.mat.peak, SE.rmse.single[[j]]$peak)
  rmse.mat.late <- cbind(rmse.mat.late, SE.rmse.single[[j]]$late)
  
}

rmse.mat.peak <- rmse.mat.peak[,-1]

#select prediction years we want (e.g. positive indices)
rmse.mat.peak <- rmse.mat.peak[pos.ind, ]

rownames(rmse.mat.peak) <- seasons[pos.ind]
colnames(rmse.mat.peak) <- seasons

z.peak.single <- t(rmse.mat.peak[5:1,])

#get the figures working correctly
z <- z.peak.single
y <- seq_len(ncol(z))
x <- seq_len(nrow(z))

x_n = 20
y_n = 5
xi <- unique(round(seq(1, nrow(z), length.out = x_n)))
yi <- unique(round(seq(1, ncol(z), length.out = y_n)))

x_at <- x[xi]
y_at <- y[yi]

x_lab <- rownames(z)[xi]
y_lab <- colnames(z)[yi]

image.plot(x, y, z, zlim =  c(0, max(z.peak.single)), 
           col = cmocean("deep")(49),xaxt = "n", yaxt = "n",
           main = "Peak Fire-Season: RMSE Withheld-Season",
           xlab = "", ylab = "")
axis(1, at = x_at, labels = x_lab, las = 2)
axis(2, at = y_at, labels = y_lab, las = 1)


#get relative to all-data and fixed-term (selection) models
alldata.peak.rmse <- sapply(SEvalid$rmse, function(x) x$base.pred[2])

rev.all.rmse <- rev(alldata.peak.rmse[pos.ind])

z.diff <- sweep(z.peak.single, 2,  rev.all.rmse, FUN = "-")

diff.max <- max(abs(z.diff))

image.plot(x, y, z.diff, zlim =  c(-diff.max, diff.max), 
           col = cmocean("balance")(49),xaxt = "n", yaxt = "n",
           main = "Peak Fire-Season: RMSE Withheld-Season - All Data",
           xlab = "", ylab = "")
axis(1, at = x_at, labels = x_lab, las = 2)
axis(2, at = y_at, labels = y_lab, las = 1)


#TODO: get predictions for the 5 seasons using the fixed-term (const) model


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




#withheld year (model year)

  i <- 19

  temp.lm.list <- SErefit.new[[2]][[i]]
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

SErmse.yearly <- SErmse.yearly[-1, ]
  
  
rownames(SErmse.yearly) <- seasons

SErmse.const.pos <- SErmse.yearly[pos.ind,2]


z.diff2 <- sweep(z.peak.single, 2,  rev(SErmse.const.pos), FUN = "-")

diff.max <- max(abs(z.diff2))

image.plot(x, y, z.diff2, zlim =  c(-diff.max, diff.max), 
           col = cmocean("balance")(49),xaxt = "n", yaxt = "n",
           main = "Peak Fire-Season: RMSE Withheld-Season - Fixed-Term",
           xlab = "", ylab = "")
axis(1, at = x_at, labels = x_lab, las = 2)
axis(2, at = y_at, labels = y_lab, las = 1)


#more exploratory, withheld models that are generally worse compared to all-data

summary(SErefit.new[[3]]$`2001-2002`[[2]]) #basically only has nino/etio really bad for 2005/06 
summary(SErefit.new[[3]]$`2009-2010`[[2]]) #again only nino/etio
summary(SErefit.new[[3]]$`2013-2014`[[2]]) #better for 2002-2005/06 worse for 2006/07 and 2015/16
summary(SErefit.new[[3]]$`2017-2018`[[2]])


