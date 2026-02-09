#new work on RMSE differences, etc


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(Metrics)) #measurement metrics

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
heatmap_fields_rwb0 <- function(z, x = NULL, y = NULL,
                                zlim = NULL,            # if NULL, uses symmetric max(abs(z))
                                nlevels = 101,          # odd number keeps 0 exactly centered
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
  cols <- colorRampPalette(c("blue3", "white", "red3"))(nlevels)
  
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

 #withheld year (model year)
for (i in 1:20) {
    
  
  temp.lm.list <- SErefit.new[[3]][[i]]
  #summary(temp.lm.list[[1]]) #early
  #summary(temp.lm.list[[2]]) #peak
  #summary(temp.lm.list[[3]]) #late
  
  
  SErmse.yearly <- matrix(NA, ncol = 3)
  colnames(SErmse.yearly) <- c("early", "peak", "late")
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
    
    SErmse.yearly <- rbind(SErmse.yearly, cbind(rmse.early, rmse.mid, rmse.late))
  }
  SE.rmse.single[[seasons[i]]] <- as.data.frame(SErmse.yearly[-1, ])
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


which(rmse.diff.peak[,19] >0 )

rmse.diff.peak[,which(rmse.diff.peak[,19] >0 )]

z.peak <- t(rmse.diff.peak[20:1, ])

par(oma = c(4, 4, 2, 3))  
heatmap_fields_rwb0(z = z.peak, main = expression(" Peak Fire-Season: "~Delta*"RMSE (All-data - Withheld-Season)") )
mtext("Withheld-Season", side=1, line=6.0, cex = 1.25)
mtext("Prediction Season",  side=2, line=6.0, cex = 1.25)

