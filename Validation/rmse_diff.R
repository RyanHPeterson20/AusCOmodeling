#get (and plot) RMSE differences

#as (all-data RMSE - withheld season RMSE), for multiple withheld seasons

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)


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

#loop through to get each year
SE.rmse <- SEvalid$rmse
SE.rmse.wo <- SErefit.wo.years$rmse

#test block
i <- 1
test.base.2001 <- SE.rmse[[i]]$base.pred
temp.wo2001 <- SE.rmse.wo[[i]]
early.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[1,])
peak.wo2001 <- lapply(temp.wo2001, function(x)  x[2,])
late.wo2001 <- lapply(temp.wo2001, function(x)  x[3,])

diff.early <- sapply(early.wo2001, function(x) test.base.2001[1]-x)
diff.peak <- sapply(peak.wo2001, function(x) test.base.2001[2]-x)
diff.late <- sapply(late.wo2001, function(x) test.base.2001[3]-x)

#test code
diff2.early <- sapply(early.wo2001, function(x) early.wo2001[[i]]-x)


#setup output
RMSE.diff.early <- matrix(NA, ncol = length(seasons))
RMSE.diff.peak <- matrix(NA, ncol = length(seasons))
RMSE.diff.late <- matrix(NA, ncol = length(seasons))
for (i in 1:length(seasons)) {
  base.rmse <- SE.rmse[[i]]$base.pred
  
  early.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[1,])
  peak.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[2,])
  late.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[3,])
  
  diff.early <- sapply(early.wo2001, function(x) base.rmse[1]-x)
  diff.peak <- sapply(peak.wo2001, function(x) base.rmse[2]-x)
  diff.late <- sapply(late.wo2001, function(x) base.rmse[3]-x)
  
  #output as matrix for each group
  RMSE.diff.early <- rbind(RMSE.diff.early, diff.early)
  RMSE.diff.peak <- rbind(RMSE.diff.peak, diff.peak)
  RMSE.diff.late <- rbind(RMSE.diff.late, diff.late)
}

RMSE.diff.early <- RMSE.diff.early[-1, ]
RMSE.diff.peak <- RMSE.diff.peak[-1, ]
RMSE.diff.late <- RMSE.diff.late[-1, ]

row.names(RMSE.diff.early) <- seasons
row.names(RMSE.diff.peak) <- seasons
row.names(RMSE.diff.late) <- seasons




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

#early
z.early <- t(RMSE.diff.early)[ ,20:1]
heatmap_fields_rwb0(z = z.temp, xlab = "Withheld-Season", ylab = "Prediction Year")
#peak
z.peak <- t(RMSE.diff.peak)[ ,20:1]
heatmap_fields_rwb0(z = z.peak)
#late
z.late <- t(RMSE.diff.late)[ ,20:1]
heatmap_fields_rwb0(z = z.late)


#update for difference from the normal ("yellow") withheld season

#setup output
RMSE.diff2.early <- matrix(NA, ncol = length(seasons))
RMSE.diff2.peak <- matrix(NA, ncol = length(seasons))
RMSE.diff2.late <- matrix(NA, ncol = length(seasons))
for (i in 1:length(seasons)) {
  early.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[1,])
  peak.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[2,])
  late.wo2001 <- lapply(SE.rmse.wo[[i]], function(x)  x[3,])
  
  diff.early <- sapply(early.wo2001, function(x) early.wo2001[[i]]-x)
  diff.peak <- sapply(peak.wo2001, function(x) peak.wo2001[[i]]-x)
  diff.late <- sapply(late.wo2001, function(x) late.wo2001[[i]]-x)
  
  #output as matrix for each group
  RMSE.diff2.early <- rbind(RMSE.diff2.early, diff.early)
  RMSE.diff2.peak <- rbind(RMSE.diff2.peak, diff.peak)
  RMSE.diff2.late <- rbind(RMSE.diff2.late, diff.late)
}

RMSE.diff2.early <- RMSE.diff2.early[-1, ]
RMSE.diff2.peak <- RMSE.diff2.peak[-1, ]
RMSE.diff2.late <- RMSE.diff2.late[-1, ]

row.names(RMSE.diff2.early) <- seasons
row.names(RMSE.diff2.peak) <- seasons
row.names(RMSE.diff2.late) <- seasons

#early
z.early <- t(RMSE.diff2.early)[ ,20:1]
heatmap_fields_rwb0(z = z.early, xlab = "Withheld-Season", ylab = "Prediction Year")
#peak
z.peak <- t(RMSE.diff2.peak)[ ,20:1]
heatmap_fields_rwb0(z = z.peak, xlab = "Withheld-Season", ylab = "Prediction Year")
#late
z.late <- t(RMSE.diff2.late)[ ,20:1]
heatmap_fields_rwb0(z = z.late, xlab = "Withheld-Season", ylab = "Prediction Year")


#create plots ( as normal plot with type = "b" and highlight the year where both withheld are the same year.)

#note, using temporary mfrow to fit the quick EDA, update to c(5,2) (or c(5,4)) for final output
par(mfrow = c(3, 2))
for (i in 1:6) {
  #TODO: finalize and update
  plot(1:20, RMSE.diff.early[i, ], type = "b", pch = 16, 
       main = seasons[i])
  abline(h =  RMSE.diff.early[i, i], lty = 2)
}

par(mfrow = c(3, 2))
for (i in 7:12) {
  #TODO: finalize and update
  plot(1:20, RMSE.diff.early[i, ], type = "b", pch = 16, 
       main = seasons[i])
  abline(h =  RMSE.diff.early[i, i], lty = 2)
}

par(mfrow = c(3, 2))
for (i in 13:18) {
  #TODO: finalize and update
  plot(1:20, RMSE.diff.early[i, ], type = "b", pch = 16, 
       main = seasons[i])
  abline(h =  RMSE.diff.early[i, i], lty = 2)
}

par(mfrow = c(1, 2))
for (i in 19:20) {
  #TODO: finalize and update
  plot(1:20, RMSE.diff.early[i, ], type = "b", pch = 16, 
       main = seasons[i])
  abline(h =  RMSE.diff.early[i, i], lty = 2)
}

dev.off()






