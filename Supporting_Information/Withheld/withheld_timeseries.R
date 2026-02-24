

#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(lubridate))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
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

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)
SEresp.early <- SEresp.mat[ ,9:13]
SEresp.late <- SEresp.mat[ ,18:22]
SEresp.peak <- SEresp.mat[ ,14:17]
SEresp.peak.wide <- SEresp.mat[ ,13:18]
  
##SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

#get lag 1-52 from week 51 and lags 1-3 from week 2 (for the peak group)
SEpreds.peak <- SE.pred$mid
SEpreds.peak51 <- SEAus.lag$`Week  51`
SEpreds.peak2 <- SEAus.lag$`Week  2`

#up to lag 52
SEpreds.peak51.nino <- SEpreds.peak51[ ,3:54]
SEpreds.peak51.wtio <- SEpreds.peak51[ ,107:158]
SEpreds.peak51.etio <- SEpreds.peak51[ ,159:210]
SEpreds.peak51.tsa <- SEpreds.peak51[ ,211:262] 
SEpreds.peak51.aao <- SEpreds.peak51[ ,263:314]
SEpreds.peak51.olr <- SEpreds.peak51[ ,315:366]
#only up to lag 3
SEpreds.peak2.nino <- SEpreds.peak2[ ,3:5]
SEpreds.peak2.wtio <- SEpreds.peak2[ ,107:109]
SEpreds.peak2.etio <- SEpreds.peak2[ ,159:161]
SEpreds.peak2.tsa <- SEpreds.peak2[ ,211:213] 
SEpreds.peak2.aao <- SEpreds.peak2[ ,263:265]
SEpreds.peak2.olr <- SEpreds.peak2[ ,315:317]

#all lags together
SEpreds.peak.nino <- cbind(SEpreds.peak2.nino, SEpreds.peak51.nino)
SEpreds.peak.wtio <- cbind(SEpreds.peak2.wtio, SEpreds.peak51.wtio)
SEpreds.peak.etio <- cbind(SEpreds.peak2.etio, SEpreds.peak51.etio)
SEpreds.peak.tsa <- cbind(SEpreds.peak2.tsa, SEpreds.peak51.tsa)
SEpreds.peak.aao <- cbind(SEpreds.peak2.aao, SEpreds.peak51.aao)
SEpreds.peak.olr <- cbind(SEpreds.peak2.olr, SEpreds.peak51.olr)

#time/date setup

#for 2001/2002 peak season
pred.df[pred.df$week == 1 & pred.df$year == 2002, ]$date  #lag 1 week 2
pred.df[pred.df$week == 50 & pred.df$year == 2001, ]$date #lag 1 week 51
start.2001 <- pred.df[pred.df$week == 51 & pred.df$year == 2000, ]$date #lag 52 week 51

#TODO: get start dates for the remaining

pred.df[51:105, ]$date


#2001/2002 pred data
nino.anom.2001 <- as.numeric(rev(SEpreds.peak.nino[1, ]))
wtio.anom.2001 <- as.numeric(rev(SEpreds.peak.wtio[1, ]))
etio.anom.2001 <- as.numeric(rev(SEpreds.peak.etio[1, ]))
tsa.anom.2001 <- as.numeric(rev(SEpreds.peak.tsa[1, ]))
aao.anom.2001 <- as.numeric(rev(SEpreds.peak.aao[1, ]))
olr.anom.2001 <- as.numeric(rev(SEpreds.peak.olr[1, ]))


#test for all years
i<-5
start.temp <- pred.df[pred.df$week == 51 & pred.df$year == season.years[i]-1, ]$date

temp.date.start <- ymd(start.temp)
temp.date.end <- temp.date.start + weeks(54)

if(epiweek(temp.date.end) != 1){
  temp.date.end <- temp.date.end + weeks(1)
}

pred.df[which(pred.df$date == temp.date.end), ]

##----- time series -----##

#ts setup
#pred colors
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"

top.col.lag <- "tomato3"
bot.col.lag <- "skyblue4"


#response colors
top.col.resp <- "#C71C1C"
bot.col.resp <- "#2A5674FF"



#temp functions (finalize later)

#monthly ticks and lines
make_month_ticks <- function(xrange) {
  ticks <- seq(floor_date(xrange[1], "month"),
               ceiling_date(xrange[2], "month"),
               by = "1 month")
  labs <- ifelse(month(ticks) == 1, format(ticks, "%b\n%Y"), format(ticks, "%b"))
  list(ticks = ticks, labs = labs)
}

make_month_lines <- function(xrange) {
  seq(floor_date(xrange[1], "month"),
      ceiling_date(xrange[2], "month"),
      by = "1 month")
}

#yearly boundary lines
make_year_lines <- function(xrange) {
  years <- seq(year(xrange[1]), year(xrange[2]), by = 1)
  ymd(paste0(years, "-01-01"))
}

# ---- envelope helpers ----
split_envelope <- function(y) {
  over <- y >= 0
  top <- y 
  top[!over] <- 0
  bot <- y
  bot[over]  <- 0
  list(top = top, bot = bot)
}



#envelope fixes:
add_zero_crossings <- function(x, y) {
  x <- as.Date(x)
  y <- as.numeric(y)
  
  o <- order(x)
  x <- x[o]
  y <- y[o]
  
  keep_x <- x[1]
  keep_y <- y[1]
  
  for (i in 2:length(x)) {
    x0 <- x[i - 1] 
    x1 <- x[i]
    y0 <- y[i - 1]
    y1 <- y[i]
    
    # if segment crosses 0 (strict sign change)
    if (!is.na(y0) && !is.na(y1) && y0 * y1 < 0) {
      # linear interpolation in "days" space
      t <- abs(y0) / (abs(y0) + abs(y1))  # fraction from (i-1) to i
      xc_num <- as.numeric(x0) + t * (as.numeric(x1) - as.numeric(x0))
      xc <- as.Date(xc_num, origin = "1970-01-01")
      
      keep_x <- c(keep_x, xc, x1)
      keep_y <- c(keep_y, 0,  y1)
    } else {
      keep_x <- c(keep_x, x1)
      keep_y <- c(keep_y, y1)
    }
  }
  
  list(x = keep_x, y = keep_y)
}


draw_envelope_zero <- function(x, y, col_pos, col_neg, alpha = 0.67) {
  xy <- add_zero_crossings(x, y)
  x2 <- xy$x
  y2 <- xy$y
  
  # positive polygon: boundary is the line where y>0, otherwise 0
  y_pos <- ifelse(y2 > 0, y2, 0) 
  polygon(c(x2, rev(x2)),
                    c(rep(0, length(x2)), rev(y_pos)),
                    col = alpha(col_pos, alpha), border = NA)
  
  # negative polygon: boundary is the line where y<0, otherwise 0
  y_neg <- ifelse(y2 < 0, y2, 0) 
  polygon(c(x2, rev(x2)),
                    c(y_neg, rep(0, length(x2))),
                    col = alpha(col_neg, alpha), border = NA)
}



#plots
#TODO: clean this up later
panel_ts <- function(x, y, env, ylim, ylab, legend_text, year_lines, xlim, 
                     show_x = FALSE, xticks, xlabs, month_lines,
                     lag_x = FALSE, lag.val) {
  plot(x, y, type = "l", col = "black", lwd = 2,
       xaxt = "n", xlab = "",
       yaxt = "n", ylab = ylab, col.lab = "black",
       xlim = xlim, ylim = ylim, bty = "n",
       cex.lab = 2.75, xpd = NA)
  #mtext(ylab, side = 2, outer = TRUE, line = 0, cex = 1.25)
  
  axis(side = 2, at = y.tick.lab, cex.axis = 2.25,
       col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
  
  abline(h = 0, lty = 1, col = "grey50", lwd = 1)
  
  # year boundaries
  abline(v = year_lines, lty = 2, col = "grey30", lwd = 2)
  
  #monthly lines
  abline(v = month_lines, lty = 3, col = "grey30", lwd = 2)  # monthly
  abline(v = month_lines[month(month_lines) == 1], lty = 2, col = "grey30", lwd = 2) # Jan darker
  
  # envelope
  draw_envelope_zero(x, y, top.col.pred, bot.col.pred, alpha = 0.50)
  if (lag_x) {
    lag.week <- 51 - lag.val
    lag.index <- which(week(pred.time) == lag.week)
    lag.index <- lag.index:(lag.index+3)
    
    draw_envelope_zero(x[lag.index], y[lag.index], top.col.lag, bot.col.lag,  alpha = 0.85)
    
    y.lag.max <- max(y[lag.index], na.rm = TRUE)
    y.lag.text <- ifelse(y.lag.max < 0, 0, y.lag.max)
    
    
    legend(x = c(x[lag.index[1]] - days(9), x[lag.index[1]] - days(9)),
           y = c(y.lag.text+1.5, y.lag.text+1.5),
           legend = paste0("Lag ", lag.val),
           box.col = NA, bg = NA, xpd = NA, text.col = "grey28", cex = 2.5)
    
    
  }
  #envelopePlot(x1 = x, y1 = env$top, x2 = x, y2 = rep(0, length(env$top)),
  #             col = alpha(top.col.pred, 0.67), lineCol = NA)
  #envelopePlot(x1 = x, y1 = env$bot, x2 = x, y2 = rep(0, length(env$bot)),
  #             col = alpha(bot.col.pred, 0.67), lineCol = NA)
  
  # label
  legend(x = c(xlim[1] + days(2), xlim[1] + days(32)),
         y = c(ylim[2], ylim[2]),
         legend = legend_text,
         box.col = NA, bg = NA, xpd = NA, text.col = "grey30", cex = 2.5)
  
  if (show_x) {
    axis(1, at = xticks, labels = xlabs, las = 2, cex.axis = 2.75, line = 1)
  }
}



#pred only lags 1 to 52
ylim.all <- NULL
for (i in 1:20) {
  #temp
  #i <- 15
  
  nino.anom.temp <- as.numeric(rev(SEpreds.peak.nino[i, ]))
  wtio.anom.temp <- as.numeric(rev(SEpreds.peak.wtio[i, ]))
  etio.anom.temp <- as.numeric(rev(SEpreds.peak.etio[i, ]))
  tsa.anom.temp <- as.numeric(rev(SEpreds.peak.tsa[i, ]))
  aao.anom.temp <- as.numeric(rev(SEpreds.peak.aao[i, ]))
  olr.anom.temp <- as.numeric(rev(SEpreds.peak.olr[i, ]))
  
  
  #select window
  start.temp <- pred.df[pred.df$week == 51 & pred.df$year == season.years[i]-1, ]$date
  
  date.start <- ymd(start.temp)
  date.end <- date.start + weeks(54)
  
  if(epiweek(date.end) != 1){
    date.end <- date.end + weeks(1)
  }
  
  pred.dates <- pred.df[pred.df$date >= date.start & pred.df$date <= date.end, ]
  
  pred.time <- as.Date(pred.dates$date)
  pred.time.range <- range(pred.time)
  
  #now using above functions
  #get monthly ticks
  pred.xt <- make_month_ticks(pred.time.range)
  
  #get boundary lines
  pred.month.lines <- make_month_lines(pred.time.range)
  pred.year.lines <- make_year_lines(pred.time.range)
  
  #y tick lines/setup
  
  #max vals
  #TODO: clean this up a little more, not happy with this...
  y.nino.max <- max(abs(range(nino.anom.temp, na.rm = TRUE)))
  y.wtio.max <- max(abs(range(wtio.anom.temp, na.rm = TRUE)))
  y.etio.max <- max(abs(round(range(etio.anom.temp, na.rm = TRUE))))
  y.tsa.max  <- max(abs(range(tsa.anom.temp,  na.rm = TRUE)))
  y.aao.max  <- max(abs(round(range(aao.anom.temp, na.rm = TRUE))))
  y.olr.max  <- max(abs(round(range(olr.anom.temp, na.rm = TRUE))))
  
  y.tick.max   <- max(y.nino.max, y.wtio.max, y.etio.max, y.tsa.max, y.aao.max, y.olr.max)
  y.tick.steps <- y.tick.max/2
  y.tick.seq   <- seq(y.tick.steps, y.tick.max - y.tick.steps, by = y.tick.steps)
  y.tick.lab   <- c(-rev(y.tick.seq), 0, y.tick.seq)
  
  #get overall ylim range 
  ylim.all <- c(ylim.all, y.tick.max)
  
  #envelope sets
  env_nino <- split_envelope(nino.anom.temp)
  env_wtio <- split_envelope(wtio.anom.temp)
  env_etio <- split_envelope(etio.anom.temp)
  env_tsa  <- split_envelope(tsa.anom.temp)
  env_aao  <- split_envelope(aao.anom.temp)
  env_olr  <- split_envelope(olr.anom.temp)
  
  #figures vars ()
  
  
  #figure output
  setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
  
  png(filename = paste0("SE", season.years[i], "_pred_ts.png"), width = 4800, height = 5600, res = 275)
  par(mfrow = c(6, 1))
  par(oma = c(5, 3.5, 1, 0))   # extra bottom margin for month labels
  par(mgp = c(4, 2, 0)) # (title, labels, lines)
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, nino.anom.temp, env_nino, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "Ni\u00f1o 3.4", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, wtio.anom.temp, env_wtio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "WTIO", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, etio.anom.temp, env_etio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "ETIO", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, tsa.anom.temp, env_tsa, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "TSA", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, aao.anom.temp, env_aao, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly", legend_text=  "SAM", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(1, 5, 0, 0))
  panel_ts(pred.time, olr.anom.temp, env_olr, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "OLR", pred.year.lines, pred.time.range,
           show_x = TRUE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  dev.off()
  
}



#specific year exploration 2006/2007, 2015/2016, 2019/2020
years <- c(6,15,19)
y.tick.max <- max(ylim.all)
for (i in years) {
  
  nino.anom.temp <- as.numeric(rev(SEpreds.peak.nino[i, ]))
  wtio.anom.temp <- as.numeric(rev(SEpreds.peak.wtio[i, ]))
  etio.anom.temp <- as.numeric(rev(SEpreds.peak.etio[i, ]))
  tsa.anom.temp <- as.numeric(rev(SEpreds.peak.tsa[i, ]))
  aao.anom.temp <- as.numeric(rev(SEpreds.peak.aao[i, ]))
  olr.anom.temp <- as.numeric(rev(SEpreds.peak.olr[i, ]))
  
  
  #select window
  start.temp <- pred.df[pred.df$week == 51 & pred.df$year == season.years[i]-1, ]$date
  
  date.start <- ymd(start.temp)
  date.end <- date.start + weeks(54)
  
  if(epiweek(date.end) != 1){
    date.end <- date.end + weeks(1)
  }
  
  pred.dates <- pred.df[pred.df$date >= date.start & pred.df$date <= date.end, ]
  
  pred.time <- as.Date(pred.dates$date)
  pred.time.range <- range(pred.time)
  #test weekly change for xlim
  pred.time.range[1] <- pred.time.range[1] + weeks(1)
  
  #now using above functions
  #get monthly ticks
  pred.xt <- make_month_ticks(pred.time.range)
  
  #get boundary lines
  pred.month.lines <- make_month_lines(pred.time.range)
  pred.year.lines <- make_year_lines(pred.time.range)
  
  #y tick lines/setup
  y.tick.steps <- round(y.tick.max/2, 1)
  y.tick.seq   <- seq(y.tick.steps, y.tick.max - y.tick.steps, by = y.tick.steps)
  y.tick.lab   <- c(-rev(y.tick.seq), 0, y.tick.seq)
  

  #envelope sets
  env_nino <- split_envelope(nino.anom.temp)
  env_wtio <- split_envelope(wtio.anom.temp)
  env_etio <- split_envelope(etio.anom.temp)
  env_tsa  <- split_envelope(tsa.anom.temp)
  env_aao  <- split_envelope(aao.anom.temp)
  env_olr  <- split_envelope(olr.anom.temp)
  
  #figures vars ()
  
  
  #figure output
  setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures/Time_Series")
  
  png(filename = paste0("SI_SE", season.years[i], "_pred_ts.png"), width = 4800, height = 5600, res = 275)
  par(mfrow = c(6, 1))
  par(oma = c(7, 4, 1, 0))   # extra bottom margin for month labels
  par(mgp = c(4, 0.25, 0)) # (title, labels, lines)
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, nino.anom.temp, env_nino, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "Ni\u00f1o 3.4", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines,
           lag_x = FALSE, lag.val = 40)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, wtio.anom.temp, env_wtio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "WTIO", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines,
           lag_x = TRUE, lag.val = 14)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, etio.anom.temp, env_etio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "ETIO", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines,
           lag_x = TRUE, lag.val = 8)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, tsa.anom.temp, env_tsa, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "TSA", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, aao.anom.temp, env_aao, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly", legend_text=  "SAM", pred.year.lines, pred.time.range,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(1, 5, 0, 0))
  panel_ts(pred.time, olr.anom.temp, env_olr, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text=  "OLR", pred.year.lines, pred.time.range,
           show_x = TRUE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  dev.off()
  
}


#repeat above for 2006, 2015, and 2019 and overlay the associated all-data model lags
#pred colors
top.col.pred <- "#F2855DFF"
bot.col.pred <- "#68ABB8FF"

top.col.lag <- "tomato3"
bot.col.lag <- "skyblue4"


#TODO: response-combo plot

## finalize for all preds
## align ylims for resp (w/o 2019, since we can't see variation in other seasons )
## align ylims for preds across ALL years. 


#get ylims
#pred lims
pred.range <- range(c(SEpreds.peak.nino, SEpreds.peak.wtio, SEpreds.peak.etio,
                      SEpreds.peak.tsa, SEpreds.peak.aao, SEpreds.peak.olr), na.rm = TRUE)

y.tick.max   <- round(max(abs(pred.range)))
y.tick.steps <- y.tick.max/2
y.tick.seq   <- seq(y.tick.steps, y.tick.max - y.tick.steps, by = y.tick.steps)
y.tick.lab   <- c(-rev(y.tick.seq), 0, y.tick.seq)

#resp lims (w/o 2019/2020)
resp.range <- range(SEresp.peak[-19, ])
y.resp.max <- round(max(abs(resp.range)))
y.resp.tick.steps <- y.resp.max/2
y.resp.tick.seq   <- seq(y.resp.tick.steps, y.resp.max - y.resp.tick.steps, by = y.resp.max)
y.resp.tick.lab   <- c(-rev(y.resp.tick.seq), 0, y.resp.tick.seq)

ylim.resp <- c(-y.resp.max, y.resp.max)

#alternate ylim tick labels
y.resp.lab.alt <- c(-12, -6, 0, 6, 12)




#start loop

for (i in 1:20) {
  
  #select window (from preds)
  start.temp <- pred.df[pred.df$week == 51 & pred.df$year == season.years[i]-1, ]$date
  
  date.start <- ymd(start.temp)
  date.end <- date.start + weeks(54)
  
  if(epiweek(date.end) != 1){
    date.end <- date.end + weeks(1)
  }
  
  pred.dates <- pred.df[pred.df$date >= date.start & pred.df$date <= date.end, ]
  
  pred.time <- as.Date(pred.dates$date)
  pred.time.range <- range(pred.time)
  
  #now using above functions
  #get monthly ticks
  pred.xt <- make_month_ticks(pred.time.range)
  
  #get boundary lines
  pred.month.lines <- make_month_lines(pred.time.range)
  pred.year.lines <- make_year_lines(pred.time.range)
  
  #preds
  nino.anom.temp <- as.numeric(rev(SEpreds.peak.nino[i, ]))
  wtio.anom.temp <- as.numeric(rev(SEpreds.peak.wtio[i, ]))
  etio.anom.temp <- as.numeric(rev(SEpreds.peak.etio[i, ]))
  tsa.anom.temp <- as.numeric(rev(SEpreds.peak.tsa[i, ]))
  aao.anom.temp <- as.numeric(rev(SEpreds.peak.aao[i, ]))
  olr.anom.temp <- as.numeric(rev(SEpreds.peak.olr[i, ]))
  
  #envelope sets
  env_nino <- split_envelope(nino.anom.temp)
  env_wtio <- split_envelope(wtio.anom.temp)
  env_etio <- split_envelope(etio.anom.temp)
  env_tsa  <- split_envelope(tsa.anom.temp)
  env_aao  <- split_envelope(aao.anom.temp)
  env_olr  <- split_envelope(olr.anom.temp)
  
  #setup for response
  resp.temp <- SEresp.peak[i, ]
  resp.temp.wide <- SEresp.peak.wide[i, ]
  
  #resp_dates
  start.peak <- resp.df[resp.df$week == 51 & resp.df$year == season.years[i], ]$date
  end.peak <- resp.df[resp.df$week == 2 & resp.df$year == season.years[i]+1, ]$date
  
  resp.dates <- resp.df[resp.df$date >= start.peak & resp.df$date <= end.peak, ]
  resp.time <- as.Date(resp.dates$date)
  resp.time.range <- range(resp.time)
  
  #get boundary lines
  resp.month.lines <- make_month_lines(resp.time.range)
  resp.year.lines <- make_year_lines(resp.time.range)
  
  xlim.common <- range(c(pred.time, resp.time), na.rm = TRUE)
  
  
  #get segment boundary
  seg.start <- ymd(start.peak) - weeks(3)
  seg.end <- ymd(end.peak) + weeks(3)
  
  #test plots
  #TODO: get test output going
  
  
  
  #figure output
  setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
  
  png(filename = paste0("combo_peak_", season.years[i], "_ts.png"), width = 4600, height = 5400, res = 275)
  par(mfrow = c(7, 1))
  par(oma = c(6, 2.5, 1, 0))   # extra bottom margin for month labels
  par(mgp = c(4, 2, 0)) # (title, labels, lines)
  
  #response output
  par(mar = c(0, 5, 0, 0))
  plot(resp.time, resp.temp, type = "l", col = "black", lwd = 2,
       xaxt = "n", xlab = "",
       yaxt = "n", ylab = "", col.lab = "black",
       xlim = xlim.common, ylim = ylim.resp,  bty = "n",
       cex.lab = 2.75, xpd = NA)
  
  axis(side = 2, at = y.resp.lab.alt, cex.axis = 2.25,
       col = NA, line = -105, col.ticks = "black", col.axis = "black", las = 1)
  mtext("CO Anomaly", side = 2, line = -100, cex = 2.0)
  
  segments(seg.start, 0, seg.end, 0, lty = 1, col = "grey", lwd = 1)
  #abline(v = resp.year.lines, lty = 2, col = "grey40", lwd = 2)
  
  #monthly lines
  abline(v = resp.month.lines, lty = 3, col = "grey50", lwd = 1)  # monthly
  abline(v = resp.month.lines[month(resp.month.lines) == 1], lty = 2, col = "grey40", lwd = 2) # Jan darker
  
  legend(x = c(xlim.common[1] - days(3), xlim.common[1] + days(27)),
         y = c(ylim.resp[2], ylim.resp[2]),
         legend = paste0("Peak ", seasons[i], " Season"),
         box.col = NA, bg = NA, xpd = NA, text.col = "grey10", cex = 3.5)
  
  draw_envelope_zero(resp.time, resp.temp, top.col.resp, bot.col.resp)
  
  #preds
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, nino.anom.temp, env_nino, ylim = range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "Ni\u00f1o 3.4",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, wtio.anom.temp, env_wtio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "WTIO",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, etio.anom.temp, env_etio, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "ETIO",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, tsa.anom.temp, env_tsa, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "TSA",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(0, 5, 0, 0))
  panel_ts(pred.time, aao.anom.temp, env_aao, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly", legend_text= "SAM (AAO)",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = FALSE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  par(mar = c(1, 5, 0, 0))
  panel_ts(pred.time, olr.anom.temp, env_olr, range(c(-y.tick.max, y.tick.max)),
           ylab = "Anomaly [W/m^2]", legend_text= "OLR",  year_lines = pred.year.lines, 
           xlim = xlim.common,
           show_x = TRUE, xticks = pred.xt$ticks, xlabs = pred.xt$labs, month_lines = pred.month.lines)
  
  dev.off()

}



setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = paste0("all_resp_ts.png"), width = 4600, height = 5400, res = 275)
par(mfrow = c(4, 5))
for (i in c(1:18,20)) {

  resp.temp <- SEresp.peak[i, ]

  par(mar = c(5, 5, 4, 0))
  plot(1:4, resp.temp, type = "l", col = "black", lwd = 2,
       xaxt = "n", xlab = "Week",
       yaxt = "n", ylab = "CO Anomaly", col.lab = "black",ylim = ylim.resp,  bty = "n",
       cex.lab = 2, xpd = NA)
  title(paste0("Peak ", seasons[i], " Season" ), adj = 0, cex.main = 2)
  
  draw_envelope_zero(1:4, resp.temp, top.col.resp, bot.col.resp)
  
  axis(side = 2, at = y.resp.lab.alt, cex.axis = 2.25,
       col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)
  axis(side = 1, at = 1:4, labels = c(51, 52, 1, 2))
  abline(h =0, lty = 2, lwd = 1, col = "grey50")
  abline(v = 2.5,  lty = 2, col = "grey40", lwd = 2)

}
dev.off()

#TODO: formalize everything below or DELETE
#response only TS

i <- 2
range(c(SEresp.early[-19, ], SEresp.peak[-19, ], SEresp.late[-19, ]))

#setup for response
resp.temp <- c(SEresp.early[i, ], SEresp.peak[i, ], SEresp.late[i, ])

line.col <- rainbow(19)

setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
png(filename = paste0("respone_test.png"), width = 3600, height = 1800, res = 275)
#response output
par(mar = c(4, 5, 1, 0))
plot(NA, NA, type = "l", col = "black", lwd = 2,
     xaxt= "n", xlab = "Week",
     yaxt = "n", ylab = "CO Anomaly", col.lab = "black",
     xlim = c(1,14), ylim = c(-28, 28), bty = "n",
     cex.lab = 2.0, xpd = NA)

axis(side = 2)
axis(side = 1, at = 1:14, labels = c(46:52, 1:7))
abline(h =0, lty = 3, lwd = 1, col = "grey50")
abline(v = c(5.5, 9.5), lwd = 1, lty = 2, col = "grey30")


#mtext("CO Anomaly", side = 2, line = 1, cex = 1.25)

for (i in c(1:18,20)) {
  
resp.temp <- c(SEresp.early[i, ], SEresp.peak[i, ], SEresp.late[i, ])
lines(1:14, resp.temp, col = line.col[i], lwd = 1.5)

}

dev.off()


#test load in data (SE Aus raw mopitt and )
setwd("~/CO_AUS/AusCOmodeling/Data")
