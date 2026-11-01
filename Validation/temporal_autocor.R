
#temporal autocorrelation

#libraries
suppressMessages( library( dplyr)) 
suppressMessages( library( scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
#load("Data/base_RAMPmodels.rda") #"base" model
#load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/std_RAMPmodels.rda") #updates lasso models

#TODO: update these notes after complete:
# Assuming your OLS model is fit and your data frame has columns:
# resid = model residuals
# year  = wildfire season year
# week  = calendar week number

#SEmodels
SE.early.lm <- SEmodels[[1]] 
SE.peak.lm <- SEmodels[[2]]
SE.late.lm <- SEmodels[[3]]


# Sort by year then week within year
#group weeks
SE.early <- 38:50
SE.peak <- c(51, 52, 1, 2)
SE.late <- 3:14


# Determine max lag based on number of weeks per year in this sub-season
max.early.lag <- length(SE.early) - 1  
max.peak.lag <- length(SE.peak) - 1  
max.late.lag <- length(SE.late) - 1  


library(sandwich)
library(lmtest)

vcov_nw <- NeweyWest(SE.early.lm, lag = max.early.lag, prewhite = FALSE)
coeftest(SE.early.lm, vcov = vcov_nw)


early.resid <- residuals(SE.early.lm)

#get acf for each year
acf_early <- NULL
for (j in 1:20) {
  x <- early.resid[seq(j, 260, by = 20)]
  acf_early <- rbind(acf_early,
                 acf(x, lag.max = max.early.lag, plot = FALSE)$acf[-1])  # drop lag 0
}


avg_acf_early <- colMeans(acf_early) #means by lag
lag_labels <- paste("Lag", seq_along(avg_acf_early))

ci_bound   <- 1.96 / sqrt(length(SE.early))  # rough 95% CI for each year's ACF

boxplot(acf_early,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        xlab      = "Lag",
        ylim      = c(-1, 1),
        pch = 16)
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")



barplot(avg_acf_early,
        names.arg = lag_labels,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        ylim      = c(-1, 1))
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")



peak.resid <- residuals(SE.peak.lm)

#get acf for each year
acf_peak <- NULL
for (j in 1:20) {
  x <- peak.resid[seq(j, 80, by = 20)]
  acf_peak <- rbind(acf_peak,
                     acf(x, lag.max = max.peak.lag, plot = FALSE)$acf[-1])  # drop lag 0
}



avg_acf_peak <- colMeans(acf_peak) #means by lag
lag_labels <- paste("Lag", seq_along(avg_acf_peak))

ci_bound   <- 1.96 / sqrt(length(SE.peak))  # rough 95% CI for each year's ACF

boxplot(acf_peak,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        xlab      = "Lag",
        ylim      = c(-1, 1),
        pch = 16)
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")


barplot(avg_acf_peak,
        names.arg = lag_labels,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        ylim      = c(-1, 1))
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")



late.resid <- residuals(SE.late.lm)

#get acf for each year
acf_late <- NULL
for (j in 1:20) {
  x <- late.resid[seq(j, 240, by = 20)]
  acf_late <- rbind(acf_late,
                    acf(x, lag.max = max.late.lag, plot = FALSE)$acf[-1])  # drop lag 0
}

colMeans(acf_late) #means by lag


avg_acf_late <- colMeans(acf_late) #means by lag
lag_labels <- paste("Lag", seq_along(avg_acf_late))

ci_bound   <- 1.96 / sqrt(length(SE.late))  # rough 95% CI for each year's ACF

boxplot(acf_late,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        xlab      = "Lag",
        ylim      = c(-1, 1),
        pch = 16)
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")



barplot(avg_acf_late,
        names.arg = lag_labels,
        main      = "Within-Year ACF of Residuals",
        ylab      = "ACF",
        ylim      = c(-1, 1))
abline(h = c(-ci_bound, ci_bound), lty = 2, col = "blue")
abline(h = 0, lty = 3, col = "gray33")



library(lmtest)

dw_blocked <- function(e, weeks_per_season) {
  e <- as.numeric(e)
  n_seasons <- length(e) / weeks_per_season
  if (n_seasons %% 1 != 0) {
    stop("Residual length is not a multiple of weeks_per_season. Check row order or dropped rows.")
  }
  season <- rep(seq_len(n_seasons), each = weeks_per_season)
  
  num <- sum(tapply(e, season, function(es) sum(diff(es)^2)))
  num / sum(e^2)
}


dw_blocked(residuals(SE.early.lm), 13)   # 260 / 13 = 20 seasons
dw_blocked(residuals(SE.peak.lm),   4)   #  80 /  4 = 20
dw_blocked(residuals(SE.late.lm),  12)   # 240 / 12 = 20


#acf output (within-block)
acf_blocked_df <- function(e, weeks_per_season, max_lag, group) {
  e <- as.numeric(e)
  n_seasons <- length(e) / weeks_per_season
  season <- rep(seq_len(n_seasons), each = weeks_per_season)
  blocks <- split(e, season)
  
  out <- lapply(1:max_lag, function(k) {
    pairs <- do.call(rbind, lapply(blocks, function(es) {
      if (length(es) > k) cbind(es[1:(length(es) - k)], es[(k + 1):length(es)]) else NULL
    }))
    data.frame(
      group   = group,
      lag     = k,
      acf     = cor(pairs[, 1], pairs[, 2]),
      n_pairs = nrow(pairs)
    )
  })
  
  out <- do.call(rbind, out)
  out$ci <- 1.96 / sqrt(out$n_pairs)
  out
}

acf_df <- rbind(
  acf_blocked_df(residuals(SE.early.lm), 13, 4, "Early"),
  acf_blocked_df(residuals(SE.peak.lm),   4, 3, "Peak"),
  acf_blocked_df(residuals(SE.late.lm),  12, 4, "Late")
)
acf_df$group <- factor(acf_df$group, levels = c("Early", "Peak", "Late"))


plot_acf_blocked <- function(acf_df, dw = NULL) {
  groups <- levels(acf_df$group)
  ylim   <- range(c(acf_df$acf, acf_df$ci, -acf_df$ci)) * 1.15
  
  op <- par(mfrow = c(1, 3), mar = c(4, 1, 3, 1), oma = c(0, 4, 1, 1))
  on.exit(par(op))
  
  for (i in seq_along(groups)) {
    g  <- groups[i]
    d  <- acf_df[acf_df$group == g, ]
    d  <- d[order(d$lag), ]
    
    plot(NA, xlim = c(0.5, max(acf_df$lag) + 0.5), ylim = ylim,
         xlab = "Within-season lag (weeks)", ylab = "",
         xaxt = "n", yaxt = "n", main = g, bty = "l")
    
    axis(1, at = 1:max(acf_df$lag))
    if (i == 1) {
      axis(2, las = 1)
      mtext("Residual autocorrelation", side = 2, line = 3, cex = 0.8)
    }
    
    # confidence band, widening with lag as pair count drops
    polygon(c(d$lag, rev(d$lag)), c(d$ci, rev(-d$ci)),
            col = adjustcolor("grey60", alpha.f = 0.3), border = NA)
    
    abline(h = 0, col = "grey30", lwd = 0.8)
    
    cols <- ifelse(d$acf > 0, "#B2182B", "#2166AC")
    segments(d$lag, 0, d$lag, d$acf, col = cols, lwd = 2)
    points(d$lag, d$acf, pch = 19, col = cols, cex = 1.2)
    
    if (!is.null(dw)) {
      lab <- sprintf("DW = %.2f", dw[[g]])
      mtext(lab, side = 3, line = -1.5, adj = 0.95, cex = 0.7, col = "grey30")
    }
  }
}

dw_vals <- list(Early = 1.871, Peak = 1.864, Late = 1.957)
plot_acf_blocked(acf_df, dw = dw_vals)

