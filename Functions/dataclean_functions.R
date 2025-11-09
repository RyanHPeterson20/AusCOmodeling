
#functions used for cleaning data

# week53_avg()
#avg data from week 52 and 53, then overwrite week 52
##we need to create a uniform set of weeks
## data.df: only the columns with data
## data.week53: indices for epiweek(date)=53
week53_avg <- function(data.df, data.week53){
  
  #get the locations epiweek 53,
  for (j in data.week53) {
    temp.52 <- data.df[j-1, ]
    temp.53 <- data.df[j, ]
    
    data.df[j-1, ] <- colMeans(rbind(temp.52, temp.53))
  }

  return(data.df)
}


# pred_lags()
#function for producing lag prediction dfs
## resp.df:
## pred.df:
## season.weeks:
## seasons:
pred_lags <- function(resp.df, pred.df, season.weeks, seasons){
  
  NE_laglist <- list()
  SE_laglist <- list()
  
  for (i in season.weeks) {
    
    resp.temp <- resp.df[resp.df$week == i, ]
    
    if (i <= 14) {
      resp.temp <- resp.temp[-which(resp.temp$year == min(resp.df$year)), ]
    }
    
    NEdf.temp <- data.frame(seasons, NEAus.anom = resp.temp$NEAus.anom)
    SEdf.temp <- data.frame(seasons, SEAus.anom = resp.temp$SEAus.anom)
    
    #min/max year for response week i
    min.year <- min(resp.temp$year)
    max.year <- max(resp.temp$year)
    
    nino.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    dmi.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    wtio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    etio.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    tsa.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    aao.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    #olr.lag <- matrix(NA, nrow = length(resp.temp$NEAus.anom))
    
    lag.vec <- c()
    for (j in 1:52) {
      lag.week <- i - j
      
      if (lag.week <= 0) {
        lag.week <- 52 + lag.week
        lag.year <- c(min.year - 1, max.year -1)
      } else {
        lag.year <- min.year #what did I do here?
        lag.year <- c(min.year, max.year)
      }
      
      #get lagged predictors
      lag.pred <- pred.df[pred.df$week == lag.week, ]
      lag.pred <- lag.pred[which((lag.pred$year >= lag.year[1] & lag.pred$year <= lag.year[2])),]
      
      nino.lag <- cbind(nino.lag, lag.pred$nino.anom)
      dmi.lag <- cbind(dmi.lag, lag.pred$dmi.anom)
      wtio.lag <- cbind(wtio.lag, lag.pred$wtio.anom)
      etio.lag <- cbind(etio.lag, lag.pred$etio.anom)
      tsa.lag <- cbind(tsa.lag, lag.pred$tsa.anom)
      aao.lag <- cbind(aao.lag, lag.pred$aao.anom)
      #olr.lag <- cbind(olr.lag, lag.olr$olr.anom)
      
      lag.vec <- c(lag.vec, j)
    }
    
    nino.lag <- nino.lag[,-1]
    colnames(nino.lag) <- paste0("nino_lag", lag.vec[1:52])
    nino.lag <- scale(nino.lag, center = TRUE, scale = TRUE)
    
    dmi.lag <- dmi.lag[,-1]
    colnames(dmi.lag) <- paste0("dmi_lag", lag.vec[1:52])
    dmi.lag <- scale(dmi.lag, center = TRUE, scale = TRUE)
    
    wtio.lag <- wtio.lag[,-1]
    colnames(wtio.lag) <- paste0("wtio_lag", lag.vec[1:52])
    wtio.lag <- scale(wtio.lag, center = TRUE, scale = TRUE)
    
    etio.lag <- etio.lag[,-1]
    colnames(etio.lag) <- paste0("etio_lag", lag.vec[1:52])
    etio.lag <- scale(etio.lag, center = TRUE, scale = TRUE)
    
    tsa.lag <- tsa.lag[,-1]
    colnames(tsa.lag) <- paste0("tsa_lag", lag.vec[1:52])
    tsa.lag <- scale(tsa.lag, center = TRUE, scale = TRUE)
    
    aao.lag <- aao.lag[,-1]
    colnames(aao.lag) <- paste0("aao_lag", lag.vec[1:52])
    aao.lag <- scale(aao.lag, center = TRUE, scale = TRUE)
    
    #olr.lag <- olr.lag[ ,-1]
    #colnames(olr.lag) <- paste0("olr_lag", lag.vec[1:52])
    #olr.lag <- scale(olr.lag, center = TRUE, scale = TRUE)
    
    NEweek.lag <- data.frame(NEdf.temp, nino.lag, dmi.lag, wtio.lag, etio.lag, tsa.lag, aao.lag)
    SEweek.lag <- data.frame(SEdf.temp, nino.lag, dmi.lag, wtio.lag, etio.lag, tsa.lag, aao.lag)
    
    NE_laglist[[paste("Week ", i)]] <- NEweek.lag
    SE_laglist[[paste("Week ", i)]] <- SEweek.lag
    
  }
  
  return(list(NElag = NE_laglist, SElag = SE_laglist))
  
}
