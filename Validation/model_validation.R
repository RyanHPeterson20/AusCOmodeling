
#model validation from lass_models.R (new_lasso_models.R)


##includes:
#predictions,
#prediction intervals,
#RMSE, 

#libraries
suppressMessages( library(RAMP)) #Lasso with efficient solution path.
suppressMessages( library(Metrics)) #measurement metrics

#import data and models
setwd("~/CO_AUS/AusCOmodeling")
load("Data/matrixdata.rda") #data as matrix
load("Data/lagdata.rda") #lagged data
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model

#load functions
source("Functions/modeling_functions.R")


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

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)

SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)


## --- partial data setup 
#leaving out a single year (loo)
SE.pred.train <- list()
SE.resp.train <- list()
for (k in 1:length(seasons)) {
  #SE Aus
  SE.pred.train[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = -c(k))
  SE.resp.train[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = -c(k)) 
}

#extracting a single year (validation data)
SE.pred.valid <- list()
SE.resp.valid <- list()
for (k in 1:length(seasons)) {
  #SE Aus
  SE.pred.valid[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = c(k))
  SE.resp.valid[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = c(k)) 
}


SE.vary.terms <- NULL #non-fixed terms
SE.const.LM <- NULL #fixed term model
SE.vary.LM <- NULL #non-fixed term model
SE.pred.int <- NULL #predctions and intervals
SE.rmse <- NULL
for (i in 1:length(seasons)) {
  #data w/o season (train)
  train.resp <- SE.resp.train[[i]]
  train.pred <- SE.pred.train[[i]]
  
  #data w/ season (test/validation)
  valid.resp <- SE.resp.valid[[i]]
  valid.pred <- SE.pred.valid[[i]]    
  
  #group data objects
  SE.var.refit <- NULL #varying terms
  SE.con <- NULL #constant linear models
  SE.var <- NULL #varying linear models
  SErmse.yearly <- matrix(NA, ncol = 3)
  colnames(SErmse.yearly) <- c("base.pred", "const.pred", "vary.pred")
  SE.intervals <- matrix(NA, ncol = 10)
  colnames(SE.intervals) <- c("true", "base.fit", "base.lwr", "base.upr",  
                              "const.fit", "const.lwr", "const.upr",
                              "vary.fit", "vary.lwr", "vary.upr")
  
  for (j in 1:3) {
    #get base model terms (and fits)
    SE.base.LM <- SEmodels[[j]] #lm model for NE group j 
    SE.base.terms <- SErefits[[j]] #terms for NE group j
    
    #lm fit data setup
    y.train <- as.numeric(train.resp[[j]])
    #with OLR
    X.train <- cbind(as.matrix(train.pred[[j]][ ,c(1:52, 105:364)])) 
    
    #varying ramp fit
    vary.fit <- RAMP(X = X.train, y = y.train,
                     penalty = "LASSO",
                     tune = "BIC",
                     n.lambda = 500)
    #refit
    SE.refit.vary <- refit_ramp(vary.fit, X.train)
    
    lm.data.fit <- as.data.frame(cbind(y.train, X.train))
    names(lm.data.fit)[1] <- "co"
    
    #refit
    SE.lm.const <- lm(formula(SE.base.terms), lm.data.fit)
    SE.lm.vary <- lm(formula(SE.refit.vary), lm.data.fit)
    
    #assign terms and models
    SE.var.refit[[j]] <- SE.refit.vary
    SE.con[[j]] <- SE.lm.const
    SE.var[[j]] <- SE.lm.vary

    #get predictions and pred intervals
    y.valid <- as.numeric(valid.resp[[j]])
    X.valid <- valid.pred[[j]][ ,c(1:52, 105:364)]
    
  
    #rmse
    pred.base <- predict(SE.base.LM, X.valid, se.fit = TRUE)
    pred.const <- predict(SE.lm.const, X.valid, se.fit = TRUE)
    pred.vary <- predict(SE.lm.vary, X.valid, se.fit = TRUE)
    
    rmse.base <-  rmse(y.valid, pred.base$fit)
    rmse.const <- rmse(y.valid, pred.const$fit)
    rmse.vary <- rmse(y.valid, pred.vary$fit)
    
    #intervals
    pred.base.interval <- predict(SE.base.LM, X.valid, interval = "prediction")
    pred.const.interval <- predict(SE.lm.const, X.valid, interval = "prediction")
    pred.vary.interval <- predict(SE.lm.vary, X.valid, interval = "prediction")
    
    #assign validations
    SErmse.yearly <- rbind(SErmse.yearly, cbind(rmse.base, rmse.const, rmse.vary))
    
    #assign intervals
    SE.intervals <- rbind(SE.intervals, 
                          cbind(y.valid, pred.base.interval, pred.const.interval, pred.vary.interval))
    
    
  }
  SE.vary.terms[[seasons[i]]] <- SE.var.refit
  SE.const.LM[[seasons[i]]] <- SE.con
  SE.vary.LM[[seasons[i]]] <- SE.var
  
  SE.rmse[[seasons[i]]] <- as.data.frame(SErmse.yearly[-1, ])
  SE.pred.int[[seasons[i]]] <- as.data.frame(SE.intervals[-1, ])
}  

SEvalid <- list(rmse = SE.rmse, preds = SE.pred.int)
SErefit.new <- list(SE.vary.terms, SE.const.LM, SE.vary.LM)

setwd("~/CO_AUS/AusCOmodeling/Data") 
save(SEvalid, SErefit.new, file = "validation_refits_new.rda")




#repeat for new withheld (varying term) models without 2019/2020 and without another year, 

#TODO: expand this for every year, not just wo 2019/2020

## --- partial data setup 
#leaving out a single year (loo) and 2019/2020
SE.pred.train <- list()
SE.resp.train <- list()
for (k in 1:length(seasons)) {
  #SE Aus
  SE.pred.temp <- list()
  SE.resp.temp <- list()
  for (j in 1:length(seasons)) {
    SE.pred.temp[[seasons[j]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = -c(k, j))
    SE.resp.temp[[seasons[j]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = -c(k, j)) 
  }
  SE.pred.train[[seasons[k]]] <- SE.pred.temp
  SE.resp.train[[seasons[k]]] <- SE.resp.temp
}

#extracting a single year (validation data)
SE.pred.valid <- list()
SE.resp.valid <- list()
for (k in 1:length(seasons)) {
  #SE Aus
  SE.pred.valid[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = c(k))
  SE.resp.valid[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = c(k)) 
}



#double withheld seasons
SE.vary.terms <- NULL #non-fixed terms
SE.vary.LM <- NULL #non-fixed term model
SE.pred.int <- NULL #predictions and intervals
SE.rmse <- NULL
for (i in 1:length(seasons)) {
  #data w/o season (train)
  train.resp <- SE.resp.train[[i]]
  train.pred <- SE.pred.train[[i]]
  
  #data w/ season (test/validation)
  valid.resp <- SE.resp.valid[[i]]
  valid.pred <- SE.pred.valid[[i]]    
  
  #group data objects
  SE.inner.terms <- NULL #non-fixed terms
  SE.inner.LM <- NULL #non-fixed term model
  SE.inner.pred <- NULL #predictions and intervals
  SE.inner.rmse <- NULL
  
  for (k in 1:length(seasons)) {
    #additional withheld years
    temp.resp <- train.resp[[k]]
    temp.pred <- train.pred[[k]]
    
    #group data objects
    SE.var.refit <- NULL #varying terms
    SE.var <- NULL #varying linear models
    SErmse.yearly <- matrix(NA, ncol = 1)
    colnames(SErmse.yearly) <- c("vary.pred")
    SE.intervals <- matrix(NA, ncol = 4)
    colnames(SE.intervals) <- c("true", 
                                "vary.fit", "vary.lwr", "vary.upr")
  
  for (j in 1:3) {
    
    #lm fit data setup
    y.train <- as.numeric(temp.resp[[j]])
    #with OLR
    X.train <- cbind(as.matrix(temp.pred[[j]][ ,c(1:52, 105:364)])) 
    
    #varying ramp fit
    vary.fit <- RAMP(X = X.train, y = y.train,
                     penalty = "LASSO",
                     tune = "BIC",
                     n.lambda = 500)
    #refit
    SE.refit.vary <- refit_ramp(vary.fit, X.train)
    
    lm.data.fit <- as.data.frame(cbind(y.train, X.train))
    names(lm.data.fit)[1] <- "co"
    
    SE.lm.vary <- lm(formula(SE.refit.vary), lm.data.fit)
    
    #assign terms and models
    SE.var.refit[[j]] <- SE.refit.vary
    SE.var[[j]] <- SE.lm.vary
    
    #get predictions and pred intervals
    y.valid <- as.numeric(valid.resp[[j]])
    X.valid <- valid.pred[[j]][ ,c(1:52, 105:364)]

    pred.vary <- predict(SE.lm.vary, X.valid, se.fit = TRUE)

    rmse.vary <- rmse(y.valid, pred.vary$fit)
    
    #intervals
    pred.vary.interval <- predict(SE.lm.vary, X.valid, interval = "prediction")
    
    #assign validations
    SErmse.yearly <- rbind(SErmse.yearly, cbind(rmse.vary))
    
    #assign intervals
    SE.intervals <- rbind(SE.intervals, 
                          cbind(y.valid,  pred.vary.interval))
    
    
  }
    SE.inner.terms[[seasons[i]]] <- SE.var.refit
    SE.inner.LM[[seasons[i]]] <- SE.var
    
    SE.inner.rmse[[seasons[i]]] <- as.data.frame(rmse = SErmse.yearly[-1, ])
    SE.inner.pred[[seasons[i]]] <- as.data.frame(SE.intervals[-1, ])
  
  
  }
  SE.vary.terms[[seasons[i]]] <- SE.inner.terms
  SE.vary.LM[[seasons[i]]] <- SE.inner.LM
  
  SE.rmse[[seasons[i]]] <- SE.inner.rmse
  SE.pred.int[[seasons[i]]] <- SE.inner.pred
}  


SErefit.wo.years <- list(rmse = SE.rmse, preds = SE.pred.int, SE.vary.lm = SE.vary.LM)

setwd("~/CO_AUS/AusCOmodeling/Data") 
save(SErefit.wo.years, file = "validation_refits_wo2019.rda")
