
#using RAMP to fit models 

#notes:
##

#libraries
suppressMessages( library(RAMP)) #Lasso with efficient solution path.
suppressMessages( library( Metrics)) #measurement metrics
#parallelization setup
suppressMessages( library(foreach)) 
suppressMessages( library(parallel))
suppressMessages( library(doParallel))

#data import
#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix
load("Data/lagdata.rda") #lagged data
load("Data/modeldata.rda") #resp/pred data

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
NE.early <- 38:46
NE.mid <- 47:51
NE.late <- c(52, 1:14)

SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
NEresp.mat <- scale(resp.matrix[,1:29], center = TRUE, scale = FALSE)
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)

NE.resp <- resp_setup(NEresp.mat, season.weeks, NE.early, NE.mid, NE.late) 
NE.pred <- pred_setup(NEAus.lag, season.weeks, NE.early, NE.mid, NE.late)
SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)


#main 

#NE Aus Early (Group 1)
y.1 <- as.numeric(NE.resp[[1]]) #co response
X.1 <- cbind(as.matrix(NE.pred[[1]][ ,c(1:52, 105:364)])) #preds without DMI

NE1.ramp <- RAMP(X = X.1, y = y.1,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

NE1.refit <- refit_ramp(NE1.ramp, X.1)

#lm refit
lm.data.1 <- as.data.frame(cbind(y.1, X.1))
names(lm.data.1)[1] <- "co"

NE1.lm <- lm(formula(NE1.refit), lm.data.1)

summary(NE1.lm)


#NE Aus Middle (Group 2)
y.2 <- as.numeric(NE.resp[[2]]) #co response
X.2 <- cbind(as.matrix(NE.pred[[2]][ ,c(1:52, 105:364)])) #preds without DMI

NE2.ramp <- RAMP(X = X.2, y = y.2,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

NE2.refit <- refit_ramp(NE2.ramp, X.2)

#lm refit
lm.data.2 <- as.data.frame(cbind(y.2, X.2))
names(lm.data.2)[1] <- "co"

NE2.lm <- lm(formula(NE2.refit), lm.data.2)

summary(NE2.lm)

#NE Aus Late (Group 3)
y.3 <- as.numeric(NE.resp[[3]]) #co response
X.3 <- cbind(as.matrix(NE.pred[[3]][ ,c(1:52, 105:364)])) #preds without DMI

NE3.ramp <- RAMP(X = X.3, y = y.3,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

NE3.refit <- refit_ramp(NE3.ramp, X.3)

#lm refit
lm.data.3 <- as.data.frame(cbind(y.3, X.3))
names(lm.data.3)[1] <- "co"

NE3.lm <- lm(formula(NE3.refit), lm.data.3)

summary(NE3.lm)


#SE Aus Early (Group 1)
y.1 <- as.numeric(SE.resp[[1]]) #co response
X.1 <- cbind(as.matrix(SE.pred[[1]][ ,c(1:52, 105:364)])) #preds without DMI

SE1.ramp <- RAMP(X = X.1, y = y.1,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

SE1.refit <- refit_ramp(SE1.ramp, X.1)

#lm refit
lm.data.1 <- as.data.frame(cbind(y.1, X.1))
names(lm.data.1)[1] <- "co"

SE1.lm <- lm(formula(SE1.refit), lm.data.1)

summary(SE1.lm)



#SE Aus Middle (Group 2)
y.2 <- as.numeric(SE.resp[[2]]) #co response
X.2 <- cbind(as.matrix(SE.pred[[2]][ ,c(1:52, 105:364)])) #preds without DMI

SE2.ramp <- RAMP(X = X.2, y = y.2,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

SE2.refit <- refit_ramp(SE2.ramp, X.2)

#lm refit
lm.data.2 <- as.data.frame(cbind(y.2, X.2))
names(lm.data.2)[1] <- "co"

SE2.lm <- lm(formula(SE2.refit), lm.data.2)

summary(SE2.lm)


#SE Aus Late (Group 3)
y.3 <- as.numeric(SE.resp[[3]]) #co response
X.3 <- cbind(as.matrix(SE.pred[[3]][ ,c(1:52, 105:364)])) #preds without DMI

SE3.ramp <- RAMP(X = X.3, y = y.3,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

SE3.refit <- refit_ramp(SE3.ramp, X.3)

#lm refit
lm.data.3 <- as.data.frame(cbind(y.3, X.3))
names(lm.data.3)[1] <- "co"

SE3.lm <- lm(formula(SE3.refit), lm.data.3)

#summary(SE3.lm)

#save models
#base model fit output
NEmodels <- list(NE1.lm, NE2.lm, NE3.lm)
NErefits <- list(NE1.refit, NE2.refit, NE3.refit)
SEmodels <- list(SE1.lm, SE2.lm, SE3.lm)
SErefits <- list(SE1.refit, SE2.refit, SE3.refit)
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(NEmodels, SEmodels, 
     NErefits, SErefits, file = "base_RAMPmodels.rda")


#Models without OLR

#NE Aus Early (Group 1)
y.1 <- as.numeric(NE.resp[[1]]) #co response
X.1 <- cbind(as.matrix(NE.pred[[1]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

NE1.ramp.noOLR <- RAMP(X = X.1, y = y.1,
                       penalty = "LASSO",
                       tune = "BIC",
                       n.lambda = 500)

NE1.refit.noOLR <- refit_ramp(NE1.ramp.noOLR, X.1)

#lm refit
lm.data.1 <- as.data.frame(cbind(y.1, X.1))
names(lm.data.1)[1] <- "co"

NE1.lm.noOLR <- lm(formula(NE1.refit.noOLR), lm.data.1)

summary(NE1.lm.noOLR)

#NE Aus Middle (Group 2)
y.2 <- as.numeric(NE.resp[[2]]) #co response
X.2 <- cbind(as.matrix(NE.pred[[2]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

NE2.ramp.noOLR <- RAMP(X = X.2, y = y.2,
                       penalty = "LASSO",
                       tune = "BIC",
                       n.lambda = 500)

NE2.refit.noOLR <- refit_ramp(NE2.ramp.noOLR, X.2)

#lm refit
lm.data.2 <- as.data.frame(cbind(y.2, X.2))
names(lm.data.2)[1] <- "co"

NE2.lm.noOLR <- lm(formula(NE2.refit.noOLR), lm.data.2)

summary(NE2.lm.noOLR)
summary(NE2.lm.noOLR)$adj.r.squared


#NE Aus Late (Group 3)
y.3 <- as.numeric(NE.resp[[3]]) #co response
X.3 <- cbind(as.matrix(NE.pred[[3]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

NE3.ramp.noOLR <- RAMP(X = X.3, y = y.3,
                       penalty = "LASSO",
                       tune = "BIC",
                       n.lambda = 500)

NE3.refit.noOLR <- refit_ramp(NE3.ramp.noOLR, X.3)

#lm refit
lm.data.3 <- as.data.frame(cbind(y.3, X.3))
names(lm.data.3)[1] <- "co"

NE3.lm.noOLR <- lm(formula(NE3.refit.noOLR), lm.data.3)

summary(NE3.lm.noOLR)


#SE Aus Early (Group 1)
y.1 <- as.numeric(SE.resp[[1]]) #co response
X.1 <- cbind(as.matrix(SE.pred[[1]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

SE1.ramp.noOLR <- RAMP(X = X.1, y = y.1,
                       penalty = "LASSO",
                       tune = "BIC",
                       n.lambda = 500)

SE1.refit.noOLR <- refit_ramp(SE1.ramp.noOLR, X.1)

#lm refit
lm.data.1 <- as.data.frame(cbind(y.1, X.1))
names(lm.data.1)[1] <- "co"

SE1.lm.noOLR <- lm(formula(SE1.refit.noOLR), lm.data.1)

summary(SE1.lm.noOLR)


#SE Aus Middle (Group 2)
y.2 <- as.numeric(SE.resp[[2]]) #co response
X.2 <- cbind(as.matrix(SE.pred[[2]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

SE2.ramp.noOLR <- RAMP(X = X.2, y = y.2,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

SE2.refit.noOLR <- refit_ramp(SE2.ramp.noOLR, X.2)

#lm refit
lm.data.2 <- as.data.frame(cbind(y.2, X.2))
names(lm.data.2)[1] <- "co"

SE2.lm.noOLR <- lm(formula(SE2.refit.noOLR), lm.data.2)

summary(SE2.lm.noOLR)


#SE Aus Late (Group 3)
y.3 <- as.numeric(SE.resp[[3]]) #co response
X.3 <- cbind(as.matrix(SE.pred[[3]][ ,c(1:52, 105:312)])) #preds without DMI and OLR

SE3.ramp.noOLR <- RAMP(X = X.3, y = y.3,
                       penalty = "LASSO",
                       tune = "BIC",
                       n.lambda = 500)

SE3.refit.noOLR <- refit_ramp(SE3.ramp.noOLR, X.3)

#lm refit
lm.data.3 <- as.data.frame(cbind(y.3, X.3))
names(lm.data.3)[1] <- "co"

SE3.lm.noOLR <- lm(formula(SE3.refit.noOLR), lm.data.3)

summary(SE3.lm.noOLR)

#output models
NEmodels.noOLR <- list(NE1.lm.noOLR, NE2.lm.noOLR, NE3.lm.noOLR)
NErefits.noOLR  <- list(NE1.refit.noOLR, NE2.refit.noOLR, NE3.refit.noOLR)
SEmodels.noOLR  <- list(SE1.lm.noOLR, SE2.lm.noOLR, SE3.lm.noOLR)
SErefits.noOLR  <- list(SE1.refit.noOLR, SE2.refit.noOLR, SE3.refit.noOLR)
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(NEmodels.noOLR , SEmodels.noOLR , 
     NErefits.noOLR , SErefits.noOLR , file = "noOLR_RAMPmodels.rda")


## ---- LOYO Models ---- ##

#Train/Test Setup

## --- partial data setup 
#leaving out a single year (loo)
NE.pred.train <- list()
NE.resp.train <- list()
SE.pred.train <- list()
SE.resp.train <- list()
for (k in 1:length(seasons)) {
  #NE Aus
  NE.pred.train[[seasons[k]]] <- pred_setup(NEAus.lag, season.weeks, NE.early, NE.mid, NE.late, j = -c(k))
  NE.resp.train[[seasons[k]]] <- resp_setup(NEresp.mat, season.weeks, NE.early, NE.mid, NE.late, j = -c(k)) 
  
  #SE Aus
  SE.pred.train[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = -c(k))
  SE.resp.train[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = -c(k)) 
}

#extracting a single year (validation data)
NE.pred.valid <- list()
NE.resp.valid <- list()
SE.pred.valid <- list()
SE.resp.valid <- list()
for (k in 1:length(seasons)) {
  #NE Aus
  NE.pred.valid[[seasons[k]]] <- pred_setup(NEAus.lag, season.weeks, NE.early, NE.mid, NE.late, j = c(k))
  NE.resp.valid[[seasons[k]]] <- resp_setup(NEresp.mat, season.weeks, NE.early, NE.mid, NE.late, j = c(k))
  
  #SE Aus
  SE.pred.valid[[seasons[k]]] <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = c(k))
  SE.resp.valid[[seasons[k]]] <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = c(k)) 
}


#models (refit and lm)
NE.vary.terms <- NULL #non-fixed terms
NE.const.LM <- NULL #fixed term model
NE.vary.LM <- NULL #non-fixed term model
for (i in 1:length(seasons)) {
  #data w/o season (train)
  train.resp <- NE.resp.train[[i]]
  train.pred <- NE.pred.train[[i]]
  
  #data w/ season (test/validation)
  valid.resp <- NE.resp.valid[[i]]
  valid.pred <- NE.pred.valid[[i]]    
  
  #group data objects
  NE.var.refit <- NULL #varying terms
  NE.con <- NULL #constant linear models
  NE.var <- NULL #varying linear models
  
  for (j in 1:3) {
    #get base model terms (and fits)
    NE.base.LM <- NEmodels[[j]] #lm model for NE group j 
    NE.base.terms <- NErefits[[j]] #terms for NE group j
  
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
    NE.refit.vary <- refit_ramp(vary.fit, X.train)
    
    lm.data.fit <- as.data.frame(cbind(y.train, X.train))
    names(lm.data.fit)[1] <- "co"
    
    #refit
    NE.lm.const <- lm(formula(NE.base.terms), lm.data.fit)
    NE.lm.vary <- lm(formula(NE.refit.vary), lm.data.fit)
    
    #assign terms and models
    NE.var.refit[[j]] <- NE.refit.vary
    NE.con[[j]] <- NE.lm.const
    NE.var[[j]] <- NE.lm.vary
    
    #TODO: add in prediction and validation, as needed
  }
  NE.vary.terms[[seasons[i]]] <- NE.var.refit
  NE.const.LM[[seasons[i]]] <- NE.con
  NE.vary.LM[[seasons[i]]] <- NE.var
}  



SE.vary.terms <- NULL #non-fixed terms
SE.const.LM <- NULL #fixed term model
SE.vary.LM <- NULL #non-fixed term model
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
    
    #TODO: add in prediction and validation, as needed
    #get predictions and pred intervals
    
    
  }
  SE.vary.terms[[seasons[i]]] <- SE.var.refit
  SE.const.LM[[seasons[i]]] <- SE.con
  SE.vary.LM[[seasons[i]]] <- SE.var
}  


#TODO: move inside for all years, only for 2019/2020 now
SE1.valid.pred <- SE.pred.valid$`2019-2020`$early
SE1.valid.resp <- SE.resp.valid$`2019-2020`$early

SE2.valid.pred <- SE.pred.valid$`2019-2020`$mid
SE2.valid.resp <- SE.resp.valid$`2019-2020`$mid

SE3.valid.pred <- SE.pred.valid$`2019-2020`$late
SE3.valid.resp <- SE.resp.valid$`2019-2020`$late

X1.valid <- SE1.valid.pred[ ,c(1:52, 105:364)]
X2.valid <- SE2.valid.pred[ ,c(1:52, 105:364)]
X3.valid <- SE3.valid.pred[ ,c(1:52, 105:364)]

SE.2019.true <- c(SE1.valid.resp, SE2.valid.resp, SE3.valid.resp)

#base model
pred.base.early <- predict(SEmodels[[1]],  X1.valid, se.fit = TRUE, interval = "prediction")
pred.base.mid <- predict(SEmodels[[2]],  X2.valid, se.fit = TRUE, interval = "prediction")
pred.base.late <- predict(SEmodels[[3]],  X3.valid, se.fit = TRUE, interval = "prediction")

#const model
pred.const.early <- predict(SE.const.LM$`2019-2020`[[1]],  X1.valid, se.fit = TRUE, interval = "prediction")
pred.const.mid <- predict(SE.const.LM$`2019-2020`[[2]],  X2.valid, se.fit = TRUE, interval = "prediction")
pred.const.late <- predict(SE.const.LM$`2019-2020`[[3]],  X3.valid, se.fit = TRUE, interval = "prediction")

#vary model
pred.vary.early <- predict(SE.vary.LM$`2019-2020`[[1]],  X1.valid, se.fit = TRUE, interval = "prediction")
pred.vary.mid <- predict(SE.vary.LM$`2019-2020`[[2]],  X2.valid, se.fit = TRUE, interval = "prediction")
pred.vary.late <- predict(SE.vary.LM$`2019-2020`[[3]],  X3.valid, se.fit = TRUE, interval = "prediction")

#temp output, update LOYO loop for all years
preds.2019.base <- list(pred.base.early, pred.base.mid, pred.base.late)
preds.2019.const <- list(pred.const.early, pred.const.mid, pred.const.late )
preds.2019.vary <- list(pred.vary.early, pred.vary.mid, pred.vary.late)
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(preds.2019.base, preds.2019.const, 
     preds.2019.vary, SE.2019.true, file = "preds_2019.rda")



#output models
NEmodels.loyo <- list(NE.vary.terms, NE.const.LM, NE.vary.LM)
SEmodels.loyo <- list(SE.vary.terms, SE.const.LM, SE.vary.LM)
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(NEmodels.loyo, SEmodels.loyo, file = "loyo_models.rda")


