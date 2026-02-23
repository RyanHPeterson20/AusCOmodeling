
#alternative LASSO models (repeats lasso_models.R but with alternate response and predictor setups)

#notes:
## specifically shows the important of splitting DMI into WTIO and ETIO

#TODO: confirm our previous assessment of issues with DMI getting dropped in non-fixed term models.


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

SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)
SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

##-- main --##

#SE Aus `Early` (Group 1)
y.1 <- as.numeric(SE.resp[[1]]) #co response
X.1 <- cbind(as.matrix(SE.pred[[1]][ ,c(1:104, 209:364)])) #preds with DMI

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

#SE Aus `Peak` (Group 2)
y.2 <- as.numeric(SE.resp[[2]]) #co response
X.2 <- cbind(as.matrix(SE.pred[[2]][ ,c(1:104, 209:364)])) #preds with DMI

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

#SE Aus `Late` (Group 3)
y.3 <- as.numeric(SE.resp[[3]]) #co response
X.3 <- cbind(as.matrix(SE.pred[[3]][ ,c(1:104, 209:364)])) #preds with DMI

SE3.ramp <- RAMP(X = X.3, y = y.3,
                 penalty = "LASSO",
                 tune = "BIC",
                 n.lambda = 500)

SE3.refit <- refit_ramp(SE3.ramp, X.3)

#lm refit
lm.data.3 <- as.data.frame(cbind(y.3, X.3))
names(lm.data.3)[1] <- "co"

SE3.lm <- lm(formula(SE3.refit), lm.data.3)

SEmodels <- list(SE1.lm, SE2.lm, SE3.lm)
SErefits <- list(SE1.refit, SE2.refit, SE3.refit)


## ---- LOYO Models ---- ##

#Train/Test Setup
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
#for (i in 1:length(seasons)) {
for (i in 19) {
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
    X.train <- cbind(as.matrix(train.pred[[j]][ ,c(1:104, 209:364)])) 
    
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

SE.base.lm <- list(SE1.lm, SE2.lm, SE3.lm)

SEmodels.dmi <- list(SE.base.lm, SE.const.LM, SE.vary.LM)
setwd("~/CO_AUS/AusCOmodeling/Data") 
save(SEmodels.dmi, file = "dmi_models.rda")


coef(SE1.lm)
coef(SE.const.LM$`2019-2020`[[1]])
coef(SE.vary.LM$`2019-2020`[[1]])

summary(SE1.lm)
summary(SE.vary.LM$`2019-2020`[[1]])

coef(SE2.lm)
coef(SE.const.LM$`2019-2020`[[2]])
coef(SE.vary.LM$`2019-2020`[[2]])

summary(SE2.lm)
summary(SE.const.LM$`2019-2020`[[2]])
summary(SE.vary.LM$`2019-2020`[[2]])

coef(SE3.lm)
coef(SE.const.LM$`2019-2020`[[3]])
coef(SE.vary.LM$`2019-2020`[[3]])

summary(SE3.lm)
summary(SE.vary.LM$`2019-2020`[[3]])

