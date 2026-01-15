##updates to lasso_models.R


#currently only SE Aus 


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
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)

SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

## --- main --- ##


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

#summary(SE1.lm)

#SE Aus Peak (Group 2)
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

#summary(SE2.lm)

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
SEmodels <- list(SE1.lm, SE2.lm, SE3.lm)
SErefits <- list(SE1.refit, SE2.refit, SE3.refit)

#setwd("~/CO_AUS/AusCOmodeling/Data") 
#save(NEmodels, SEmodels, 
#     NErefits, SErefits, file = "base_RAMPmodels.rda")


