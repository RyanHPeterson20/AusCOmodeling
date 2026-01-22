## work on... (see notes)

#1. Get model info (terms and coefficients) for the "interesting" seasons 
#2....


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(grid)) #table/grid setup and lines between plots
suppressMessages( library(gridExtra))

#data
#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

#functions
source("Functions/coef_int_functions.R")
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

#get single season models for peak 2019/2020 with additional withheld:
## 2001/2002, 2010/2011, and 2011/2012 (along with 2005/2006 and 2016/2017)

#peak all-data models
SE2.lm <- SEmodels[[2]] 



#model for predicting 2019/2020 (generalize as needed)
SE.lm.2019 <- SErefit.wo.years$SE.vary.lm$`2019-2020`
#get all peak models
SE2.lm.2019 <- lapply(SE.lm.2019, function(z) z[[2]])

#get only 2019/2020 withheld
SE.lm.wo2019 <- SE2.lm.2019[[19]]


## ---- Coef/Interaction Figures ---- ##
## setup
#SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
#SE3.coef <- coef(SE3.lm)


#peak models
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    #const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(SE2.lm.2019[[i]])  
  )
  
  png(filename = paste0("SEcoefs_2019peak_withheld_", season.years[i], ".png"), width = 3400, height = 4400, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak 2019/2020 Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="darkorange2", vary="royalblue3"))
  dev.off()
}


#get tables
card1 <- lm_card_grob(SE2.lm, border = "forestgreen", fill = alpha("springgreen4", 0.2))
card2 <- lm_card_grob(SE.lm.wo2019, border = "darkorange2", fill = alpha("orange3", 0.2))

grid.arrange(card1, card2, ncol = 2)


#peak plot
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/Temp_Figures")
for (i in 1:20) {
  png(filename = paste0("new_model_tables_", season.years[i], ".png"), width = 3500, height = 1500, res = 275)
  
  card2.vary.wo2019 <- lm_card_grob(SE2.lm.2019[[i]], border = "royalblue3", fill = alpha("steelblue4", 0.2))

  
  grid.arrange(card1, card2, card2.vary.wo2019, ncol = 3)
  
  dev.off()
}

#TODO: isolate the weeks that each model looks at when "training" so that we can look at everything.
#note, get each year (52 weeks preceding a group) at a time then combine them later.

#pred data (kinda hacky, fix later if needed)

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14


SEAus.lag$`Week  51`

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


SEpreds.peak.nino <- cbind(SEpreds.peak2.nino, SEpreds.peak51.nino)
SEpreds.peak.wtio <- cbind(SEpreds.peak2.wtio, SEpreds.peak51.wtio)
SEpreds.peak.etio <- cbind(SEpreds.peak2.etio, SEpreds.peak51.etio)
SEpreds.peak.tsa <- cbind(SEpreds.peak2.tsa, SEpreds.peak51.tsa)
SEpreds.peak.aao <- cbind(SEpreds.peak2.aao, SEpreds.peak51.aao)
SEpreds.peak.olr <- cbind(SEpreds.peak2.olr, SEpreds.peak51.olr)



#start with frequency histograms
par(mfrow = c(3,2))
hist(as.matrix(SEpreds.peak.nino), freq = FALSE, main = "Nino - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.wtio), freq = FALSE, main = "WTIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.etio), freq = FALSE, main = "ETIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.tsa), freq = FALSE, main = "TSA - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.aao), freq = FALSE, main = "SAM (AAO) - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.olr), freq = FALSE, main = "OLR - Peak Group", xlab = "Anomaly")


