#coefficient/interaction plots

## (these plots are unique enough to use their own .R file)

#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
#load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)
load("Data/validation_refits_wo2019.rda") #RMSE/Preds/Models w/o 2019/2020 data

#load functions
source("Functions/coef_int_functions.R")


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



#SEmodels
SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#SEmodels.loyo
#SE.const.LM <- SEmodels.loyo[[2]]
#SE.vary.LM <- SEmodels.loyo[[3]]

#extract linear models
## constant/varying model set-up
const.early <- lapply( SErefit.new[[2]], function(x) x[[1]])
const.peak <- lapply( SErefit.new[[2]], function(x) x[[2]]) 
const.late <- lapply( SErefit.new[[2]], function(x) x[[3]]) 

vary.early <- lapply( SErefit.new[[3]], function(x) x[[1]])
vary.peak <- lapply( SErefit.new[[3]], function(x) x[[2]]) 
vary.late <- lapply( SErefit.new[[3]], function(x) x[[3]]) 

#varying models w/o 2019/2020
vary.early.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[1]])
vary.peak.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[2]]) 
vary.late.wo2019 <- lapply( SErefit.wo2019[[3]], function(x) x[[3]]) 

## ---- Coeff/Interaction Figures ---- ##
## setup
SE1.coef <- coef(SE1.lm)
SE2.coef <- coef(SE2.lm)
SE3.coef <- coef(SE3.lm)



#varying models w.o 2019/2020
#early models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs1 <- list(
    base  = SE1.coef,
    const = coef(vary.early.wo2019[[i]]), 
    vary  = coef(vary.early[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_early_", season.years[i], ".png"), width = 3600, height = 3600, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs1,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Early Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}


#peak models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs2 <- list(
    base  = SE2.coef,
    const = coef(vary.peak.wo2019[[i]]), 
    vary  = coef(vary.peak[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_peak_", season.years[i], ".png"), width = 3600, height = 3600, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs2,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Peak Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}


#late models
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp/New")
for (i in 1:20) {
  coefs3 <- list(
    base  = SE3.coef,
    const = coef(vary.late.wo2019[[i]]), 
    vary  = coef(vary.late[[i]])  
  )
  
  png(filename = paste0("new_SEcoefs_late_", season.years[i], ".png"), width = 3600, height = 3600, res = 300)
  plot_lagged_coef_panels(
    coefs_named_list = coefs3,
    vars_order = c("nino","wtio", "etio", "tsa","aao", "olr"),  # include OLR panel
    coef_range = c(-5, 5),
    main_title = paste0("Late Fire Season (", seasons[i], " Withheld)"),   
    quad_y_jitter = 0.004,
    model_cols = c(base="forestgreen", const="royalblue3", vary="darkorange2"))
  dev.off()
}



#2001/2002
#early 
SE1.constcoef <- coef(SE.const.LM$`2001-2002`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2001-2002`[[1]])

i <- 1

coefs1 <- list(
  base  = SE1.coef,
  const = coef(vary.early.wo2019[[1]]), 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2001-2002`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2001-2002`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()

#late
SE3.constcoef <- coef(SE.const.LM$`2001-2002`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2001-2002`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2001.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2001/2002 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2002/2003
#early 
SE1.constcoef <- coef(SE.const.LM$`2002-2003`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2002-2003`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2002-2003`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2002-2003`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2002-2003`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2002-2003`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2002.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2002/2003 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2003/2004
#early 
SE1.constcoef <- coef(SE.const.LM$`2003-2004`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2003-2004`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2003-2004`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2003-2004`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2003-2004`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2003-2004`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2003.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2003/2004 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2004/2005
#early 
SE1.constcoef <- coef(SE.const.LM$`2004-2005`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2004-2005`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2004-2005`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2004-2005`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2004-2005`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2004-2005`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2004.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2004/2005 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2005/2006
#early 
SE1.constcoef <- coef(SE.const.LM$`2005-2006`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2005-2006`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2005-2006`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2005-2006`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2005-2006`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2005-2006`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2005.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2005/2006 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2006/2007
#early 
SE1.constcoef <- coef(SE.const.LM$`2006-2007`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2006-2007`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2006-2007`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2006-2007`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2006-2007`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2006-2007`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2006.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2006/2007 Withheld)",
  quad_y_jitter = 0.005)
dev.off()



#2007/2008
#early 
SE1.constcoef <- coef(SE.const.LM$`2007-2008`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2007-2008`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2007-2008`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2007-2008`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2007-2008`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2007-2008`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2007.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2007/2008 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2008/2009
#early 
SE1.constcoef <- coef(SE.const.LM$`2008-2009`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2008-2009`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2008-2009`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2008-2009`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2008-2009`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2008-2009`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2008.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2008/2009 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2009/2010
#early 
SE1.constcoef <- coef(SE.const.LM$`2009-2010`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2009-2010`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2009-2010`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2009-2010`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2009-2010`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2009-2010`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2009.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2009/2010 Withheld)",
  quad_y_jitter = 0.005)
dev.off()



#2010/2011
#early 
SE1.constcoef <- coef(SE.const.LM$`2010-2011`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2010-2011`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2010-2011`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2010-2011`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2010-2011`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2010-2011`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2010.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2010/2011 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2011/2012
#early 
SE1.constcoef <- coef(SE.const.LM$`2011-2012`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2011-2012`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2011-2012`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2011-2012`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2011-2012`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2011-2012`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2011.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2011/2012 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2012/2013
#early 
SE1.constcoef <- coef(SE.const.LM$`2012-2013`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2012-2013`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2012-2013`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2012-2013`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2012-2013`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2012-2013`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2012.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2012/2013 Withheld)",
  quad_y_jitter = 0.005)
dev.off()

#2013/2014
#early 
SE1.constcoef <- coef(SE.const.LM$`2013-2014`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2013-2014`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2013-2014`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2013-2014`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2013-2014`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2013-2014`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2013.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2013/2014 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2014/2015
#early 
SE1.constcoef <- coef(SE.const.LM$`2014-2015`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2014-2015`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2014-2015`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2014-2015`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2014-2015`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2014-2015`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2014.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2014/2015 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2015/2016
#early 
SE1.constcoef <- coef(SE.const.LM$`2015-2016`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2015-2016`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2015-2016`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2015-2016`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2015-2016`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2015-2016`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2015.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2015/2016 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2016/2017
#early 
SE1.constcoef <- coef(SE.const.LM$`2016-2017`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2016-2017`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2016-2017`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2016-2017`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2016-2017`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2016-2017`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2016.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2016/2017 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2017/2018
#early 
SE1.constcoef <- coef(SE.const.LM$`2017-2018`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2017-2018`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2017-2018`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2017-2018`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2017-2018`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2017-2018`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2017.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2017/2018 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2018/2019
#early 
SE1.constcoef <- coef(SE.const.LM$`2018-2019`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2018-2019`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2018-2019`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2018-2019`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2018-2019`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2018-2019`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2018.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2018/2019 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2019/2020
#early 
SE1.constcoef <- coef(SE.const.LM$`2019-2020`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2019-2020`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2019-2020`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2019-2020`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2019-2020`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2019-2020`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2019.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2019/2020 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#2020/2021
#early 
SE1.constcoef <- coef(SE.const.LM$`2020-2021`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2020-2021`[[1]])

coefs1 <- list(
  base  = SE1.coef,
  #const = SE1.constcoef, 
  vary  = SE1.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_early_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs1,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Early Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#peak
SE2.constcoef <- coef(SE.const.LM$`2020-2021`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2020-2021`[[2]])

coefs2 <- list(
  base  = SE2.coef,
  #const = SE2.constcoef, 
  vary  = SE2.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_peak_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs2,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Peak Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()


#late
SE3.constcoef <- coef(SE.const.LM$`2020-2021`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2020-2021`[[3]])

coefs3 <- list(
  base  = SE3.coef,
  #const = SE3.constcoef, 
  vary  = SE3.varycoef  
)

setwd("~/CO_AUS/AusCOmodeling/Figures/Examples/Temp")
png(filename = "SEcoefs_late_2020.png", width = 3600, height = 3600, res = 300)
plot_lagged_coef_panels(
  coefs_named_list = coefs3,
  vars_order = c("nino","wtio", "etio", "tsa","aao","olr"),  # include OLR panel
  coef_range = c(-5, 5),
  main_title = "Late Fire Season (2020/2021 Withheld)",
  quad_y_jitter = 0.005)
dev.off()




###

