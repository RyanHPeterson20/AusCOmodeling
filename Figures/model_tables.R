##output models coeffs and info (as tables)


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/validation_refits_new.rda") #updated RMSE and Predictions (w/ intervals)

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

#base/full data models
SEmodels[[1]]$coefficients #early
summary(SEmodels[[1]])
SEmodels[[2]]$coefficients #peak
summary(SEmodels[[2]])
SEmodels[[3]]$coefficients #late
summary(SEmodels[[3]])


#2001/2002
#early
SErefit.new[[2]]$`2001-2002`[[1]]$coefficients
summary(SErefit.new[[2]]$`2001-2002`[[1]])
SErefit.new[[3]]$`2001-2002`[[1]]$coefficients
summary(SErefit.new[[3]]$`2001-2002`[[1]])
#peak
SErefit.new[[2]]$`2001-2002`[[2]]$coefficients
summary(SErefit.new[[2]]$`2001-2002`[[2]])
SErefit.new[[3]]$`2001-2002`[[2]]$coefficients
summary(SErefit.new[[3]]$`2001-2002`[[2]])
#late
SErefit.new[[2]]$`2001-2002`[[3]]$coefficients
summary(SErefit.new[[2]]$`2001-2002`[[3]])
SErefit.new[[3]]$`2001-2002`[[3]]$coefficients
summary(SErefit.new[[3]]$`2001-2002`[[3]])


#2002/2003
#early
SErefit.new[[2]]$`2002-2003`[[1]]$coefficients
summary(SErefit.new[[2]]$`2002-2003`[[1]])
SErefit.new[[3]]$`2002-2003`[[1]]$coefficients
summary(SErefit.new[[3]]$`2002-2003`[[1]])
#peak
SErefit.new[[2]]$`2002-2003`[[2]]$coefficients
summary(SErefit.new[[2]]$`2002-2003`[[2]])
SErefit.new[[3]]$`2002-2003`[[2]]$coefficients
summary(SErefit.new[[3]]$`2002-2003`[[2]])
#late
SErefit.new[[2]]$`2002-2003`[[3]]$coefficients
summary(SErefit.new[[2]]$`2002-2003`[[3]])
SErefit.new[[3]]$`2002-2003`[[3]]$coefficients
summary(SErefit.new[[3]]$`2002-2003`[[3]])



