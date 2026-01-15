##output models coeffs and info (as tables)


#libraries
suppressMessages( library(scales)) #for adjusting opacity
suppressMessages( library(fields)) #for envelope plot
suppressMessages( library(grid)) #table/grid setup
suppressMessages( library(gridExtra))

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


#function setup
lm_card_grob <- function(fit,
                         border = "#4C78A8",
                         fill   = "#E8F1FB",
                         title  = "Est (Std. Error)",
                         digits = 1,
                         fontfamily = "mono") {
  sm <- summary(fit)
  co <- sm$coefficients
  
  # Format coefficient rows
  term <- rownames(co)
  est  <- round(co[, "Estimate"], digits)
  se   <- round(co[, "Std. Error"], digits)
  
  # Make fixed-width lines (monospace) that line up nicely
  term_w <- max(nchar(term))
  est_w  <- max(nchar(format(est, trim = TRUE)))
  lines <- sprintf(
    paste0("%-", term_w, "s  %", est_w, "s (%s)"),
    term,
    format(est, trim = TRUE),
    format(se, trim = TRUE)
  )
  
  # Footer stats
  ## temp footer: (change later)
  ar2     <- sm$adj.r.squared
  nterms  <- nrow(co)
  
  footer <- c(
    "",
    sprintf("Adjusted R-squared: %.2f", ar2),
    sprintf("Number of terms: %d", nterms)
  )
  
  # Assemble full text block
  text_block <- paste(c(title, lines, footer), collapse = "\n")
  
  grobTree(
    rectGrob(gp = gpar(col = border, fill = fill, lwd = 3)),
    textGrob(
      text_block,
      x = unit(0.04, "npc"), y = unit(0.96, "npc"),
      just = c("left", "top"),
      gp = gpar(fontfamily = fontfamily, fontsize = 11, col = "black")
    )
  )
}  
  


#base/full data models
fit1 <- SEmodels[[1]] #early
fit2 <- SEmodels[[2]] #peak
fit3 <- SEmodels[[3]] #late


#test table
card1 <- lm_card_grob(fit1, border = "forestgreen", fill = alpha("springgreen3", 0.1))
card2 <- lm_card_grob(fit2, border = "forestgreen", fill = alpha("springgreen4", 0.2))
card3 <- lm_card_grob(fit3, border = "forestgreen", fill = alpha("springgreen3", 0.1))

grid.arrange(card1, card2, card3, ncol = 3)


#2001/2002
## constant model
#early
fit1.const.2001 <- SErefit.new[[2]]$`2001-2002`[[1]]
#peak
fit2.const.2001 <- SErefit.new[[2]]$`2001-2002`[[2]]
#late
fit3.const.2001 <- SErefit.new[[2]]$`2001-2002`[[3]]

#update cards
card1.const.2001 <- lm_card_grob(fit1.const.2001, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2001 <- lm_card_grob(fit2.const.2001, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2001 <- lm_card_grob(fit3.const.2001, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2001, card2.const.2001, card3.const.2001, ncol = 3)


## varying
#early
fit1.vary.2001 <- SErefit.new[[3]]$`2001-2002`[[1]]
#peak
fit2.vary.2001 <- SErefit.new[[3]]$`2001-2002`[[2]]
#late
fit3.vary.2001 <- SErefit.new[[3]]$`2001-2002`[[3]]

#update cards
card1.vary.2001 <- lm_card_grob(fit1.vary.2001, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2001 <- lm_card_grob(fit2.vary.2001, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2001 <- lm_card_grob(fit3.vary.2001, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2001, card2.vary.2001, card3.vary.2001, ncol = 3)


#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2001.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2001, card2.const.2001, card3.const.2001,
             card1.vary.2001, card2.vary.2001, card3.vary.2001, ncol = 3)
dev.off()

#2002/2003
## constant model
#early
fit1.const.2002 <- SErefit.new[[2]]$`2002-2003`[[1]]
#peak
fit2.const.2002 <- SErefit.new[[2]]$`2002-2003`[[2]]
#late
fit3.const.2002 <- SErefit.new[[2]]$`2002-2003`[[3]]

#update cards
card1.const.2002 <- lm_card_grob(fit1.const.2002, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2002 <- lm_card_grob(fit2.const.2002, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2002 <- lm_card_grob(fit3.const.2002, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2002, card2.const.2002, card3.const.2002, ncol = 3)

## varying
#early
fit1.vary.2002 <- SErefit.new[[3]]$`2002-2003`[[1]]
#peak
fit2.vary.2002 <- SErefit.new[[3]]$`2002-2003`[[2]]
#late
fit3.vary.2002 <- SErefit.new[[3]]$`2002-2003`[[3]]

#update cards
card1.vary.2002 <- lm_card_grob(fit1.vary.2002, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2002 <- lm_card_grob(fit2.vary.2002, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2002 <- lm_card_grob(fit3.vary.2002, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2002, card2.vary.2002, card3.vary.2002, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2002.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2002, card2.const.2002, card3.const.2002,
             card1.vary.2002, card2.vary.2002, card3.vary.2002, ncol = 3)
dev.off()



#2003/2004
## constant model
#early
fit1.const.2003 <- SErefit.new[[2]]$`2003-2004`[[1]]
#peak
fit2.const.2003 <- SErefit.new[[2]]$`2003-2004`[[2]]
#late
fit3.const.2003 <- SErefit.new[[2]]$`2003-2004`[[3]]

#update cards
card1.const.2003 <- lm_card_grob(fit1.const.2003, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2003 <- lm_card_grob(fit2.const.2003, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2003 <- lm_card_grob(fit3.const.2003, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2003, card2.const.2003, card3.const.2003, ncol = 3)

## varying
#early
fit1.vary.2003 <- SErefit.new[[3]]$`2003-2004`[[1]]
#peak
fit2.vary.2003 <- SErefit.new[[3]]$`2003-2004`[[2]]
#late
fit3.vary.2003 <- SErefit.new[[3]]$`2003-2004`[[3]]

#update cards
card1.vary.2003 <- lm_card_grob(fit1.vary.2003, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2003 <- lm_card_grob(fit2.vary.2003, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2003 <- lm_card_grob(fit3.vary.2003, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2003, card2.vary.2003, card3.vary.2003, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2003.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2003, card2.const.2003, card3.const.2003,
             card1.vary.2003, card2.vary.2003, card3.vary.2003, ncol = 3)
dev.off()



#2004/2005
## constant model
#early
fit1.const.2004 <- SErefit.new[[2]]$`2004-2005`[[1]]
#peak
fit2.const.2004 <- SErefit.new[[2]]$`2004-2005`[[2]]
#late
fit3.const.2004 <- SErefit.new[[2]]$`2004-2005`[[3]]

#update cards
card1.const.2004 <- lm_card_grob(fit1.const.2004, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2004 <- lm_card_grob(fit2.const.2004, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2004 <- lm_card_grob(fit3.const.2004, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2004, card2.const.2004, card3.const.2004, ncol = 3)

## varying
#early
fit1.vary.2004 <- SErefit.new[[3]]$`2004-2005`[[1]]
#peak
fit2.vary.2004 <- SErefit.new[[3]]$`2004-2005`[[2]]
#late
fit3.vary.2004 <- SErefit.new[[3]]$`2004-2005`[[3]]

#update cards
card1.vary.2004 <- lm_card_grob(fit1.vary.2004, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2004 <- lm_card_grob(fit2.vary.2004, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2004 <- lm_card_grob(fit3.vary.2004, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2004, card2.vary.2004, card3.vary.2004, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2004.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2004, card2.const.2004, card3.const.2004,
             card1.vary.2004, card2.vary.2004, card3.vary.2004, ncol = 3)
dev.off()



#2005/2006
## constant model
#early
fit1.const.2005 <- SErefit.new[[2]]$`2005-2006`[[1]]
#peak
fit2.const.2005 <- SErefit.new[[2]]$`2005-2006`[[2]]
#late
fit3.const.2005 <- SErefit.new[[2]]$`2005-2006`[[3]]

#update cards
card1.const.2005 <- lm_card_grob(fit1.const.2005, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2005 <- lm_card_grob(fit2.const.2005, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2005 <- lm_card_grob(fit3.const.2005, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2005, card2.const.2005, card3.const.2005, ncol = 3)

## varying
#early
fit1.vary.2005 <- SErefit.new[[3]]$`2005-2006`[[1]]
#peak
fit2.vary.2005 <- SErefit.new[[3]]$`2005-2006`[[2]]
#late
fit3.vary.2005 <- SErefit.new[[3]]$`2005-2006`[[3]]

#update cards
card1.vary.2005 <- lm_card_grob(fit1.vary.2005, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2005 <- lm_card_grob(fit2.vary.2005, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2005 <- lm_card_grob(fit3.vary.2005, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2005, card2.vary.2005, card3.vary.2005, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2005.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2005, card2.const.2005, card3.const.2005,
             card1.vary.2005, card2.vary.2005, card3.vary.2005, ncol = 3)
dev.off()


#2006/2007
## constant model
#early
fit1.const.2006 <- SErefit.new[[2]]$`2006-2007`[[1]]
#peak
fit2.const.2006 <- SErefit.new[[2]]$`2006-2007`[[2]]
#late
fit3.const.2006 <- SErefit.new[[2]]$`2006-2007`[[3]]

#update cards
card1.const.2006 <- lm_card_grob(fit1.const.2006, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2006 <- lm_card_grob(fit2.const.2006, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2006 <- lm_card_grob(fit3.const.2006, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2006, card2.const.2006, card3.const.2006, ncol = 3)

## varying
#early
fit1.vary.2006 <- SErefit.new[[3]]$`2006-2007`[[1]]
#peak
fit2.vary.2006 <- SErefit.new[[3]]$`2006-2007`[[2]]
#late
fit3.vary.2006 <- SErefit.new[[3]]$`2006-2007`[[3]]

#update cards
card1.vary.2006 <- lm_card_grob(fit1.vary.2006, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2006 <- lm_card_grob(fit2.vary.2006, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2006 <- lm_card_grob(fit3.vary.2006, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2006, card2.vary.2006, card3.vary.2006, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2006.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2006, card2.const.2006, card3.const.2006,
             card1.vary.2006, card2.vary.2006, card3.vary.2006, ncol = 3)
dev.off()

#2007/2008
## constant model
#early
fit1.const.2007 <- SErefit.new[[2]]$`2007-2008`[[1]]
#peak
fit2.const.2007 <- SErefit.new[[2]]$`2007-2008`[[2]]
#late
fit3.const.2007 <- SErefit.new[[2]]$`2007-2008`[[3]]

#update cards
card1.const.2007 <- lm_card_grob(fit1.const.2007, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2007 <- lm_card_grob(fit2.const.2007, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2007 <- lm_card_grob(fit3.const.2007, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2007, card2.const.2007, card3.const.2007, ncol = 3)

## varying
#early
fit1.vary.2007 <- SErefit.new[[3]]$`2007-2008`[[1]]
#peak
fit2.vary.2007 <- SErefit.new[[3]]$`2007-2008`[[2]]
#late
fit3.vary.2007 <- SErefit.new[[3]]$`2007-2008`[[3]]

#update cards
card1.vary.2007 <- lm_card_grob(fit1.vary.2007, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2007 <- lm_card_grob(fit2.vary.2007, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2007 <- lm_card_grob(fit3.vary.2007, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2007, card2.vary.2007, card3.vary.2007, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2007.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2007, card2.const.2007, card3.const.2007,
             card1.vary.2007, card2.vary.2007, card3.vary.2007, ncol = 3)
dev.off()



#2008/2009
## constant model
#early
fit1.const.2008 <- SErefit.new[[2]]$`2008-2009`[[1]]
#peak
fit2.const.2008 <- SErefit.new[[2]]$`2008-2009`[[2]]
#late
fit3.const.2008 <- SErefit.new[[2]]$`2008-2009`[[3]]

#update cards
card1.const.2008 <- lm_card_grob(fit1.const.2008, border = "magenta3", fill = alpha("orchid3", 0.1))
card2.const.2008 <- lm_card_grob(fit2.const.2008, border = "magenta3", fill = alpha("orchid4", 0.2))
card3.const.2008 <- lm_card_grob(fit3.const.2008, border = "magenta3", fill = alpha("orchid3", 0.1))

grid.arrange(card1.const.2008, card2.const.2008, card3.const.2008, ncol = 3)

## varying
#early
fit1.vary.2008 <- SErefit.new[[3]]$`2008-2009`[[1]]
#peak
fit2.vary.2008 <- SErefit.new[[3]]$`2008-2009`[[2]]
#late
fit3.vary.2008 <- SErefit.new[[3]]$`2008-2009`[[3]]

#update cards
card1.vary.2008 <- lm_card_grob(fit1.vary.2008, border = "darkorange2", fill = alpha("orange2", 0.1))
card2.vary.2008 <- lm_card_grob(fit2.vary.2008, border = "darkorange2", fill = alpha("orange3", 0.2))
card3.vary.2008 <- lm_card_grob(fit3.vary.2008, border = "darkorange2", fill = alpha("orange2", 0.1))

grid.arrange(card1.vary.2008, card2.vary.2008, card3.vary.2008, ncol = 3)

#final plot
setwd("~/CO_AUS/AusCOmodeling/Figures/Examples")
png(filename = "model_tables_2008.png", width = 3500, height = 4000, res = 275)
grid.arrange(card1, card2, card3,
             card1.const.2008, card2.const.2008, card3.const.2008,
             card1.vary.2008, card2.vary.2008, card3.vary.2008, ncol = 3)
dev.off()

