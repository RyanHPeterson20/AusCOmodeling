
#model figures

#figure order:
#1. prediction (2019-2020)
#2. Coefficient/interaction plots

#libraries



#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits


#model review
#NE Aus Early (Group 1)
summary(NE1.lm)
summary(NE.const.LM$`2019-2020`[[1]])
summary(NE.vary.LM$`2019-2020`[[1]])

#NE Aus Middle (Group 2)
summary(NE2.lm)
summary(NE.const.LM$`2019-2020`[[2]])
summary(NE.vary.LM$`2019-2020`[[2]])

#NE Aus Late (Group 3)
summary(NE3.lm)
summary(NE.const.LM$`2019-2020`[[3]])
summary(NE.vary.LM$`2019-2020`[[3]])


#SE Aus Early (Group 1)
summary(SE1.lm)
summary(SE.const.LM$`2019-2020`[[1]])
summary(SE.vary.LM$`2019-2020`[[1]])

#SE Aus Middle (Group 2)
summary(SE2.lm)
summary(SE.const.LM$`2019-2020`[[2]])
summary(SE.vary.LM$`2019-2020`[[2]])

#SE Aus Late (Group 3)
summary(SE3.lm)
summary(SE.const.LM$`2019-2020`[[3]])
summary(SE.vary.LM$`2019-2020`[[3]])

## --- Model Predictions --- ##




## ---- Coeff/Interaction Figures ---- ##
## setup
SE2.coef <- coef(SE2.lm)
SE2.constcoef <- coef(SE.const.LM$`2019-2020`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2019-2020`[[2]])




## SE Aus Middle (Group 2)
SE2.coef
SE2.constcoef
SE2.varycoef

layout(matrix(c(1, 6,
                2, 6,
                3, 6,
                4, 6,
                5, 6), ncol = 2, byrow = TRUE),
       widths = c(1.5, 1), heights = c(1, 1, 1, 1, 1))

par(oma = c(1, 1, 3, 9))

# Store links
links <- list()

# --- Data Setup --- #
## Nino
SE2_ninolag <- c(40)
SE2_ninocoef <- SE2.coef[2]
SE22_ninolag <- c(40)
SE22_ninocoef <- SE2.constcoef[2]

## WTIO
SE2_wtiolag <- c(14,46)
SE2_wtiocoef <- SE2.coef[c(3:4)]
SE22_wtiolag <- c(14,46)
SE22_wtiocoef <- SE2.constcoef[c(3:4)]
SE23_wtiolag <- c(14)
SE23_wtiocoef <- SE2.varycoef[2]

## ETIO
SE2_etiolag <- c(7, 33)
SE2_etiocoef <- SE2.coef[c(5,6)]
SE22_etiolag <- c(7, 33)
SE22_etiocoef <- SE2.constcoef[c(5,6)]
SE23_etiolag  <- c(8)
SE23_etiocoef <- SE2.varycoef[5]

## TSA
SE2_tsalag <- c(29)
SE2_tsacoef <- SE2.coef[7]
SE22_tsalag <- c(29)
SE22_tsacoef <- SE2.constcoef[7]
SE23_tsalag <- c(31,34)
SE23_tsacoef <- SE2.varycoef[c(6,3)]

## SAM (AAO)
SE2_aaolag <- c(9, 21)
SE2_aaocoef <- SE2.coef[c(8,9)]
SE22_aaolag <- c(9, 21)
SE22_aaocoef <- SE2.constcoef[c(8,9)]
SE23_aaolag <- c(42)
SE23_aaocoef <- SE2.varycoef[4]

## OLR
#NA

# --- Range ---
SEAus2_absmax <- max(abs(range(SE2.coef,
                               SE2.constcoef,
                               SE2.varycoef)))
SEAus2_range <- c(-SEAus2_absmax, SEAus2_absmax)


#TODO: delete when done; temp row
par(mfrow = c(4, 1))

# --- Plot 1: Nino ---
par(mar = c(4, 4, 2, 1))

plot(SE2_ninolag, SE2_ninocoef, pch = 22, 
     col = "grey4", bg =  alpha("green4",.95), cex = 1.5,
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_ninolag, SE22_ninocoef, pch = 22, col = "black",
       bg =  alpha("chartreuse2",.65) , cex = 1.5)
abline(h = 0, lty = 2)
title("Nino", adj = 0)

## --- Nino Interaction
## nino_lag40:etio_lag7
## base 
## const

# --- Plot 2: WTIO & ETIO ---
par(mar = c(4, 4, 2, 1))

plot(SE2_wtiolag-0.125, SE2_wtiocoef, pch = 22, col = "black",
     bg =  alpha("magenta4",.95) , cex = 1.5, 
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_wtiolag, SE22_wtiocoef, pch = 22, col = "black",
       bg =  alpha("palevioletred2",.65) , cex = 1.5)
points(SE23_wtiolag+0.125, SE23_wtiocoef, pch = 24, col = "black",
       bg =  alpha("palevioletred2",.65) , cex = 1.33)
points(SE2_etiolag, SE2_etiocoef, pch = 22, 
       col = "grey4", bg =  alpha("royalblue4",.95), cex = 1.5)
points(SE22_etiolag, SE22_etiocoef, pch = 22, col = "black",
       bg =  alpha("royalblue2",.65) , cex = 1.5)
points(SE23_etiolag, SE23_etiocoef, pch = 24, col = "black",
       bg =  alpha("royalblue2",.65) , cex = 1.33)
abline(h = 0, lty = 2)
title("WTIO & ETIO", adj = 0)

## --- ETIO Interaction
## nino_lag40:etio_lag7
## base
## const

## I(etio_lag8^2) 
## vary
## etio_lag8:tsa_lag31
## vary


# --- Plot 3: TSA ---
par(mar = c(4, 4, 2, 1))

plot(SE2_tsalag-0.33, SE2_tsacoef, pch = 22, col = "black",
     bg =  alpha("darkorange3", 0.95), xlim = c(1,52), cex = 1.4,
     ylim = SEAus2_range,
     xlab = "", ylab = "Coefficients", cex.lab = 1.33)
points(SE22_tsalag+0.33, SE22_tsacoef, pch = 22, col = "black",
       bg =  alpha("darkgoldenrod2",.55) , cex = 1.5)
points(SE23_tsalag, SE23_tsacoef, pch = 24, col = "black",
       bg =  alpha("darkgoldenrod2",.65) , cex = 1.33)
abline(h = 0, lty = 2)
title("TSA", adj = 0)

## --- TSA Interaction
## etio_lag8:tsa_lag31
## vary


# --- Plot 4: SAM/AAO ---
par(mar = c(4, 4, 2, 1))
plot(SE2_aaolag-0.33, SE2_aaocoef, pch = 22,
     col = "grey4",
     bg =  alpha("red3",.95), cex = 1.5,
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "Lag", ylab = "")
points(SE22_aaolag+0.33, SE22_aaocoef, pch = 22, col = "black",
       bg =  alpha("coral2",.55) , cex = 1.5)
points(SE23_aaolag, SE23_aaocoef, pch = 24, col = "black",
       bg =  alpha("coral2",.65) , cex = 1.33)
abline(h = 0, lty = 2)
title("SAM (AAO)", adj = 0)


#dev.off()
