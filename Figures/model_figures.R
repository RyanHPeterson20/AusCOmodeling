
#model figures

#figure order:
#1. prediction (2019-2020)
#2. Coefficient/interaction plots

#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity


#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits
load("Data/preds_2019.rda") #2019 predictions 


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

#get 2019/2020 SE Aus prediction and 95% PI for base, const, and vary (3 figures)
#setup
pred.base.fit <- c(pred.base.early$fit[,1], pred.base.mid$fit[,1], pred.base.late$fit[,1])
pred.base.lwr <- c(pred.base.early$fit[,2], pred.base.mid$fit[,2], pred.base.late$fit[,2])
pred.base.upr <- c(pred.base.early$fit[,3], pred.base.mid$fit[,3], pred.base.late$fit[,3])

pred.const.fit <- c(pred.const.early$fit[,1], pred.const.mid$fit[,1], pred.const.late$fit[,1])
pred.const.lwr <- c(pred.const.early$fit[,2], pred.const.mid$fit[,2], pred.const.late$fit[,2])
pred.const.upr <- c(pred.const.early$fit[,3], pred.const.mid$fit[,3], pred.const.late$fit[,3])

pred.vary.fit <- c(pred.vary.early$fit[,1], pred.vary.mid$fit[,1], pred.vary.late$fit[,1])
pred.vary.lwr <- c(pred.vary.early$fit[,2], pred.vary.mid$fit[,2], pred.vary.late$fit[,2])
pred.vary.upr <- c(pred.vary.early$fit[,3], pred.vary.mid$fit[,3], pred.vary.late$fit[,3])

base.range <- range(SE.2019.true, pred.base.fit, pred.base.lwr, pred.base.upr)
const.range <- range(SE.2019.true, pred.const.fit, pred.const.lwr, pred.const.upr)
vary.range <- range(SE.2019.true, pred.vary.fit, pred.vary.lwr, pred.vary.upr)
all.range <- range(base.range, const.range, vary.range)



setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEpreds_2019_new.png", width = 2750, height = 3000, res = 300)
par(mfrow = c(3, 1), oma = c(2.5, 1, 1, 1), mar = c(2.5, 4, 4, 2))
#par(mar = c(2,4,2,2), oma = c(2,2,1,0), mgp = c(4,1,0))
#full model
plot(1:29, SE.2019.true, type = "l", ylim = all.range, axes = FALSE, lwd = 1.52,
     ylab = "", xlab = "", xlim = c(1.95, 28.05))
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.2)
axis(2, cex.axis = 1.25)  
lines(1:29, pred.base.fit, lty = 4, lwd = 1.75)
lines(1:29, pred.base.lwr, lty = 2, lwd = 1.75, col = "royalblue3")
lines(1:29, pred.base.upr, lty = 2, lwd = 1.75, col = "firebrick3")
abline(h=0, lty =3)
abline(v = c(13.5, 17.5), lty = 3)
title("Full Model", adj = 0, cex.main = 1.25)
legend("topright", 
       legend = c("True",
                  "Prediction",
                  "Upper 95% PI",
                  "Lower 95% PI"),
       lty = c(1,4,2,2), 
       lwd = 1.5,
       cex = 1.25,
       col = c("black", "black", 
               "firebrick3", "royalblue3"),
       xpd = TRUE)

#const model
plot(1:29, SE.2019.true, type = "l",  ylim = all.range, axes = FALSE, lwd = 1.52,
     ylab = "", xlab = "", xlim = c(1.95, 28.05), cex.lab = 1.5)
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.2)
axis(2, cex.axis = 1.25)  
lines(1:29, pred.const.fit, lty = 4, lwd = 1.75)
lines(1:29, pred.const.lwr, lty = 2, lwd = 1.75, col = "royalblue3")
lines(1:29, pred.const.upr, lty = 2, lwd = 1.75, col = "firebrick3")
abline(h=0, lty =3)
abline(v = c(13.5, 17.5), lty = 3)
title("Fixed Model", adj = 0, cex.main = 1.25)

#vary model
plot(1:29, SE.2019.true, type = "l", ylim = all.range, axes = FALSE, lwd = 1.52,
     ylab = "", xlab = "", xlim = c(1.95, 28.05), cex.lab = 1.5)
box()
axis(1, labels = season.weeks, at = 1:29, cex.axis = 1.2)
axis(2, cex.axis = 1.25)  
lines(1:29, pred.vary.fit, lty = 4, lwd = 1.75)
lines(1:29, pred.vary.lwr, lty = 2, lwd = 1.75, col = "royalblue3")
lines(1:29, pred.vary.upr, lty = 2, lwd = 1.75, col = "firebrick3")
abline(h=0, lty =3)
abline(v = c(13.5, 17.5), lty = 3)
title("Non-Fixed Model", adj = 0, cex.main = 1.25)

mtext("CO Anomaly (ppb)", side = 2, outer = TRUE, padj = 0.5)
mtext("Week", side = 1, outer = TRUE, adj = 0.5)
dev.off()



## ---- Coeff/Interaction Figures ---- ##
## setup



SE2.coef <- coef(SE2.lm)
SE2.constcoef <- coef(SE.const.LM$`2019-2020`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2019-2020`[[2]])




## SE Aus Middle (Group 2)
SE2.coef
SE2.constcoef
SE2.varycoef

#non-OLR plot, LASSO does not select for OLR in any LOYO variation



setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEcoefs_mid.png", width = 3000, height = 3000, res = 300)
layout(matrix(c(1, 5,
                2, 5,
                3, 5,
                4, 5), ncol = 2, byrow = TRUE),
       widths = c(1.75, 1.25), heights = c(1, 1, 1, 1, 1))

par(oma = c(1, 1, 1, 1))

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


# --- Plot 1: Nino ---
par(mar = c(4, 4, 2, 1))

plot(SE2_ninolag, SE2_ninocoef, pch = 22, 
     col = "grey4", bg =  alpha("green4",.95), cex = 2,
     xlim = c(1,52), cex.axis = 1.2, 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_ninolag, SE22_ninocoef, pch = 22, col = "black",
       bg =  alpha("chartreuse2",.65) , cex = 2)
abline(h = 0, lty = 2)
title("Ni\u00f1o", adj = 0, cex.main = 1.5)

## --- Nino Interaction
## nino_lag40:etio_lag7
## base
links[[1]] <- list(
  y_val = SE2_ninocoef[1],
  from_x = grconvertX(SE2_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE2_ninocoef[1], from = "user", to = "ndc")
)

## constant
links[[2]] <- list(
  y_val = SE22_ninocoef[1],
  from_x = grconvertX(SE22_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE22_ninocoef[1], from = "user", to = "ndc")
)

# --- Plot 2: WTIO & ETIO ---
par(mar = c(4, 4, 2, 1))

plot(SE2_wtiolag-0.25, SE2_wtiocoef, pch = 22, col = "black",
     bg =  alpha("magenta4",.95) , cex = 2, cex.axis = 1.2,
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_wtiolag+0.25, SE22_wtiocoef, pch = 22, col = "black",
       bg =  alpha("palevioletred2",.65) , cex = 2)
points(SE23_wtiolag, SE23_wtiocoef, pch = 24, col = "black",
       bg =  alpha("palevioletred2",.65) , cex = 1.75)
points(SE2_etiolag, SE2_etiocoef, pch = 22, 
       col = "grey4", bg =  alpha("royalblue4",.95), cex = 2)
points(SE22_etiolag, SE22_etiocoef, pch = 22, col = "black",
       bg =  alpha("royalblue2",.65) , cex = 2)
points(SE23_etiolag, SE23_etiocoef, pch = 24, col = "black",
       bg =  alpha("royalblue2",.65) , cex = 1.75)
abline(h = 0, lty = 2)
title("WTIO & ETIO", adj = 0)

## --- ETIO Interaction
## nino_lag40:etio_lag7
## base
links[[3]] <- list(
  y_val = SE2_etiocoef[1],
  from_x = grconvertX(SE2_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE2_etiocoef[1], from = "user", to = "ndc")
)

## constant
links[[4]] <- list(
  y_val = SE22_etiocoef[1],
  from_x = grconvertX(SE22_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE22_etiocoef[1], from = "user", to = "ndc")
)

## I(etio_lag8^2) 
## vary
links[[5]] <- list(
  y_val = SE23_etiocoef[1],
  from_x = grconvertX(SE23_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE23_etiocoef[1], from = "user", to = "ndc")
)
## etio_lag8:tsa_lag31
## vary
links[[6]] <- list(
  y_val = SE23_etiocoef[1],
  from_x = grconvertX(SE23_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE23_etiocoef[1], from = "user", to = "ndc")
)

# --- Plot 3: TSA ---
par(mar = c(4, 4, 2, 1))

plot(SE2_tsalag-0.25, SE2_tsacoef, pch = 22, col = "black",
     bg =  alpha("darkorange3", 0.95), xlim = c(1,52), cex = 2,
     ylim = SEAus2_range, cex.axis = 1.2,
     xlab = "", ylab = "", cex.lab = 1.33)
points(SE22_tsalag+0.25, SE22_tsacoef, pch = 22, col = "black",
       bg =  alpha("darkgoldenrod2",.55) , cex = 2)
points(SE23_tsalag, SE23_tsacoef, pch = 24, col = "black",
       bg =  alpha("darkgoldenrod2",.65) , cex = 1.75)
abline(h = 0, lty = 2)
title("TSA", adj = 0)

## --- TSA Interaction
## etio_lag8:tsa_lag31
## vary
links[[7]] <- list(
  y_val = SE23_tsacoef[1],
  from_x = grconvertX(SE23_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE23_tsacoef[1], from = "user", to = "ndc")
)

# --- Plot 4: SAM/AAO ---
par(mar = c(4, 4, 2, 1))
plot(SE2_aaolag-0.25, SE2_aaocoef, pch = 22,
     col = "grey4",
     bg =  alpha("red3",.95), cex = 2,
     xlim = c(1,52),  cex.axis = 1.2, cex.lab = 1.33,
     ylim = SEAus2_range,
     xlab = "Lag", ylab = "")
points(SE22_aaolag+0.25, SE22_aaocoef, pch = 22, col = "black",
       bg =  alpha("coral2",.55) , cex = 2)
points(SE23_aaolag, SE23_aaocoef, pch = 24, col = "black",
       bg =  alpha("coral2",.65) , cex = 1.75)
abline(h = 0, lty = 2)
title("SAM (AAO)", adj = 0)


# --- Plot 5: Interaction Effects ---
par(mar = c(4, 4, 2, 2))

plot(SE2.coef[1], 0, type = "n", main = "", 
     ylim = c(0,1), xlim = SEAus2_range, cex = 2, cex.axis = 1.2,
     xlab = "Coefficients", cex.lab = 1.33,
     yaxt = "n",  ylab = "")
abline(v= 0, lty = 2)

#square terms
int_1 <- grconvertY(links[[5]]$from_y, from = "ndc", to = "user") #I(etio_lag8^2)  (varying terms)

#interactions
int_2 <- grconvertY(links[[1]]$from_y, from = "ndc", to = "user") # nino_lag40-> etio_lag7 #base
int_3 <- grconvertY(links[[2]]$from_y, from = "ndc", to = "user") # nino_lag40-> etio_lag7 #const
int_4 <- grconvertY(links[[3]]$from_y, from = "ndc", to = "user") # etio_lag7 -> nino_lag40 #base
int_5 <- grconvertY(links[[4]]$from_y, from = "ndc", to = "user") # etio_lag7 -> nino_lag40 #const
int_6 <- grconvertY(links[[6]]$from_y, from = "ndc", to = "user") # etio_lag8 -> tsa_lag31 #vary
int_7 <- grconvertY(links[[7]]$from_y, from = "ndc", to = "user") # tsa_lag31 -> etio_lag8 #vary

#nino_lag40:etio_lag7
## base
int_pt1 <- (int_2 + int_4)/2
segments(SE2.coef[10], int_2, SE2.coef[10], int_pt1, col = "green4", lty = 2, lwd = 1.75)
segments(SE2.coef[10], int_4, SE2.coef[10], int_pt1, col = "royalblue4", lty = 2, lwd = 1.75)
## const
int_pt2 <- (int_3 + int_5)/2
segments(SE2.constcoef[10], int_3, SE2.constcoef[10], int_pt2, col = "chartreuse2", lty = 2, lwd = 1.75)
segments(SE2.constcoef[10], int_5, SE2.constcoef[10], int_pt2, col = "royalblue2", lty = 2, lwd = 1.75)
## etio_lag8:tsa_lag31
## vary
int_pt3 <- (int_6 + int_7)/2
segments(SE2.varycoef[8], int_6, SE2.varycoef[8], int_pt3, col = "royalblue2", lty = 3, lwd = 1.75)
segments(SE2.varycoef[8], int_7, SE2.varycoef[8], int_pt3, col = "darkgoldenrod2", lty = 3, lwd = 1.75)

#interaction points
points(SE2.varycoef[7], int_1,  pch = 24, col = "grey4",
       bg =  alpha("royalblue2",.65), cex = 1.75,) 
points(SE2.coef[10], int_pt1,  pch = 22, col = "grey4",
       bg =  alpha("slategray",.95), cex = 2,) 
points(SE2.constcoef[10], int_pt2,  pch = 22, col = "grey4",
       bg =  alpha("gray",.95), cex = 2,) 
points(SE2.varycoef[8], int_pt3,  pch = 24, col = "grey4",
       bg =  alpha("gray",.95), cex = 1.75,) 

#link to x 
links[[1]]$to_x <- grconvertX(SE2.coef[10], from = "user", to = "ndc")
links[[2]]$to_x <- grconvertX(SE2.constcoef[10], from = "user", to = "ndc")
links[[3]]$to_x <- grconvertX(SE2.coef[10], from = "user", to = "ndc")
links[[4]]$to_x <- grconvertX(SE2.constcoef[10], from = "user", to = "ndc")
links[[5]]$to_x <- grconvertX(SE2.varycoef[7], from = "user", to = "ndc")
links[[6]]$to_x <- grconvertX(SE2.varycoef[8], from = "user", to = "ndc")
links[[7]]$to_x <- grconvertX(SE2.varycoef[8], from = "user", to = "ndc")

for (i in 1:length(links)) {
  links[[i]]$to_y <- links[[i]]$from_y  # same y to keep it horizontal
}

# --- Draw horizontal linking lines ---
par(xpd = NA)  # allow drawing outside plot regions
colors <- c("green4", "chartreuse2", "royalblue4", "royalblue2", "royalblue2", "royalblue2", "darkgoldenrod2")
linetypes <- c(rep(2,4), rep(3,3))


for (i in 1:length(links)) {
  grid.lines(
    x = unit(c(links[[i]]$from_x, links[[i]]$to_x), "npc"),
    y = unit(c(links[[i]]$from_y, links[[i]]$to_y), "npc"),
    gp = gpar(col = colors[i], lwd = 1.75, lty = linetypes[i])
  )
}

mtext("Coefficients", side = 2, outer = TRUE, padj = 0.5)

#add legends
par(xpd = NA)
legend("topright", inset = c(0.00, 0.00),
       title = "Ni\u00f1o", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = c(22, 22, 24),
       col = c("grey4", "grey4", "grey4"),
       pt.bg = c("green4",  "chartreuse2", "chartreuse2"),
       pt.cex = c(1.5, 1.5, 1.33))
legend("topright", inset = c(0.00, 0.09),
       title = "WTIO", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = c(22, 22, 24),
       col = c("grey4", "grey4", "grey4"),
       pt.bg = c("magenta4",  "palevioletred2", "palevioletred2"),
       pt.cex = c(1.5, 1.5, 1.33))
legend("topright", inset = c(0.00, 0.18),
       title = "ETIO", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = c(22, 22, 24),
       col = c("grey4", "grey4", "grey4"),
       pt.bg = c("royalblue4",  "royalblue2", "royalblue2"),
       pt.cex = c(1.5, 1.5, 1.33))
legend("topright", inset = c(0.00, 0.27),
       title = "TSA", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = c(22, 22, 24),
       col = c("grey4", "grey4", "grey4"),
       pt.bg = c("darkorange3",  "darkgoldenrod2", "darkgoldenrod2"),
       pt.cex = c(1.5, 1.5, 1.33))
legend("topright", inset = c(0.00, 0.36),
       title = "SAM (AAO)", cex = 1.25,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = c(22, 22, 24),
       col = c("grey4", "grey4", "grey4"),
       pt.bg = c("red3",  "coral2", "coral2"),
       pt.cex = c(1.5, 1.5, 1.33))


dev.off()
