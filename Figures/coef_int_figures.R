
#coefficient/interaction plots

## (these plots are unique enough to use their own .R file)

#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity


#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits


# main setup #

#SEmodels
SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#SEmodels.loyo
SE.const.LM <- SEmodels.loyo[[2]]
SE.vary.LM <- SEmodels.loyo[[3]]


## ---- Coeff/Interaction Figures ---- ##
## setup
SE1.coef <- coef(SE1.lm)
SE1.constcoef <- coef(SE.const.LM$`2019-2020`[[1]])
SE1.varycoef <- coef(SE.vary.LM$`2019-2020`[[1]])

SE2.coef <- coef(SE2.lm)
SE2.constcoef <- coef(SE.const.LM$`2019-2020`[[2]])
SE2.varycoef <- coef(SE.vary.LM$`2019-2020`[[2]])

SE3.coef <- coef(SE3.lm)
SE3.constcoef <- coef(SE.const.LM$`2019-2020`[[3]])
SE3.varycoef <- coef(SE.vary.LM$`2019-2020`[[3]])

#TODO: get updated figure with split WTIO and ETIO
#notes for updated figures: increase height, update layout, new plot for ETIO, ensure alignment

#new interaction figure
#fig 2a - `peak` group
setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEcoefs_peak_new.png", width = 3000, height = 4000, res = 300)
layout(matrix(c(1, 6,
                2, 6,
                3, 6,
                4, 6,
                5, 6), ncol = 2, byrow = TRUE),
       widths = c(1.75, 1.25), heights = c(1, 1, 1, 1, 1, 1))

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
#nino pch 21
plot(SE2_ninolag, SE2_ninocoef, pch = 21, 
     col = "grey4", bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52), cex.axis = 1.6, 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_ninolag, SE22_ninocoef, pch = 21, col = "black",
       bg =  alpha("magenta4",.65), cex = 2.25)
abline(h = 0, lty = 2)
title("Ni\u00f1o 3.4", adj = 0, cex.main = 1.5)

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

#TODO: seperate this into WTIO and ETIO plots
# --- Plot 2: WTIO ---
par(mar = c(4, 4, 2, 1))
#wtio pch 24
plot(SE2_wtiolag-0.25, SE2_wtiocoef, pch = 24, col = "black",
     bg =  alpha("forestgreen",.5), cex = 1.8, cex.axis = 1.6,
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_wtiolag+0.25, SE22_wtiocoef, pch = 24, col = "black",
       bg =  alpha("magenta4",.65) , cex = 1.8)
points(SE23_wtiolag, SE23_wtiocoef, pch = 24, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 1.8)
abline(h = 0, lty = 2)
title("WTIO", adj = 0, cex.main = 1.5)


# --- Plot 3: ETIO
par(mar = c(4, 4, 2, 1))
#etio pch 25
plot(SE2_etiolag, SE2_etiocoef, pch = 25, col = "grey4",
     bg =  alpha("forestgreen",.5), cex = 1.8, cex.axis = 1.6,
     xlim = c(1,52), 
     ylim = SEAus2_range,
     xlab = "", ylab = "")
points(SE22_etiolag, SE22_etiocoef, pch = 25, col = "black",
       bg =  alpha("magenta4",.65) , cex = 1.8)
points(SE23_etiolag, SE23_etiocoef, pch = 25, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 1.8)
abline(h = 0, lty = 2)
title("ETIO", adj = 0, cex.main = 1.5)


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


# --- Plot 4: TSA ---
par(mar = c(4, 4, 2, 1))
#tsa pch 22
plot(SE2_tsalag-0.25, SE2_tsacoef, pch = 22, col = "black",
     bg =  alpha("forestgreen", 0.5), xlim = c(1,52), cex = 2.25,
     ylim = SEAus2_range, cex.axis = 1.6,
     xlab = "", ylab = "")
points(SE22_tsalag+0.25, SE22_tsacoef, pch = 22, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE23_tsalag, SE23_tsacoef, pch = 22, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("TSA", adj = 0, cex.main = 1.5)

## --- TSA Interaction
## etio_lag8:tsa_lag31
## vary
links[[7]] <- list(
  y_val = SE23_tsacoef[1],
  from_x = grconvertX(SE23_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE23_tsacoef[1], from = "user", to = "ndc")
)

# --- Plot 5: SAM/AAO ---
par(mar = c(4, 4, 2, 1))
#sam pch 23
plot(SE2_aaolag-0.25, SE2_aaocoef, pch = 23,
     col = "grey4",
     bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52),  cex.axis = 1.6, cex.lab = 1.75,
     ylim = SEAus2_range,
     xlab = "Lag", ylab = "")
points(SE22_aaolag+0.25, SE22_aaocoef, pch = 23, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE23_aaolag, SE23_aaocoef, pch = 23, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("SAM", adj = 0, cex.main = 1.5)

# --- Plot 6: Interaction Effects ---
par(mar = c(4, 4, 2, 2))

plot(SE2.coef[1], 0, type = "n", main = "", 
     ylim = c(0,1), xlim = SEAus2_range, cex = 2, cex.axis = 1.6,
     xlab = "Coefficients", cex.lab = 1.75,
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
segments(SE2.coef[10], int_2, SE2.coef[10], int_pt1, col = "forestgreen", lty = 2, lwd = 1.75)
segments(SE2.coef[10], int_4, SE2.coef[10], int_pt1, col = "forestgreen", lty = 2, lwd = 1.75)
## const
int_pt2 <- (int_3 + int_5)/2
segments(SE2.constcoef[10], int_3, SE2.constcoef[10], int_pt2, col = "magenta4", lty = 2, lwd = 1.75)
segments(SE2.constcoef[10], int_5, SE2.constcoef[10], int_pt2, col = "magenta4", lty = 2, lwd = 1.75)
## etio_lag8:tsa_lag31
## vary
int_pt3 <- (int_6 + int_7)/2
segments(SE2.varycoef[8], int_6, SE2.varycoef[8], int_pt3, col = "darkorange2", lty = 2, lwd = 1.75)
segments(SE2.varycoef[8], int_7, SE2.varycoef[8], int_pt3, col = "darkorange2", lty = 2, lwd = 1.75)

#interaction points
points(SE2.varycoef[7], int_1,  pch = 25, col = "grey4",
       bg = alpha("darkorange2",.65), cex = 2,) 
points(SE2.coef[10], int_pt1,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE2.constcoef[10], int_pt2,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE2.varycoef[8], int_pt3,  pch = 11, col = alpha("darkorange3",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 

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
colors <- c("forestgreen", "magenta4", "forestgreen", "magenta4", "darkorange2", "darkorange2", "darkorange2")
linetypes <- rep(2,7)


for (i in 1:length(links)) {
  grid.lines(
    x = unit(c(links[[i]]$from_x, links[[i]]$to_x), "npc"),
    y = unit(c(links[[i]]$from_y, links[[i]]$to_y), "npc"),
    gp = gpar(col = colors[i], lwd = 1.75, lty = linetypes[i])
  )
}

mtext("Coefficients", side = 2, outer = TRUE, padj = 0.5, cex = 1.25)

#add legends
par(xpd = NA)
legend("topright", inset = c(0.00, 0.00),
       title = "Terms", cex = 1.5,
       legend = c("Ni\u00f1o 3.4", "WTIO", "ETIO", "TSA", "SAM", "Interaction"),
       pch = c(21, 24, 25, 22, 23, 11),
       col = "grey4",
       pt.bg = alpha("gray36",.65),
       pt.cex = c(2.25, 1.8, 1.8, 2.25, 2.25, 1.8))
legend("topright", inset = c(0.004, 0.1775),
       title = "Model", cex = 1.5,
       legend = c("Full", "Fixed", "Non-Fixed"),
       pch = 15,
       col = c("forestgreen", "magenta4", "darkorange2"),
       pt.cex = 2)

dev.off()



#new interaction figure

#fig SI - early group
SE1.coef
SE1.constcoef
SE1.varycoef


# --- Data Setup --- #
## Nino
SE1_ninolag <- c(33)
SE1_ninocoef <- SE1.coef[2]
SE12_ninolag <- c(33)
SE12_ninocoef <- SE1.constcoef[2]
SE13_ninolag <- c(40)
SE13_ninocoef <- SE1.varycoef[2]

## WTIO
SE1_wtiolag <- c(5)
SE1_wtiocoef <- SE1.coef[3]
SE12_wtiolag <- c(5)
SE12_wtiocoef <- SE1.constcoef[3]

## ETIO
SE13_etiolag <- c(2, 42)
SE13_etiocoef <- SE1.varycoef[3:4]

## TSA
SE1_tsalag <- c(12,14)
SE1_tsacoef <- SE1.coef[4:5]
SE12_tsalag <- c(12,14)
SE12_tsacoef <- SE1.constcoef[4:5]
SE13_tsalag <- c(12,16,19)
SE13_tsacoef <- SE1.varycoef[5:7]

## SAM (AAO)
SE1_aaolag <- c(24,28,29,33,41)
SE1_aaocoef <- SE1.coef[6:10]
SE12_aaolag <- c(24,28,29,33,41)
SE12_aaocoef <- SE1.constcoef[6:10]
SE13_aaolag <- c(24,29)
SE13_aaocoef <- SE1.varycoef[8:9]

## OLR (MJO)
SE1_olrlag <- c(2, 14)
SE1_olrcoef <- SE1.coef[11:12]
SE12_olrlag <- c(2, 14)
SE12_olrcoef <- SE1.constcoef[11:12]
SE13_olrlag <- c(2, 14)
SE13_olrcoef <- SE1.varycoef[10:11]

# --- Range ---
SEAus1_absmax <- max(abs(range(SE1.coef,
                               SE1.constcoef,
                               SE1.varycoef)))
SEAus1_range <- c(-SEAus1_absmax, SEAus1_absmax)

# --- Plot 1: Nino ---
par(mar = c(4, 4, 2, 1))
#nino pch 21
plot(SE1_ninolag, SE1_ninocoef, pch = 21, 
     col = "grey4", bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52), cex.axis = 1.6, 
     ylim = SEAus1_range,
     xlab = "", ylab = "")
points(SE12_ninolag, SE12_ninocoef, pch = 21, col = "black",
       bg =  alpha("magenta4",.65), cex = 2.25)
points(SE13_ninolag, SE13_ninocoef, pch = 21, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("Ni\u00f1o 3.4", adj = 0, cex.main = 1.5)

# --- Plot 2: WTIO & ETIO ---
par(mar = c(4, 4, 2, 1))
#wtio pch 24, etio pch 25
plot(SE1_wtiolag, SE1_wtiocoef, pch = 24, col = "black",
     bg =  alpha("forestgreen",.5), cex = 1.8, cex.axis = 1.6,
     xlim = c(1,52), 
     ylim = SEAus1_range,
     xlab = "", ylab = "")
points(SE12_wtiolag, SE12_wtiocoef, pch = 24, col = "black",
       bg =  alpha("magenta4",.65) , cex = 1.8)
points(SE13_etiolag, SE13_etiocoef, pch = 25, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 1.8)
abline(h = 0, lty = 2)
title("WTIO & ETIO", adj = 0, cex.main = 1.5)

# --- Plot 3: TSA ---
par(mar = c(4, 4, 2, 1))
#tsa pch 22
plot(SE1_tsalag, SE1_tsacoef, pch = 22, col = "black",
     bg =  alpha("forestgreen", 0.5), xlim = c(1,52), cex = 2.25,
     ylim = SEAus1_range, cex.axis = 1.6,
     xlab = "", ylab = "")
points(SE12_tsalag, SE12_tsacoef, pch = 22, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE13_tsalag, SE13_tsacoef, pch = 22, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("TSA", adj = 0, cex.main = 1.5)

# --- Plot 4: SAM/AAO ---
par(mar = c(4, 4, 2, 1))
#sam pch 23
plot(SE1_aaolag+0.25, SE1_aaocoef, pch = 23,
     col = "grey4",
     bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52),  cex.axis = 1.6, cex.lab = 1.75,
     ylim = SEAus1_range,
     xlab = "", ylab = "")
points(SE12_aaolag-0.25, SE12_aaocoef, pch = 23, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE13_aaolag, SE13_aaocoef, pch = 23, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("SAM", adj = 0, cex.main = 1.5)

# --- Plot 5: OLR ---
par(mar = c(4, 4, 2, 1))
#olr pch ??
plot(SE1_olrlag+0.15, SE1_olrcoef, pch = 19,
     col = alpha("forestgreen",.5), cex = 2,
     xlim = c(1,52),  cex.axis = 1.6, cex.lab = 1.75,
     ylim = SEAus1_range,
     xlab = "Lag", ylab = "")
points(SE1_olrlag+0.15, SE1_olrcoef, pch = 10, col = "black",
       #bg =  alpha("magenta4",.65), 
       cex = 2)
points(SE12_olrlag+0.25, SE12_olrcoef, pch = 19,
       col =  alpha("magenta4",.65), 
       cex = 2)
points(SE12_olrlag+0.25, SE12_olrcoef, pch = 10, col = "black",
       cex = 2)
points(SE13_olrlag-0.30, SE13_olrcoef, pch = 19, col = alpha("darkorange2",.65),
       cex = 2)
points(SE13_olrlag-0.30, SE13_olrcoef, pch = 10, col = "black",
       cex = 2)
abline(h = 0, lty = 2)
title("OLR", adj = 0, cex.main = 1.5)






#fig SI - late group
SE3.coef
SE3.constcoef
SE3.varycoef


# --- Data Setup --- #
## Nino
SE3_ninolag <- c(25,47)
SE3_ninocoef <- SE3.coef[2:3]
SE32_ninolag <- c(25, 47)
SE32_ninocoef <- SE3.constcoef[2:3]
SE32_ninolag <- c(16, 25, 35)
SE32_ninocoef <- SE3.varycoef[2:4]

## WTIO



#old style coef/inter plot (doesn't use colors to differentiate )
setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEcoefs_group2.png", width = 3000, height = 3000, res = 300)
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
title("Ni\u00f1o 3.4", adj = 0, cex.main = 1.5)

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


