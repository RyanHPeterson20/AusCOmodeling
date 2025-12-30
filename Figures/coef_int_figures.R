
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
png(filename = "SEcoefs_peak_new.png", width = 3200, height = 4000, res = 300)
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
points(SE22_ninolag, SE22_ninocoef, pch = 21, col = "grey4",
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
#TODO: jitter this line down a little
## etio_lag8:tsa_lag31
## vary
links[[6]] <- list(
  y_val = SE23_etiocoef[1], #small shift down here.
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


#fig SI
setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEcoefs_early.png", width = 3600, height = 3600, res = 300)
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

## --- Nino Interaction
## I(nino_lag33^2) 
## base
links[[1]] <- list(
  y_val = SE1_ninocoef[1],
  from_x = grconvertX(SE1_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE1_ninocoef[1], from = "user", to = "ndc")
)

## constant
links[[2]] <- list(
  y_val = SE12_ninocoef[1],
  from_x = grconvertX(SE12_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE12_ninocoef[1], from = "user", to = "ndc")
)

## nino_lag33:tsa_lag12
## base
links[[3]] <- list(
  y_val = SE1_ninocoef[1],
  from_x = grconvertX(SE1_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE1_ninocoef[1], from = "user", to = "ndc")
)

## constant
links[[4]] <- list(
  y_val = SE12_ninocoef[1],
  from_x = grconvertX(SE12_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE12_ninocoef[1], from = "user", to = "ndc")
)

## nino_lag40:etio_lag42 
## varying
links[[5]] <- list(
  y_val = SE13_ninocoef[1],
  from_x = grconvertX(SE13_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_ninocoef[1], from = "user", to = "ndc")
)

## nino_lag40:tsa_lag12
## varying
links[[6]] <- list(
  y_val = SE13_ninocoef[1],
  from_x = grconvertX(SE13_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_ninocoef[1], from = "user", to = "ndc")
)

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

## --- ETIO Interaction
## etio_lag2:aao_lag29  
## varying
links[[7]] <- list(
  y_val = SE13_etiocoef[1],
  from_x = grconvertX(SE13_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_etiocoef[1], from = "user", to = "ndc")
)

## etio_lag2:etio_lag42 
## varying
links[[8]] <- list(
  y_val = SE13_etiocoef[1],
  from_x = grconvertX(SE13_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_etiocoef[1], from = "user", to = "ndc")
)

## etio_lag2:etio_lag42 
## varying
links[[9]] <- list(
  y_val = SE13_etiocoef[2],
  from_x = grconvertX(SE13_etiolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_etiocoef[2], from = "user", to = "ndc")
)

## nino_lag40:etio_lag42
## varying
links[[10]] <- list(
  y_val = SE13_etiocoef[2],
  from_x = grconvertX(SE13_etiolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_etiocoef[2], from = "user", to = "ndc")
)


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

## --- TSA Interaction
## tsa_lag14:aao_lag33 
## base
links[[11]] <- list(
  y_val = SE1_tsacoef[2],
  from_x = grconvertX(SE1_tsalag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE1_tsacoef[2], from = "user", to = "ndc")
)

## tsa_lag14:aao_lag33 
## constant
links[[12]] <- list(
  y_val = SE12_tsacoef[2],
  from_x = grconvertX(SE12_tsalag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE12_tsacoef[2], from = "user", to = "ndc")
)

## nino_lag33:tsa_lag12 
## base
links[[13]] <- list(
  y_val = SE1_tsacoef[1],
  from_x = grconvertX(SE1_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE1_tsacoef[1], from = "user", to = "ndc")
)

## nino_lag33:tsa_lag12 
## constant
links[[14]] <- list(
  y_val = SE12_tsacoef[1],
  from_x = grconvertX(SE12_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE12_tsacoef[1], from = "user", to = "ndc")
)

## nino_lag40:tsa_lag12
## varying
links[[15]] <- list(
  y_val = SE13_tsacoef[1],
  from_x = grconvertX(SE13_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_tsacoef[1], from = "user", to = "ndc")
)

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

## --- SAM Interaction
## tsa_lag14:aao_lag33
## base
links[[16]] <- list(
  y_val = SE1_aaocoef[4],
  from_x = grconvertX(SE1_aaolag[4], from = "user", to = "ndc"),
  from_y = grconvertY(SE1_aaocoef[4], from = "user", to = "ndc")
)

## tsa_lag14:aao_lag33
## constant
links[[17]] <- list(
  y_val = SE12_aaocoef[4],
  from_x = grconvertX(SE12_aaolag[4], from = "user", to = "ndc"),
  from_y = grconvertY(SE12_aaocoef[4], from = "user", to = "ndc")
)

## I(aao_lag24^2)
## varying
links[[18]] <- list(
  y_val = SE13_aaocoef[1],
  from_x = grconvertX(SE13_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_aaocoef[1], from = "user", to = "ndc")
)

## etio_lag2:aao_lag29
## varying
links[[19]] <- list(
  y_val = SE13_aaocoef[2],
  from_x = grconvertX(SE13_aaolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE13_aaocoef[2], from = "user", to = "ndc")
)

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


# --- Plot 6: Interaction Effects ---
par(mar = c(4, 4, 2, 2))

plot(SE1.coef[1], 0, type = "n", main = "", 
     ylim = c(0,1), xlim = c(-2, 2), cex = 2, cex.axis = 1.6,
     xlab = "Coefficients", cex.lab = 1.75,
     yaxt = "n",  ylab = "")
abline(v= 0, lty = 2)

#square terms
int_1 <- grconvertY(links[[1]]$from_y, from = "ndc", to = "user") #I(nino_lag33^2) (base)
int_2 <- grconvertY(links[[2]]$from_y, from = "ndc", to = "user") #I(nino_lag33^2) (const)
int_3 <- grconvertY(links[[18]]$from_y, from = "ndc", to = "user")  #I(aao_lag24^2) (vary)

#interactions
int_4 <- grconvertY(links[[3]]$from_y, from = "ndc", to = "user") # nino_lag33 -> tsa_lag12 #base
int_5 <- grconvertY(links[[4]]$from_y, from = "ndc", to = "user") # nino_lag33 -> tsa_lag12 #const
int_6 <- grconvertY(links[[5]]$from_y, from = "ndc", to = "user") # nino_lag40 -> etio_lag42 #vary
int_7 <- grconvertY(links[[6]]$from_y, from = "ndc", to = "user") # nino_lag40 -> tsa_lag12 #vary
int_8 <- grconvertY(links[[7]]$from_y, from = "ndc", to = "user") # etio_lag2 -> aao_lag29 #vary
int_9 <- grconvertY(links[[8]]$from_y, from = "ndc", to = "user")  # etio_lag2 -> etio_lag42 #vary
int_10 <- grconvertY(links[[9]]$from_y, from = "ndc", to = "user")  # etio_lag42 -> etio_lag2 #vary
int_11 <- grconvertY(links[[10]]$from_y, from = "ndc", to = "user") # etio_lag42 -> nino_lag40 #vary
int_12 <- grconvertY(links[[11]]$from_y, from = "ndc", to = "user")  # tsa_lag14 -> aao_lag33 #base
int_13 <- grconvertY(links[[12]]$from_y, from = "ndc", to = "user")  # tsa_lag14 -> aao_lag33 #const
int_14 <- grconvertY(links[[13]]$from_y, from = "ndc", to = "user")  # tsa_lag12 -> nino_lag33 #base
int_15 <- grconvertY(links[[14]]$from_y, from = "ndc", to = "user")  # tsa_lag12 -> nino_lag33 #const
int_16 <- grconvertY(links[[15]]$from_y, from = "ndc", to = "user") # tsa_lag12 -> nino_lag40 #vary
int_17 <- grconvertY(links[[16]]$from_y, from = "ndc", to = "user")  # aao_lag33 -> tsa_lag14 #base
int_18 <- grconvertY(links[[17]]$from_y, from = "ndc", to = "user")  # aao_lag33 -> tsa_lag14 #const
int_19 <- grconvertY(links[[19]]$from_y, from = "ndc", to = "user") # aao_lag29 -> etio_lag2 #vary

# nino_lag33 : tsa_lag12
# base
int_pt1 <- (int_4 + int_14)/2
segments(SE1.coef[15], int_4, SE1.coef[15], int_pt1, col = "forestgreen", lty = 2, lwd = 2)
segments(SE1.coef[15], int_14, SE1.coef[15], int_pt1, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt2 <- (int_5 + int_15)/2
segments(SE1.constcoef[15], int_5, SE1.constcoef[15], int_pt2, col = "magenta4", lty = 2, lwd = 2)
segments(SE1.constcoef[15], int_15, SE1.constcoef[15], int_pt2, col = "magenta4", lty = 2, lwd = 2)
# nino_lag40 : etio_lag42
# varying
int_pt3 <- (int_6 + int_11)/2
segments(SE1.varycoef[15], int_6, SE1.varycoef[15], int_pt3, col = "darkorange2", lty = 2, lwd = 2)
segments(SE1.varycoef[15], int_11, SE1.varycoef[15], int_pt3, col = "darkorange2", lty = 2, lwd = 2)
# nino_lag40 : tsa_lag12
# varying
int_pt4 <- (int_7 + int_16)/2
segments(SE1.varycoef[16], int_7, SE1.varycoef[16], int_pt4, col = "darkorange2", lty = 2, lwd = 2)
segments(SE1.varycoef[16], int_16, SE1.varycoef[16], int_pt4, col = "darkorange2", lty = 2, lwd = 2)
# etio_lag2 : aao_lag29
# varying
int_pt5 <- (int_8 + int_19)/2
segments(SE1.varycoef[13], int_8, SE1.varycoef[13], int_pt5, col = "darkorange2", lty = 2, lwd = 2)
segments(SE1.varycoef[13], int_19, SE1.varycoef[13], int_pt5, col = "darkorange2", lty = 2, lwd = 2)
# etio_lag2 : etio_lag42
# varying
int_pt6 <- (int_9 + int_10)/2
segments(SE1.varycoef[14], int_9, SE1.varycoef[14], int_pt6, col = "darkorange2", lty = 2, lwd = 2)
segments(SE1.varycoef[14], int_10, SE1.varycoef[14], int_pt6, col = "darkorange2", lty = 2, lwd = 2)
# tsa_lag14 : aao_lag33
# base
int_pt7 <- (int_12 + int_17)/2
segments(SE1.coef[14], int_12, SE1.coef[14], int_pt7, col = "forestgreen", lty = 2, lwd = 2)
segments(SE1.coef[14], int_17, SE1.coef[14], int_pt7, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt8 <- (int_13 + int_18)/2
segments(SE1.constcoef[14], int_13, SE1.constcoef[14], int_pt8, col = "magenta4", lty = 2, lwd = 2)
segments(SE1.constcoef[14], int_18, SE1.constcoef[14], int_pt8, col = "magenta4", lty = 2, lwd = 2)

#quad points
points(SE1.coef[13], int_1,  pch = 21, col = "grey4",
       bg = alpha("forestgreen",.65), cex = 2) 
points(SE1.constcoef[13], int_2,  pch = 21, col = "grey4",
       bg = alpha("magenta4",.65), cex = 2) 
points(SE1.varycoef[12], int_3,  pch = 23, col = "grey4",
       bg = alpha("darkorange2",.65), cex = 2) 
#interaction points
points(SE1.coef[15], int_pt1,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE1.constcoef[15], int_pt2,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE1.varycoef[15], int_pt3,  pch = 11, col = alpha("darkorange3",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE1.varycoef[16], int_pt4,  pch = 11, col = alpha("darkorange3",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE1.varycoef[13], int_pt5,  pch = 11, col = alpha("darkorange3",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE1.varycoef[14], int_pt6,  pch = 11, col = alpha("darkorange3",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE1.coef[14], int_pt7,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE1.constcoef[14], int_pt8,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 

#link to x 
links[[1]]$to_x <- grconvertX(SE1.coef[13], from = "user", to = "ndc")
links[[2]]$to_x <- grconvertX(SE1.constcoef[13], from = "user", to = "ndc")
links[[3]]$to_x <- grconvertX(SE1.coef[15], from = "user", to = "ndc")
links[[4]]$to_x <- grconvertX(SE1.constcoef[15], from = "user", to = "ndc")
links[[5]]$to_x <- grconvertX(SE1.varycoef[15], from = "user", to = "ndc")
links[[6]]$to_x <- grconvertX(SE1.varycoef[16], from = "user", to = "ndc")
links[[7]]$to_x <- grconvertX(SE1.varycoef[13], from = "user", to = "ndc")
links[[8]]$to_x <- grconvertX(SE1.varycoef[14], from = "user", to = "ndc")
links[[9]]$to_x <- grconvertX(SE1.varycoef[14], from = "user", to = "ndc")
links[[10]]$to_x <- grconvertX(SE1.varycoef[15], from = "user", to = "ndc")
links[[11]]$to_x <- grconvertX(SE1.coef[14], from = "user", to = "ndc")
links[[12]]$to_x <- grconvertX(SE1.constcoef[14], from = "user", to = "ndc")
links[[13]]$to_x <- grconvertX(SE1.coef[15], from = "user", to = "ndc")
links[[14]]$to_x <- grconvertX(SE1.constcoef[15], from = "user", to = "ndc")
links[[15]]$to_x <- grconvertX(SE1.varycoef[16], from = "user", to = "ndc")
links[[16]]$to_x <- grconvertX(SE1.coef[14], from = "user", to = "ndc")
links[[17]]$to_x <- grconvertX(SE1.constcoef[14], from = "user", to = "ndc")
links[[18]]$to_x <- grconvertX(SE1.varycoef[12], from = "user", to = "ndc")
links[[19]]$to_x <- grconvertX(SE1.varycoef[13], from = "user", to = "ndc")

for (i in 1:length(links)) {
  links[[i]]$to_y <- links[[i]]$from_y  # same y to keep it horizontal
}

# --- Draw horizontal linking lines ---
par(xpd = NA)  # allow drawing outside plot regions
colors <- c("forestgreen", "magenta4", "forestgreen", "magenta4", rep( "darkorange2", 6),
            "forestgreen", "magenta4", "forestgreen", "magenta4", "darkorange2", 
            "forestgreen", "magenta4", "darkorange2", "darkorange2")
linetypes <- rep(2,19)


for (i in 1:length(links)) {
  grid.lines(
    x = unit(c(links[[i]]$from_x, links[[i]]$to_x), "npc"),
    y = unit(c(links[[i]]$from_y, links[[i]]$to_y), "npc"),
    gp = gpar(col = colors[i], lwd = 1.75, lty = linetypes[i])
  )
}


mtext("Coefficients", side = 2, outer = TRUE, padj = 0.5, cex = 1.25)

dev.off()



#fig SI - late group
SE3.coef
SE3.constcoef
SE3.varycoef




#fig SI
setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "SEcoefs_late.png", width = 3600, height = 3600, res = 300)
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
SE3_ninolag <- c(25,47)
SE3_ninocoef <- SE3.coef[2:3]
SE32_ninolag <- c(25, 47)
SE32_ninocoef <- SE3.constcoef[2:3]
SE33_ninolag <- c(16, 25, 35)
SE33_ninocoef <- SE3.varycoef[2:4]

## WTIO
SE33_wtiolag <- c(38,51)
SE33_wtiocoef <- SE3.varycoef[5:6]

## ETIO
SE3_etiolag <- c(16,33)
SE3_etiocoef <- SE3.coef[4:5]
SE32_etiolag <- c(16,33)
SE32_etiocoef <- SE3.constcoef[4:5]
SE33_etiolag <- c(16,19)
SE33_etiocoef <- SE3.varycoef[7:8]

## TSA
SE3_tsalag <- c(22)
SE3_tsacoef <- SE3.coef[6]
SE32_tsalag <- c(22)
SE32_tsacoef <- SE3.constcoef[6]
SE33_tsalag <- c(22)
SE33_tsacoef <- SE3.varycoef[9]

## SAM (AAO)
SE3_aaolag <- c(1,50)
SE3_aaocoef <- SE3.coef[7:8]
SE32_aaolag <- c(1,50)
SE32_aaocoef <- SE3.constcoef[7:8]
SE33_aaolag <- c(37)
SE33_aaocoef <- SE3.varycoef[10]

## OLR
SE3_olrlag <- c(6)
SE3_olrcoef <- SE3.coef[9]
SE32_olrlag <- c(6)
SE32_olrcoef <- SE3.constcoef[9]
SE33_olrlag <- c(6, 9)
SE33_olrcoef <- SE3.varycoef[11:12]


# --- Range ---
SEAus3_absmax <- max(abs(range(SE3.coef,
                               SE3.constcoef,
                               SE3.varycoef)))
SEAus3_range <- c(-SEAus3_absmax, SEAus3_absmax)

# --- Plot 1: Nino ---
par(mar = c(4, 4, 2, 1))
#nino pch 21
plot(SE3_ninolag+0.25, SE3_ninocoef, pch = 21, 
     col = "grey4", bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52), cex.axis = 1.6, 
     ylim = SEAus3_range,
     xlab = "", ylab = "")
points(SE32_ninolag-0.25, SE32_ninocoef, pch = 21, col = "black",
       bg =  alpha("magenta4",.65), cex = 2.25)
points(SE33_ninolag, SE33_ninocoef, pch = 21, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("Ni\u00f1o 3.4", adj = 0, cex.main = 1.5)

## --- Nino Interaction
## nino_lag25:aao_lag50
## base
links[[1]] <- list(
  y_val = SE3_ninocoef[1],
  from_x = grconvertX(SE3_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_ninocoef[1], from = "user", to = "ndc")
)

## nino_lag25:aao_lag50
## constant
links[[2]] <- list(
  y_val = SE32_ninocoef[1],
  from_x = grconvertX(SE32_ninolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_ninocoef[1], from = "user", to = "ndc")
)

# --- Plot 2: WTIO & ETIO ---
par(mar = c(4, 4, 2, 1))
#wtio pch 24, etio pch 25
plot(SE33_wtiolag, SE33_wtiocoef, pch = 24, col = "black",
     bg =  alpha("darkorange2",.65), cex = 1.8, cex.axis = 1.6,
     xlim = c(1,52), 
     ylim = SEAus3_range,
     xlab = "", ylab = "")
points(SE3_etiolag+0.25, SE3_etiocoef, pch = 25, col = "black",
       bg =  alpha("forestgreen",.5) , cex = 1.8)
points(SE32_etiolag-0.25, SE32_etiocoef, pch = 25, col = "black",
       bg =  alpha("magenta4",.65) , cex = 1.8)
points(SE33_etiolag, SE33_etiocoef, pch = 25, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 1.8)
abline(h = 0, lty = 2)
title("WTIO & ETIO", adj = 0, cex.main = 1.5)

## --- WTIO Interactions
## wtio_lag38:aao_lag37 
## varying
links[[3]] <- list(
  y_val = SE33_wtiocoef[1],
  from_x = grconvertX(SE33_wtiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE33_wtiocoef[1], from = "user", to = "ndc")
)

## --- ETIO Interactions
## etio_lag16:aao_lag1
## base
links[[4]] <- list(
  y_val = SE3_etiocoef[1],
  from_x = grconvertX(SE3_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_etiocoef[1], from = "user", to = "ndc")
)

## etio_lag16:aao_lag1
## constant
links[[5]] <- list(
  y_val = SE32_etiocoef[1],
  from_x = grconvertX(SE32_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_etiocoef[1], from = "user", to = "ndc")
)

##  etio_lag33:tsa_lag22
## base
links[[6]] <- list(
  y_val = SE3_etiocoef[2],
  from_x = grconvertX(SE3_etiolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_etiocoef[2], from = "user", to = "ndc")
)

## etio_lag33:tsa_lag22
## constant
links[[7]] <- list(
  y_val = SE32_etiocoef[2],
  from_x = grconvertX(SE32_etiolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_etiocoef[2], from = "user", to = "ndc")
)

## I(etio_lag16^2)
## varying
links[[8]] <- list(
  y_val = SE33_etiocoef[1],
  from_x = grconvertX(SE33_etiolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE33_etiocoef[1], from = "user", to = "ndc")
)


# --- Plot 3: TSA ---
par(mar = c(4, 4, 2, 1))
#tsa pch 22
plot(SE3_tsalag+0.25, SE3_tsacoef, pch = 22, col = "black",
     bg =  alpha("forestgreen", 0.5), xlim = c(1,52), cex = 2.25,
     ylim = SEAus3_range, cex.axis = 1.6,
     xlab = "", ylab = "")
points(SE32_tsalag-0.25, SE32_tsacoef, pch = 22, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE33_tsalag, SE33_tsacoef, pch = 22, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("TSA", adj = 0, cex.main = 1.5)

## --- TSA Interactions
## tsa_lag22:aao_lag1
## base
links[[9]] <- list(
  y_val = SE3_tsacoef[1],
  from_x = grconvertX(SE3_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_tsacoef[1], from = "user", to = "ndc")
)

## tsa_lag22:aao_lag1
## constant
links[[10]] <- list(
  y_val = SE32_tsacoef[1],
  from_x = grconvertX(SE32_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_tsacoef[1], from = "user", to = "ndc")
)

## etio_lag33:tsa_lag22
## base
links[[11]] <- list(
  y_val = SE3_tsacoef[1],
  from_x = grconvertX(SE3_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_tsacoef[1], from = "user", to = "ndc")
)

## etio_lag33:tsa_lag22
## constant
links[[12]] <- list(
  y_val = SE32_tsacoef[1],
  from_x = grconvertX(SE32_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_tsacoef[1], from = "user", to = "ndc")
)

## tsa_lag22:olr_lag6
## base
links[[13]] <- list(
  y_val = SE3_tsacoef[1],
  from_x = grconvertX(SE3_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_tsacoef[1], from = "user", to = "ndc")
)

## tsa_lag22:olr_lag6
## constant
links[[14]] <- list(
  y_val = SE32_tsacoef[1],
  from_x = grconvertX(SE32_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_tsacoef[1], from = "user", to = "ndc")
)

## tsa_lag22:aao_lag37
## varying
links[[15]] <- list(
  y_val = SE33_tsacoef[1],
  from_x = grconvertX(SE33_tsalag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE33_tsacoef[1], from = "user", to = "ndc")
)

# --- Plot 4: SAM/AAO ---
par(mar = c(4, 4, 2, 1))
#sam pch 23
plot(SE3_aaolag+0.25, SE3_aaocoef, pch = 23,
     col = "grey4",
     bg =  alpha("forestgreen",.5), cex = 2.25,
     xlim = c(1,52),  cex.axis = 1.6, cex.lab = 1.75,
     ylim = SEAus3_range,
     xlab = "", ylab = "")
points(SE32_aaolag-0.25, SE32_aaocoef, pch = 23, col = "black",
       bg =  alpha("magenta4",.65) , cex = 2.25)
points(SE33_aaolag, SE33_aaocoef, pch = 23, col = "black",
       bg =  alpha("darkorange2",.65) , cex = 2.25)
abline(h = 0, lty = 2)
title("SAM", adj = 0, cex.main = 1.5)

## --- SAM(AAO) Interactions
## tsa_lag22:aao_lag1
## base
links[[16]] <- list(
  y_val = SE3_aaocoef[1],
  from_x = grconvertX(SE3_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_aaocoef[1], from = "user", to = "ndc")
)

## tsa_lag22:aao_lag1
## constant
links[[17]] <- list(
  y_val = SE32_aaocoef[1],
  from_x = grconvertX(SE32_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_aaocoef[1], from = "user", to = "ndc")
)

## etio_lag16:aao_lag1
## base
links[[18]] <- list(
  y_val = SE3_aaocoef[1],
  from_x = grconvertX(SE3_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_aaocoef[1], from = "user", to = "ndc")
)

## etio_lag16:aao_lag1
## constant
links[[19]] <- list(
  y_val = SE32_aaocoef[1],
  from_x = grconvertX(SE32_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_aaocoef[1], from = "user", to = "ndc")
)

## nino_lag25:aao_lag50
## base
links[[20]] <- list(
  y_val = SE3_aaocoef[2],
  from_x = grconvertX(SE3_aaolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_aaocoef[2], from = "user", to = "ndc")
)

## nino_lag25:aao_lag50
## constant
links[[21]] <- list(
  y_val = SE32_aaocoef[2],
  from_x = grconvertX(SE32_aaolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_aaocoef[2], from = "user", to = "ndc")
)

## aao_lag50:olr_lag6
## base
links[[22]] <- list(
  y_val = SE3_aaocoef[2],
  from_x = grconvertX(SE3_aaolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_aaocoef[2], from = "user", to = "ndc")
)

## aao_lag50:olr_lag6
## constant
links[[23]] <- list(
  y_val = SE32_aaocoef[2],
  from_x = grconvertX(SE32_aaolag[2], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_aaocoef[2], from = "user", to = "ndc")
)

## tsa_lag22:aao_lag37
## varying
links[[24]] <- list(
  y_val = SE33_aaocoef[1],
  from_x = grconvertX(SE33_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE33_aaocoef[1], from = "user", to = "ndc")
)

## wtio_lag38:aao_lag37
## varying
links[[25]] <- list(
  y_val = SE33_aaocoef[1],
  from_x = grconvertX(SE33_aaolag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE33_aaocoef[1], from = "user", to = "ndc")
)

# --- Plot 5: OLR ---
par(mar = c(4, 4, 2, 1))
#olr pch ??
plot(SE3_olrlag+0.25, SE3_olrcoef, pch = 9,
     col = "forestgreen", cex = 2,
     xlim = c(1,52),  cex.axis = 1.6, cex.lab = 1.75,
     ylim = SEAus3_range,
     xlab = "Lag", ylab = "")
points(SE32_olrlag-0.25, SE32_olrcoef, pch = 9,
       col =  "magenta4", 
       cex = 2)
points(SE33_olrlag, SE33_olrcoef, pch = 9, col = "darkorange2",
       cex = 2)
abline(h = 0, lty = 2)
title("OLR", adj = 0, cex.main = 1.5)

## --- OLR Interaction
## aao_lag50:olr_lag6
## base
links[[26]] <- list(
  y_val = SE3_olrcoef[1],
  from_x = grconvertX(SE3_olrlag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_olrcoef[1], from = "user", to = "ndc")
)

## aao_lag50:olr_lag6
## constant
links[[27]] <- list(
  y_val = SE32_olrcoef[1],
  from_x = grconvertX(SE32_olrlag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_olrcoef[1], from = "user", to = "ndc")
)

## tsa_lag22:olr_lag6
## base
links[[28]] <- list(
  y_val = SE3_olrcoef[1],
  from_x = grconvertX(SE3_olrlag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE3_olrcoef[1], from = "user", to = "ndc")
)

## tsa_lag22:olr_lag6
## constant
links[[29]] <- list(
  y_val = SE32_olrcoef[1],
  from_x = grconvertX(SE32_olrlag[1], from = "user", to = "ndc"),
  from_y = grconvertY(SE32_olrcoef[1], from = "user", to = "ndc")
)

# --- Plot 6: Interaction Effects ---
par(mar = c(4, 4, 2, 2))

plot(SE1.coef[1], 0, type = "n", main = "", 
     ylim = c(0,1), xlim = SEAus3_range, cex = 2, cex.axis = 1.6,
     xlab = "Coefficients", cex.lab = 1.75,
     yaxt = "n",  ylab = "")
abline(v= 0, lty = 2)

## quadratic
int_8 <- grconvertY(links[[8]]$from_y, from = "ndc", to = "user") # I(etio_lag16^2) # varying  SE3.varycoef[13]

## interactions
int_1 <- grconvertY(links[[1]]$from_y, from = "ndc", to = "user") # nino_lag25 -> aao_lag50 # base  SE3.coef[13]
int_2 <- grconvertY(links[[2]]$from_y, from = "ndc", to = "user") # nino_lag25 -> aao_lag50 # constant  SE3.constcoef[13]
int_3 <- grconvertY(links[[3]]$from_y, from = "ndc", to = "user") # wtio_lag38 -> aao_lag37 # varying  SE3.varycoef[15]
int_4 <- grconvertY(links[[4]]$from_y, from = "ndc", to = "user") # etio_lag16 -> aao_lag1 # base  SE3.coef[11]
int_5 <- grconvertY(links[[5]]$from_y, from = "ndc", to = "user") # etio_lag16 -> aao_lag1 # constant  SE3.constcoef[11]
int_6 <- grconvertY(links[[6]]$from_y, from = "ndc", to = "user") # etio_lag33 -> tsa_lag22 # base  SE3.coef[12]
int_7 <- grconvertY(links[[7]]$from_y, from = "ndc", to = "user") # etio_lag33 -> tsa_lag22 # constant  SE3.constcoef[12]

int_9 <- grconvertY(links[[9]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> aao_lag1 # base   SE3.coef[10]
int_10 <- grconvertY(links[[10]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> aao_lag1 # constant  SE3.constcoef[10]
int_11 <- grconvertY(links[[11]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> etio_lag33 # base  SE3.coef[12]
int_12 <- grconvertY(links[[12]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> etio_lag33 # constant  SE3.constcoef[12]
int_13 <- grconvertY(links[[13]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> olr_lag6 # base  SE3.coef[15]
int_14 <- grconvertY(links[[14]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> olr_lag6 # constant   SE3.constcoef[15]
int_15 <- grconvertY(links[[15]]$from_y, from = "ndc", to = "user") # tsa_lag22 -> aao_lag37 # varying  SE3.varycoef[14]
int_16 <- grconvertY(links[[16]]$from_y, from = "ndc", to = "user") # aao_lag1 -> tsa_lag22 # base  SE3.coef[10]
int_17 <- grconvertY(links[[17]]$from_y, from = "ndc", to = "user") # aao_lag1 -> tsa_lag22 # constant  SE3.constcoef[10]
int_18 <- grconvertY(links[[18]]$from_y, from = "ndc", to = "user") # aao_lag1 -> etio_lag16 # base  SE3.coef[11]
int_19 <- grconvertY(links[[19]]$from_y, from = "ndc", to = "user") # aao_lag1 -> etio_lag16 # constant  SE3.constcoef[11]
int_20 <- grconvertY(links[[20]]$from_y, from = "ndc", to = "user") # aao_lag50 -> nino_lag25 # base  SE3.coef[13]
int_21 <- grconvertY(links[[21]]$from_y, from = "ndc", to = "user") # aao_lag50 -> nino_lag25 # constant  SE3.constcoef[13]
int_22 <- grconvertY(links[[22]]$from_y, from = "ndc", to = "user") # aao_lag50 -> olr_lag6 # base  SE3.coef[14]
int_23 <- grconvertY(links[[23]]$from_y, from = "ndc", to = "user") # aao_lag50 -> olr_lag6 # constant   SE3.constcoef[14]
int_24 <- grconvertY(links[[24]]$from_y, from = "ndc", to = "user") # aao_lag37 -> tsa_lag22 # varying  SE3.varycoef[14]
int_25 <- grconvertY(links[[25]]$from_y, from = "ndc", to = "user") # aao_lag37 -> wtio_lag38 # varying  SE3.varycoef[15]
int_26 <- grconvertY(links[[26]]$from_y, from = "ndc", to = "user") # olr_lag6 -> aao_lag50 # base   SE3.coef[14]
int_27 <- grconvertY(links[[27]]$from_y, from = "ndc", to = "user") # olr_lag6 -> aao_lag50 # constant  SE3.constcoef[14]
int_28 <- grconvertY(links[[28]]$from_y, from = "ndc", to = "user") # olr_lag6 -> tsa_lag22 # base  SE3.coef[15]
int_29 <- grconvertY(links[[29]]$from_y, from = "ndc", to = "user") # olr_lag6 -> tsa_lag22 # constant  SE3.constcoef[15]

## segments
# nino_lag25 : aao_lag50
# base
int_pt1 <- (int_1 + int_20)/2
segments(SE3.coef[13], int_1, SE3.coef[13], int_pt1, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[13], int_20, SE3.coef[13], int_pt1, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt2 <- (int_2 + int_21)/2
segments(SE3.constcoef[13], int_2, SE3.constcoef[13], int_pt2, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[13], int_21, SE3.constcoef[13], int_pt2, col = "magenta4", lty = 2, lwd = 2)
# wtio_lag38 : aao_lag37 
# varying
int_pt3 <- (int_3 + int_25)/2
segments(SE3.varycoef[15], int_3, SE3.varycoef[15], int_pt3, col = "darkorange2", lty = 2, lwd = 2)
segments(SE3.varycoef[15], int_25, SE3.varycoef[15], int_pt3, col = "darkorange2", lty = 2, lwd = 2)
# etio_lag16 : aao_lag1
# base
int_pt4 <- (int_4 + int_18)/2
segments(SE3.coef[11], int_4, SE3.coef[11], int_pt4, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[11], int_18, SE3.coef[11], int_pt4, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt5 <- (int_5 + int_19)/2
segments(SE3.constcoef[11], int_5, SE3.constcoef[11], int_pt5, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[11], int_19, SE3.constcoef[11], int_pt5, col = "magenta4", lty = 2, lwd = 2)
# etio_lag33 : tsa_lag22 
# base
int_pt6 <- (int_6 + int_11)/2
segments(SE3.coef[12], int_6, SE3.coef[12], int_pt6, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[12], int_11, SE3.coef[12], int_pt6, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt7 <- (int_7 + int_12)/2
segments(SE3.constcoef[12], int_7, SE3.constcoef[12], int_pt7, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[12], int_12, SE3.constcoef[12], int_pt7, col = "magenta4", lty = 2, lwd = 2)
# tsa_lag22 : aao_lag1
# base
int_pt8 <- (int_9 + int_16)/2
segments(SE3.coef[10], int_9, SE3.coef[10], int_pt8, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[10], int_16, SE3.coef[10], int_pt8, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt9 <- (int_10 + int_17)/2
segments(SE3.constcoef[10], int_10, SE3.constcoef[10], int_pt9, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[10], int_17, SE3.constcoef[10], int_pt9, col = "magenta4", lty = 2, lwd = 2)
# tsa_lag22 : olr_lag6 
# base
int_pt10 <- (int_13 + int_28)/2
segments(SE3.coef[15], int_13, SE3.coef[15], int_pt10, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[15], int_28, SE3.coef[15], int_pt10, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt11 <- (int_14 + int_29)/2
segments(SE3.constcoef[15], int_13, SE3.constcoef[15], int_pt11, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[15], int_29, SE3.constcoef[15], int_pt11, col = "magenta4", lty = 2, lwd = 2)
# tsa_lag22 : aao_lag37 
# varying
int_pt12 <- (int_15 + int_24)/2
segments(SE3.varycoef[14], int_15, SE3.varycoef[14], int_pt12, col = "darkorange2", lty = 2, lwd = 2)
segments(SE3.varycoef[14], int_24, SE3.varycoef[14], int_pt12, col = "darkorange2", lty = 2, lwd = 2)
# aao_lag50 : olr_lag6 
# base
int_pt13 <- (int_22 + int_26)/2
segments(SE3.coef[14], int_22, SE3.coef[14], int_pt13, col = "forestgreen", lty = 2, lwd = 2)
segments(SE3.coef[14], int_26, SE3.coef[14], int_pt13, col = "forestgreen", lty = 2, lwd = 2)
# constant
int_pt14 <- (int_23 + int_27)/2
segments(SE3.constcoef[14], int_23, SE3.constcoef[14], int_pt14, col = "magenta4", lty = 2, lwd = 2)
segments(SE3.constcoef[14], int_27, SE3.constcoef[14], int_pt14, col = "magenta4", lty = 2, lwd = 2)

# quad points
points(SE3.varycoef[13], int_8,  pch = 25, col = "grey4",
       bg = alpha("darkorange2",.65), cex = 2) 
# interaction points
points(SE3.coef[13], int_pt1,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[13], int_pt2,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE3.varycoef[15], int_pt3,  pch = 11, col = alpha("darkorange2",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE3.coef[11], int_pt4,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[11], int_pt5,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE3.coef[12], int_pt6,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[12], int_pt7,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE3.coef[10], int_pt8,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[10], int_pt9,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE3.coef[15], int_pt10,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[15], int_pt11,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 
points(SE3.varycoef[14], int_pt12,  pch = 11, col = alpha("darkorange2",.99),
       bg = alpha("darkorange2",.95), cex = 1.9) 
points(SE3.coef[14], int_pt13,  pch = 11, col = alpha("forestgreen",.99),
       bg = alpha("forestgreen",.95), cex = 1.9) 
points(SE3.constcoef[14], int_pt14,  pch = 11, col = alpha("magenta4",.99),
       bg = alpha("magenta4",.95), cex = 1.9) 

#link to x 
links[[1]]$to_x <- grconvertX(SE3.coef[13], from = "user", to = "ndc")
links[[2]]$to_x <- grconvertX(SE3.constcoef[13], from = "user", to = "ndc")
links[[3]]$to_x <- grconvertX(SE3.varycoef[15], from = "user", to = "ndc")
links[[4]]$to_x <- grconvertX(SE3.coef[11], from = "user", to = "ndc")
links[[5]]$to_x <- grconvertX(SE3.constcoef[11], from = "user", to = "ndc")
links[[6]]$to_x <- grconvertX(SE3.coef[12], from = "user", to = "ndc")
links[[7]]$to_x <- grconvertX(SE3.constcoef[12], from = "user", to = "ndc")
links[[8]]$to_x <- grconvertX(SE3.varycoef[13], from = "user", to = "ndc")
links[[9]]$to_x <- grconvertX(SE3.coef[10], from = "user", to = "ndc")
links[[10]]$to_x <- grconvertX(SE3.constcoef[10], from = "user", to = "ndc")
links[[11]]$to_x <- grconvertX(SE3.coef[12], from = "user", to = "ndc")
links[[12]]$to_x <- grconvertX(SE3.constcoef[12], from = "user", to = "ndc")
links[[13]]$to_x <- grconvertX(SE3.coef[15], from = "user", to = "ndc")
links[[14]]$to_x <- grconvertX(SE3.constcoef[15], from = "user", to = "ndc")
links[[15]]$to_x <- grconvertX(SE3.varycoef[14], from = "user", to = "ndc")
links[[16]]$to_x <- grconvertX(SE3.coef[14], from = "user", to = "ndc")
links[[17]]$to_x <- grconvertX(SE3.constcoef[14], from = "user", to = "ndc")
links[[18]]$to_x <- grconvertX(SE3.coef[11], from = "user", to = "ndc")
links[[19]]$to_x <- grconvertX(SE3.constcoef[11], from = "user", to = "ndc")
links[[20]]$to_x <- grconvertX(SE3.coef[13], from = "user", to = "ndc")
links[[21]]$to_x <- grconvertX(SE3.constcoef[13], from = "user", to = "ndc")
links[[22]]$to_x <- grconvertX(SE3.coef[14], from = "user", to = "ndc")
links[[23]]$to_x <- grconvertX(SE3.constcoef[14], from = "user", to = "ndc")
links[[24]]$to_x <- grconvertX(SE3.varycoef[14], from = "user", to = "ndc")
links[[25]]$to_x <- grconvertX(SE3.varycoef[15], from = "user", to = "ndc")
links[[26]]$to_x <- grconvertX(SE3.coef[14], from = "user", to = "ndc")
links[[27]]$to_x <- grconvertX(SE3.constcoef[14], from = "user", to = "ndc")
links[[28]]$to_x <- grconvertX(SE3.coef[15], from = "user", to = "ndc")
links[[29]]$to_x <- grconvertX(SE3.constcoef[15], from = "user", to = "ndc")


for (i in 1:length(links)) {
  links[[i]]$to_y <- links[[i]]$from_y  # same y to keep it horizontal
}


# --- Draw horizontal linking lines ---
par(xpd = NA)  # allow drawing outside plot regions
colors <- c("forestgreen", "magenta4", "darkorange2", 
            rep( c("forestgreen", "magenta4"), 2), "darkorange2",
            rep( c("forestgreen", "magenta4"), 3), "darkorange2",
            rep( c("forestgreen", "magenta4"), 4), "darkorange2", "darkorange2",
            rep( c("forestgreen", "magenta4"), 2))
linetypes <- rep(2, 29)


for (i in 1:length(links)) {
  grid.lines(
    x = unit(c(links[[i]]$from_x, links[[i]]$to_x), "npc"),
    y = unit(c(links[[i]]$from_y, links[[i]]$to_y), "npc"),
    gp = gpar(col = colors[i], lwd = 1.75, lty = linetypes[i])
  )
}

mtext("Coefficients", side = 2, outer = TRUE, padj = 0.5, cex = 1.25)

dev.off()








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


