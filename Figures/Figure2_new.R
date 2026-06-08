
#libraries
suppressMessages(library(grid)) #gridlines between plots
suppressMessages( library(scales)) #for adjusting opacity

#data import
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/base_RAMPmodels.rda") #"base" model
load("Data/loyo_models.rda") #leave one year out models/refits

#functions
rename_term <- function(x) {
  # Handle interaction terms (split on ":", process each, rejoin with " x ")
  parts <- strsplit(x, ":")[[1]]
  
  parts <- sapply(parts, function(term) {
    # Match "varname_lagN" pattern
    m <- regmatches(term, regexpr("^(.+?)_lag(\\d+)$", term, perl = TRUE))
    
    if (length(m) == 0) return(term)  # no match, return as-is
    
    # Extract variable name and lag number
    var <- gsub("^(.+?)_lag(\\d+)$", "\\1", term, perl = TRUE)
    lag <- gsub("^(.+?)_lag(\\d+)$", "\\2", term, perl = TRUE)
    
    # Capitalise and format
    var <- toupper(var)
    sprintf("%s (%s)", var, lag)
  })
  
  paste(parts, collapse = " x ")
}



#data setup

#SEmodels
SE1.lm <- SEmodels[[1]]
SE2.lm <- SEmodels[[2]]
SE3.lm <- SEmodels[[3]]

#SEmodels.loyo
SE.const.LM <- SEmodels.loyo[[2]]
SE.vary.LM <- SEmodels.loyo[[3]]

#get model summaries
summary(SE2.lm)
summary(SE.const.LM$`2019-2020`[[2]])
summary(SE.vary.LM$`2019-2020`[[2]])

order.new <- sort( abs(coef(SE2.lm)), decreasing = FALSE)

ord.in <- match(names(order.new), names(coef(SE2.lm)))

SE2.ci    <- confint(SE2.lm)
SE2.coefs <- coef(SE2.lm)

# Drop intercept (usually on a very different scale)
SE2.coefs <- SE2.coefs[ord.in]
SE2.ci    <- SE2.ci[ord.in, ]

SE2.ci    <- SE2.ci[-1, ]
SE2.coefs <- SE2.coefs[-1]
SE2.terms <- names(SE2.coefs)

#update pch values based off a couple of conditions
#TODO: automate
## wtio 14:16; aao 9:21; tsa 29:16; aao 21:21; wtio 46:21; 
## etio 33:21; nino40xetio7:24;  nino 40:21; etio7:16
SE2.pch <- c(16, 21, 16, 21, 21, 21, 24, 21, 16)
SE2.cex <- 1.75

SE2.terms <- sapply(SE2.terms, rename_term)
names(coef(SE.vary.LM$`2019-2020`[[2]]))

par(mar = c(4, 6, 2, 2))  # wider left margin for term labels
plot(SE2.coefs, seq_along(SE2.coefs),
     xlim = range(SE2.ci) * 1.1,
     ylim = c(0.5, length(SE2.coefs) + 0.5),
     yaxt = "n", ylab = "", xlab = "Estimate",
     pch = SE2.pch, col = "grey12", 
     bg =  alpha("grey70",.65), cex = SE2.cex,
     main = "Coefficient Plot")

# Y-axis with term names
axis(2, at = seq_along(SE2.coefs), labels = SE2.terms, las = 1)
# Confidence intervals as segments
segments(x0 = SE2.ci[, 1], x1 = SE2.ci[, 2],
         y0 = seq_along(SE2.coefs), y1 = seq_along(SE2.coefs),
         col = "grey12", lwd = 1.75, lty = 2)

# Reference line at zero
abline(v = 0, lty = 3, col = "grey40")

cap <- 0.1  # half-height of the cap
segments(x0 = SE2.ci[, 1], x1 = SE2.ci[, 1],
         y0 = seq_along(SE2.coefs) - cap, y1 = seq_along(SE2.coefs) + cap,
         col = "grey12", lwd = 1.75)
segments(x0 = SE2.ci[, 2], x1 = SE2.ci[, 2],
         y0 = seq_along(SE2.coefs) - cap, y1 = seq_along(SE2.coefs) + cap,
         col = "grey12", lwd = 1.75)


##
coef(SE3.lm)
coef(SE.vary.LM$`2019-2020`[[1]])


#full plot
SE.lm.list <- list(SE3.lm, SE2.lm, SE1.lm)
SEci.vec <- c()
SEcoef.vec <- c()
SEterms.vec <- c()
for (j in 1:3) {
  lm.tmp <- SE.lm.list[[j]]
  
  SE.ci    <- confint(lm.tmp)
  SE.coefs <- coef(lm.tmp)
  
  SE.ci    <- SE.ci[-1, ]
  SE.coefs <- SE.coefs[-1]
  SE.terms <- names(SE.coefs)
  
  order.new <- sort( abs(SE.coefs), decreasing = FALSE)
  ord.in <- match(names(order.new), SE.terms)
  
  SE.coefs <- SE.coefs[ord.in]
  SE.ci    <- SE.ci[ord.in, ]
  SE.terms <- names(SE.coefs)
  
  SE.terms <- sapply(SE.terms, rename_term)
  
  SEci.vec <- rbind(SEci.vec, SE.ci)
  SEci.vec <- rbind(SEci.vec, c(NA,NA))
  SEcoef.vec <- c(SEcoef.vec, c(SE.coefs, NA))
  SEterms.vec <- c(SEterms.vec, c(SE.terms, NA))
}

SEterms.vec <- as.list(SEterms.vec)
SEterms.vec[[26]] <- bquote("NINO (33)"^2)
SEterms.vec[[25]] <- c("")
SEterms.vec[[15]] <- c("")

max.ci <- max(abs(SEci.vec), na.rm = TRUE)


#update pch and cex
#TODO: automate
SE.pch <- c(24, 21, 16, 24, 24, 24, 21, 21, 24, 16, 21, 24, 16, 16, NA,
            16, 21, 16, 21, 21, 21, 24, 21, 16, NA,
            22, 21, 16, 24, 16, 16, 21, 24, 21, 16, 16, 16, 21, 16, NA)
SE.col <- c(rep("darkgreen", 15), rep("firebrick",  10), rep("royalblue4", 15))
SE.bg <- c(rep(alpha("chartreuse2", 0.40), 15), 
           rep(alpha("coral3", 0.40), 10),
           rep(alpha("royalblue", 0.40), 15))
SE.cex <- c()



setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "fig2_new.png", width = 3200, height = 4000, res = 300)
par(mar = c(4, 10, 3, 2), oma = c(0, 1.5, 0, 0.5))  # wider left margin for term labels
plot(SEcoef.vec, seq_along(SEcoef.vec),
     xlim = c(-max.ci, max.ci) * 0.98,
     ylim = c(1.75, length(SEcoef.vec) - 1.75),
     yaxt = "n", ylab = "", xlab = "Estimate",
     pch = SE.pch, col = SE.col, 
     bg =  SE.bg, cex = 1.5)
title("Figure 2", adj = 0, cex.main = 2)

mtext("Early", side = 2, cex = 1.5, adj = 0.825, padj = 0, line = 0, outer = TRUE)
mtext("Peak", side = 2, cex = 1.5, padj = 0, line = 0, outer = TRUE)
mtext("Late", side = 2, cex = 1.5, adj = 0.20, padj = 0, line = 0, outer = TRUE)
legend("topright", 
       title = "Robust",
       legend = c("Main",
                  "Square",
                  "Interaction"),
       inset =  c(0.0, 0),
       pch = c(16, 15, 17),
       cex = 1.07, x.intersp = 3,
       xpd = TRUE)
legend("topright", 
       title = "Not-Robust",
       legend = c("Main",
                  "Square",
                  "Interaction"),
       inset =  c(0.0, 0.10),
       pch = c(21, 22, 24),
       col = "grey12", 
       pt.bg =  alpha("grey70",.65),
       cex = 1.07, x.intersp = 3,
       xpd = TRUE)

#add in legend segments 95\% CI
lg <- legend("topright",
             legend = "95% CI",
             inset =  c(0.0, 0.20),
             bty = "o",        # draw the box
             pch = NA,         # no default symbol
             lty = 0,         # no default line
             cex = 1.00, x.intersp = 4.75)
lg
# lg$rect gives you: left, top, w (width), h (height)
x_left  <- lg$rect$left
x_right <- lg$rect$left + lg$rect$w
y_top   <- lg$rect$top
y_mid   <- lg$rect$top - lg$rect$h / 2  # vertical center of legend

# Now draw your error bar centered inside the legend box
x_center <- x_left + lg$rect$w * 0.3    # tweak multiplier to taste
half_w   <- lg$rect$w * 0.2
cap      <- lg$rect$h * 0.15

segments(x0 = x_center - half_w, x1 = x_center + half_w,
         y0 = y_mid, y1 = y_mid,
         col = "grey12", lwd = 1.75, lty = 2)

# Tick caps
segments(x0 = c(x_center - half_w, x_center + half_w),
         x1 = c(x_center - half_w, x_center + half_w),
         y0 = y_mid - cap, y1 = y_mid + cap,
         col = "grey12", lwd = 1.75)



# Y-axis with term names
axis(2, at = seq_along(SEcoef.vec), 
     labels = do.call(expression, SEterms.vec), las = 1)
# Confidence intervals as segments
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec), y1 = seq_along(SEcoef.vec),
         col = "grey12", lwd = 1.75, lty = 2)

# Reference line at zero
abline(v = 0, lty = 3, col = "grey40")
abline(h = 15, lty = 1, col = "grey40")
abline(h = 25, lty = 1, col = "grey40")

cap <- 0.1  # half-height of the cap
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 1],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 1.75)
segments(x0 = SEci.vec[, 2], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 1.75)
dev.off()

