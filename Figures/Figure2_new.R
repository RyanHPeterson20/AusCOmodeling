
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
#summary(SE.const.LM$`2019-2020`[[2]])
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
SE.cexpts <- ifelse(SE.pch %in% c(15, 16 ,17), 2.25, 1.90)



setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "new_fig2.png", width = 3400, height = 4000, res = 300)
par(mar = c(4, 10, 3, 2), oma = c(0.5, 2.0, 0, 0))  # wider left margin for term labels
plot(SEcoef.vec, seq_along(SEcoef.vec), type = "n",
     xlim = c(-max.ci, max.ci) * 0.98,
     ylim = c(1.75, length(SEcoef.vec) - 1.75),
     yaxt = "n", ylab = "", xlab = "Estimate", xaxt = "n",
     pch = SE.pch, col = SE.col, 
     bg =  SE.bg, cex.lab = 1.68, lwd = 2)
title("Figure 2", adj = 0, cex.main = 2)

rect(-7, 0.5+(2*(0:19)), 7, 1.5+(2*(0:19)), col = alpha("gray78",0.5), border = NA)
#rect(-7, 2.5, 7, 3.5, col = alpha("gray78",0.5), border = NA)
#rect(-7, 4.5, 7, 5.5, col = alpha("gray78",0.5), border = NA)

# Reference line at zero
abline(v = 0, lty = 2, col = "grey24")
abline(h = 15, lty = 1, col = "grey36")
abline(h = 25, lty = 1, col = "grey36")
abline(v = c(-6:-1, 1:6), lty = 2, col = "grey66")

#y-axis label
mtext("Early Season", side = 2, cex = 1.65, 
      adj = 0.825, padj = 0, line = 0.45, outer = TRUE)
mtext("Peak Season", side = 2, cex = 1.65, 
      padj = 0, line = 0.45, outer = TRUE)
mtext("Late Season", side = 2, cex = 1.65, 
      adj = 0.20, padj = 0, line = 0.45, outer = TRUE)

# X-axis
axis(1, at = -6:6, cex.axis = 1.35)

# Y-axis with term names
axis(2, at = seq_along(SEcoef.vec), 
     labels = do.call(expression, SEterms.vec), las = 1, cex.axis = 1.05)
# Confidence intervals as segments
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec), y1 = seq_along(SEcoef.vec),
         col = "grey12", lwd = 2.0, lty = 2)


cap <- 0.2  # half-height of the cap
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 1],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 2.0)
segments(x0 = SEci.vec[, 2], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 2.0)

#points
points(SEcoef.vec, seq_along(SEcoef.vec),
       pch = SE.pch, col = SE.col,  bg = "white",
       cex = SE.cexpts, lwd = 2.5)


legend("topright", 
       title = "Robust",
       legend = c("Main",
                  "Square",
                  "Interaction"),
       inset =  c(0.0, 0),
       pch = c(16, 15, 17),
       title.cex = 1.45, title.adj = 0.125,
       pt.cex = 1.75,
       cex = 1.4, x.intersp = 2,
       xpd = TRUE)
legend("topright", 
       title = "Not-Robust",
       legend = c("Main",
                  "Square",
                  "Interaction"),
       inset =  c(0.0, 0.115),
       pch = c(21, 22, 24),
       col = "black", pt.lwd = 2,
       pt.bg =  "white",
       title.cex = 1.45, title.adj = 0.125,
       pt.cex = 1.5,
       cex = 1.4, x.intersp = 2,
       xpd = TRUE)

#add in legend segments 95\% CI
lg <- legend("topright",
             legend = "95% CI",
             inset =  c(0.0, 0.232),
             bty = "o",        # draw the box
             pch = NA,         # no default symbol
             lty = 0,         # no default line
             cex = 1.36, x.intersp = 3.78)
lg
# lg$rect gives you: left, top, w (width), h (height)
x_left  <- lg$rect$left
x_right <- lg$rect$left + lg$rect$w
y_top   <- lg$rect$top
y_mid   <- lg$rect$top - lg$rect$h / 2  # vertical center of legend

# Now draw your error bar centered inside the legend box
x_center <- x_left + lg$rect$w * 0.25    # tweak multiplier to taste
half_w   <- lg$rect$w * 0.18
cap      <- lg$rect$h * 0.14
#legend segents
segments(x0 = x_center - half_w, x1 = x_center + half_w,
         y0 = y_mid, y1 = y_mid,
         col = "grey12", lwd = 2.0, lty = 2)

# Tick caps
segments(x0 = c(x_center - half_w, x_center + half_w),
         x1 = c(x_center - half_w, x_center + half_w),
         y0 = y_mid - cap, y1 = y_mid + cap,
         col = "grey12", lwd = 2.0)


dev.off()

