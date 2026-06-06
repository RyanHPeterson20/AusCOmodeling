
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
  SEcoef.vec <- c(SEcoef.vec, SE.coefs)
  SEterms.vec <- c(SEterms.vec, SE.terms)
}

max.ci <- max(abs(SEci.vec))



setwd("~/CO_AUS/AusCOmodeling/Figures")
png(filename = "fig2_new.png", width = 2400, height = 4800, res = 300)
par(mar = c(4, 6, 2, 2))  # wider left margin for term labels
plot(SEcoef.vec, seq_along(SEcoef.vec),
     xlim = c(-max.ci, max.ci) * 1.05,
     #ylim = c(0.5, length(SEcoef.vec) + 0.5),
     yaxt = "n", ylab = "", xlab = "Estimate",
     pch = 16, col = "grey12", 
     bg =  alpha("grey70",.65), cex = 1.75,
     main = "Coefficient Plot")

# Y-axis with term names
axis(2, at = seq_along(SEcoef.vec), labels = SEterms.vec, las = 1)
# Confidence intervals as segments
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec), y1 = seq_along(SEcoef.vec),
         col = "grey12", lwd = 1.75, lty = 2)

# Reference line at zero
abline(v = 0, lty = 3, col = "grey40")

cap <- 0.1  # half-height of the cap
segments(x0 = SEci.vec[, 1], x1 = SEci.vec[, 1],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 1.75)
segments(x0 = SEci.vec[, 2], x1 = SEci.vec[, 2],
         y0 = seq_along(SEcoef.vec) - cap, y1 = seq_along(SEcoef.vec) + cap,
         col = "grey12", lwd = 1.75)
dev.off()

