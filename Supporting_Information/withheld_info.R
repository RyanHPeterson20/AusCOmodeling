
#some info theory explorations into our withheld models
library(scatterplot3d)
library(rgl)

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data
load("Data/matrixdata.rda") #data as matrix


#setup
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#TODO: move this to a new R script (or RMD) this is interesting enough in its own right. 
#pred data (kinda hacky, fix later if needed)

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14


SEAus.lag$`Week  51`

SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

#get lag 1-52 from week 51 and lags 1-3 from week 2 (for the peak group)
SEpreds.peak <- SE.pred$mid
SEpreds.peak51 <- SEAus.lag$`Week  51`
SEpreds.peak2 <- SEAus.lag$`Week  2`

#up to lag 52
SEpreds.peak51.nino <- SEpreds.peak51[ ,3:54]
SEpreds.peak51.wtio <- SEpreds.peak51[ ,107:158]
SEpreds.peak51.etio <- SEpreds.peak51[ ,159:210]
SEpreds.peak51.tsa <- SEpreds.peak51[ ,211:262] 
SEpreds.peak51.aao <- SEpreds.peak51[ ,263:314]
SEpreds.peak51.olr <- SEpreds.peak51[ ,315:366]
#only up to lag 3
SEpreds.peak2.nino <- SEpreds.peak2[ ,3:5]
SEpreds.peak2.wtio <- SEpreds.peak2[ ,107:109]
SEpreds.peak2.etio <- SEpreds.peak2[ ,159:161]
SEpreds.peak2.tsa <- SEpreds.peak2[ ,211:213] 
SEpreds.peak2.aao <- SEpreds.peak2[ ,263:265]
SEpreds.peak2.olr <- SEpreds.peak2[ ,315:317]

SEpreds.peak.nino <- cbind(SEpreds.peak2.nino, SEpreds.peak51.nino)
SEpreds.peak.wtio <- cbind(SEpreds.peak2.wtio, SEpreds.peak51.wtio)
SEpreds.peak.etio <- cbind(SEpreds.peak2.etio, SEpreds.peak51.etio)
SEpreds.peak.tsa <- cbind(SEpreds.peak2.tsa, SEpreds.peak51.tsa)
SEpreds.peak.aao <- cbind(SEpreds.peak2.aao, SEpreds.peak51.aao)
SEpreds.peak.olr <- cbind(SEpreds.peak2.olr, SEpreds.peak51.olr)


#start with frequency histograms
par(mfrow = c(3,2))
hist(as.matrix(SEpreds.peak.nino), freq = FALSE, main = "Nino - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.wtio), freq = FALSE, main = "WTIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.etio), freq = FALSE, main = "ETIO - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.tsa), freq = FALSE, main = "TSA - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.aao), freq = FALSE, main = "SAM (AAO) - Peak Group", xlab = "Anomaly")
hist(as.matrix(SEpreds.peak.olr), freq = FALSE, main = "OLR - Peak Group", xlab = "Anomaly")



#TODO: update as to look at the lag of each week in the peak group separately, since we don't quite have a good understanding of this.
## or raw weekly data?? It's hard to tell here. Try both?

#weekly data
nino.std <- scale(pred.matrix[-1, 1:52], center = TRUE, scale = TRUE)
nino.std[,1]

wtio.std <- scale(pred.matrix[-1, 105:156], center = TRUE, scale = TRUE)
wtio.std[,1]

etio.std <- scale(pred.matrix[-1, 157:208], center = TRUE, scale = TRUE)
etio.std[,1]

tsa.std <- scale(pred.matrix[-1, 209:260], center = TRUE, scale = TRUE)
tsa.std[,1]

aao.std <- scale(pred.matrix[-1, 261:312], center = TRUE, scale = TRUE)
aao.std[,1]

olr.std <- scale(pred.matrix[-1, 313:364], center = TRUE, scale = TRUE)
olr.std[,1]




#temp functions for wasserstein distance
wasserstein_1d <- function(x, y, p = 1, n_grid = 2000) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  y <- as.numeric(y); y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  
  u <- (1:n_grid - 0.5) / n_grid
  qx <- as.numeric(quantile(x, probs = u, type = 8, names = FALSE))
  qy <- as.numeric(quantile(y, probs = u, type = 8, names = FALSE))
  
  if (p == 1) mean(abs(qx - qy)) else (mean(abs(qx - qy)^p))^(1/p)
}

# Compare one year to the rest, marginal-by-marginal
year_vs_rest_w1 <- function(X, year, target_year, standardize = TRUE, n_grid = 2000) {
  idx_y <- year == target_year
  idx_r <- !idx_y
  
  Xy <- X[idx_y, , drop = FALSE]
  Xr <- X[idx_r, , drop = FALSE]
  
  if (standardize) {
    mu <- colMeans(Xr, na.rm = TRUE)
    sdv <- apply(Xr, 2, sd, na.rm = TRUE)
    sdv[sdv == 0 | !is.finite(sdv)] <- 1
    Xy <- sweep(sweep(Xy, 2, mu, "-"), 2, sdv, "/")
    Xr <- sweep(sweep(Xr, 2, mu, "-"), 2, sdv, "/")
  }
  
  w <- vapply(seq_len(ncol(X)), function(j) wasserstein_1d(Xy[, j], Xr[, j], p = 1, n_grid = n_grid),
              numeric(1))
  
  list(
    target_year = target_year,
    per_feature_w1 = w,
    mean_w1 = mean(w, na.rm = TRUE),
    median_w1 = median(w, na.rm = TRUE),
    max_w1 = max(w, na.rm = TRUE)
  )
}

# Run for all years and return a summary table
predictor_info_by_year <- function(X, year, standardize = TRUE, n_grid = 2000) {
  yrs <- sort(unique(year))
  res <- lapply(yrs, function(yr) year_vs_rest_w1(X, year, yr, standardize = standardize, n_grid = n_grid))
  
  data.frame(
    year = yrs,
    mean_w1 = vapply(res, `[[`, numeric(1), "mean_w1"),
    median_w1 = vapply(res, `[[`, numeric(1), "median_w1"),
    max_w1 = vapply(res, `[[`, numeric(1), "max_w1"),
    stringsAsFactors = FALSE
  )
}


#TODO: take the above code apart, test to make sure everything works correctly
X <- etio.std
year <- season.years[1:20] #2001 #or do I pass in all years?

#pass into predictor_info_by_year
#predictor_info_by_year() internals

yrs <- sort(unique(year))
#run lapply individually
res <- lapply(yrs, function(yr) year_vs_rest_w1(X, year, yr, standardize = standardize, n_grid = n_grid))



#call year_vs_rest_w1()
#function(X, year, target_year, standardize = TRUE, n_grid = 2000)
target_year <- 2001

idx_y <- year == target_year #selects target yer
idx_r <- !idx_y #selects all other years

Xy <- X[idx_y, , drop = FALSE] #X_y
Xr <- X[idx_r, , drop = FALSE] #X_{-y}

#assume standardized 

#vapply(seq_len(ncol(X)), function(j) wasserstein_1d(Xy[, j], Xr[, j], p = 1, n_grid = n_grid), numeric(1))
##calls function wasserstein_1d() 
#compares Xy[, j], Xr[, j], for each j (different lags of a given climate mode)



tab <- predictor_info_by_year(etio.std, year)
tab[order(tab$mean_w1), ] #gives a weird output

#basic PCA (so that we understand how many dims we need to visualize)

pca.nino <- prcomp(t(nino.std), center = TRUE, scale. = TRUE )
pca.wtio <- prcomp(t(wtio.std), center = TRUE, scale. = TRUE )
pca.etio <- prcomp(t(etio.std), center = TRUE, scale. = TRUE )
pca.tsa <- prcomp(t(tsa.std), center = TRUE, scale. = TRUE )

kmode <- 2
#EOF spatial [np, kmod]
pc_EOF <- pca.etio$rotation[, 1:kmode]

#pca time series
pc_ts <- pca.etio$x[, 1:kmode]

#percent/proportion for each mode
per.nino <- pca.nino$sdev^2 / sum(pca.nino$sdev^2)
per.wtio <- pca.wtio$sdev^2 / sum(pca.wtio$sdev^2)
per.etio <- pca.etio$sdev^2 / sum(pca.etio$sdev^2)
per.tsa <- pca.tsa$sdev^2 / sum(pca.tsa$sdev^2)

sum(per.nino[1:3])
sum(per.wtio[1:3])
sum(per.etio[1:3])
sum(per.tsa[1:3])

par(mfrow = c(2, 2))
plot(1:20, per.nino, type = "l")
plot(1:20, per.wtio, type = "l")
plot(1:20, per.etio, type = "l")
plot(1:20, per.tsa, type = "l")


#basic MDS
dvar.etio <- dist(t(etio.std), method = "euclidean")

fit3 <- cmdscale(dvar.etio, k = 3, eig = TRUE)
coords3 <- fit3$points  # p_vars x 3

col.vec <- rep("gray2", ncol(etio.std))
col.vec[43:45] <- "red3"
col.vec[42] <- "darkorange"
col.vec[46] <- "darkviolet"

open3d()
plot3d(coords3[,1], coords3[,2], coords3[,3],
       col = col.vec, type = "s",
       xlab = "MDS1", ylab = "MDS2", zlab = "MDS3")

scatterplot3d(coords3, pch = 16, color = col.vec)


#for 2-D relationship of lagged predictors
dvar.etio <- dist(t(etio.std), method = "euclidean")

fit2 <- cmdscale(dvar.etio, k = 2, eig = TRUE)
coords2 <- fit2$points  # p_vars x 2

# Plot
plot(coords2, asp = 1, xlab = "MDS1", ylab = "MDS2", pch = 16)
text(coords2, labels = colnames(etio.std), cex = 0.7)
abline(h = 0, v = 0, col = "gray50")




#TODO: clean up another lazy code drop


X <- as.matrix(etio.std)
row.names(X) <- season.years[1:20]
### X: n_years x p_vars matrix
### rownames(X) = years, colnames(X) = variables

mds_vars <- function(X, k = 2, dist_method = "euclidean", scale_cols = TRUE) {
  if (scale_cols) X <- scale(X)
  D <- dist(t(X), method = dist_method)  # variables as points
  fit <- cmdscale(D, k = k, eig = FALSE)
  rownames(fit) <- colnames(X)
  fit
}

procrustes_align <- function(Xref, Y) {
  Xc <- scale(Xref, center = TRUE, scale = FALSE)
  Yc <- scale(Y,    center = TRUE, scale = FALSE)
  
  M  <- t(Yc) %*% Xc
  sv <- svd(M)
  R  <- sv$u %*% t(sv$v)
  
  Y_aligned <- Yc %*% R
  Y_aligned <- sweep(Y_aligned, 2, colMeans(Xref), "+")
  rownames(Y_aligned) <- rownames(Y)
  Y_aligned
}


# ---- 1) Reference: all years
coords_all <- mds_vars(X, k = 2)

# ---- 2) Cumulative drops
years_drop <- c("2019", "2011")  # order matters for the path, but data are cumulative
coords_path <- list(All = coords_all)

X_work <- X
for (yr in years_drop) {
  X_work <- X_work[rownames(X_work) != yr, , drop = FALSE]  # cumulative removal
  coords_tmp <- mds_vars(X_work, k = 2)
  coords_tmp_aligned <- procrustes_align(coords_all, coords_tmp)
  coords_path[[paste0("Drop_", paste(years_drop[1:match(yr, years_drop)], collapse = "_"))]] <-
    coords_tmp_aligned
}

# ---- 3) Plot + chain segments
all_mat <- do.call(rbind, coords_path)
xlim <- range(all_mat[,1])
ylim <- range(all_mat[,2])
max(all_mat[,1])



plot(coords_all, type="n", asp=1, xlab="MDS1", ylab="MDS2", xlim=c(0, 4), ylim=c(0,-3))
abline(h=0, v=0, col="gray85")

# Points
points(coords_all, pch=16, cex=0.9)
text(coords_all, labels=rownames(coords_all), pos=3, cex=0.6)

# Overlay cumulative-drop points with different symbols
nm2 <- names(coords_path)[-1]
pch_seq <- c(1, 2, 0, 5, 6)  # enough symbols if you add more steps
for (i in seq_along(nm2)) {
  points(coords_path[[nm2[i]]], pch=pch_seq[i], cex=0.9)
}

# Chain segments: All -> Drop(2012) -> Drop(2012,2013) -> ...
ordered <- coords_path[c("All", nm2)]
for (i in seq_len(length(ordered) - 1)) {
  A <- ordered[[i]]
  B <- ordered[[i+1]]
  segments(A[,1], A[,2], B[,1], B[,2])
}

legend("topleft",
       legend = c("All years",
                  paste0("Drop ", years_drop[1]),
                  paste0("Drop ", paste(years_drop[1:2], collapse=", "))),
       pch = c(16, pch_seq[1], pch_seq[2]),
       bty = "n")


#get pca to see if 2-dims is enough

