
#some info theory explorations into our withheld models

#import models and data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/modeldata.rda") #resp/pred data
load("Data/lagdata.rda") #lagged data


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


#basic MDS

#for 2-D relationship of lagged predictors
dvar.etio <- dist(t(as.matrix(SEpreds.peak.etio)), method = "euclidean")

fit2 <- cmdscale(dvar.etio, k = 2, eig = TRUE)
coords2 <- fit2$points  # p_vars x 2

# Plot
plot(coords2, asp = 1, xlab = "MDS1", ylab = "MDS2", pch = 16)
text(coords2, labels = colnames(SEpreds.peak.etio), cex = 0.7)
abline(h = 0, v = 0, col = "gray80")



#for 2-D relationship of years
Dtime <- dist(as.matrix(SEpreds.peak.etio), "euclidean")  # now distance between years
fit2 <- cmdscale(Dtime, k = 2, eig = TRUE)

plot(fit2$points, pch = 16, xlab = "MDS1", ylab = "MDS2")
text(fit2$points, labels = rownames(SEpreds.peak.etio), pos = 3, cex = 0.7)




#TODO: fix this lazy code
### INPUT ASSUMPTION:
### X: n_years x p_vars numeric matrix
### rownames(X) are years (e.g., "2005", "2006", ...)
### colnames(X) are variable names

mds_vars <- function(X, k = 2, dist_method = "euclidean", scale_cols = TRUE) {
  if (scale_cols) X <- scale(X)
  D <- dist(t(X), method = dist_method)      # variables as rows
  fit <- cmdscale(D, k = k, eig = FALSE)
  fit
}

# Procrustes alignment: align Y to X (both n x k)
procrustes_align <- function(Xref, Y) {
  Xc <- scale(Xref, center = TRUE, scale = FALSE)
  Yc <- scale(Y,    center = TRUE, scale = FALSE)
  
  # Find rotation R minimizing ||Yc R - Xc||_F
  M <- t(Yc) %*% Xc
  sv <- svd(M)
  R <- sv$u %*% t(sv$v)
  
  Y_aligned <- Yc %*% R
  
  # Match the reference translation (put back at Xref mean)
  Y_aligned <- sweep(Y_aligned, 2, colMeans(Xref), "+")
  Y_aligned
}


X.etio <- as.matrix(SEpreds.peak.etio)
row.names(X.etio) <- season.years[1:20]

### 1) Full-data MDS (reference)
coords_all <- mds_vars(X.etio, k = 2, dist_method = "euclidean", scale_cols = TRUE)
# Ensure rownames are variable names for easier matching
rownames(coords_all) <- colnames(X.etio)

### 2) Leave-one-year-out, pick a year to remove
year_drop <- "2019"   # <-- change to whatever year label you have
keep_idx  <- rownames(X.etio) != year_drop
X_drop    <- X.etio[keep_idx, , drop = FALSE]

coords_drop <- mds_vars(X_drop, k = 2, dist_method = "euclidean", scale_cols = TRUE)
rownames(coords_drop) <- colnames(X.etio)

### 3) Align leave-one-out coords to full-data coords
coords_drop_aligned <- procrustes_align(coords_all, coords_drop)

### 4) Plot: full-data points + leave-one-out points + connecting segments
# Plot window that includes both
xy_all  <- coords_all
xy_drop <- coords_drop_aligned

xlim <- range(c(xy_all[,1], xy_drop[,1]))
ylim <- range(c(xy_all[,2], xy_drop[,2]))

plot(xy_all, type = "n", asp = 1, xlab = "MDS1", ylab = "MDS2",
     xlim = xlim, ylim = ylim)
abline(h = 0, v = 0, col = "gray85")

# Full-data points
points(xy_all, pch = 16, cex = 0.8)
text(xy_all, labels = rownames(xy_all), pos = 3, cex = 0.6)

# Dropped-year points (different symbol)
points(xy_drop, pch = 1, cex = 0.9)

# Segments connecting same variable
segments(x0 = xy_all[,1], y0 = xy_all[,2],
         x1 = xy_drop[,1], y1 = xy_drop[,2])

legend("topleft",
       legend = c("All years", paste0("Leave out ", year_drop)),
       pch = c(16, 1), bty = "n")




#TODO: clean up another lazy code drop


X <- as.matrix(SEpreds.peak.etio)
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
years_drop <- c("2019", "2010")  # order matters for the path, but data are cumulative
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
xlim <- range(all_mat[,1]); ylim <- range(all_mat[,2])

plot(coords_all, type="n", asp=1, xlab="MDS1", ylab="MDS2", xlim=xlim, ylim=ylim)
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
