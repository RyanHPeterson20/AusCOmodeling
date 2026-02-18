
#functions for pIOD index processing


##interpolate to common grid, requires fields
sst.interp <- function(sst, sst.lon, sst.lat, grid.list){
  
  #TODO: add in fields library check
  
  #get dime
  dim.sst <- dim(sst)
  
  nx <- dim.sst[1]
  ny <- dim.sst[2]
  nt <- dim.sst[3]
  
  #interpolate 
  sst.interp <- NULL
  for (j in seq_len(nt)) {
    sst.obj <-  list(x = sst.lon, y = sst.lat, z = sst[,,j])
    
    temp.interp <- interp.surface.grid(sst.obj, grid.list)
    sst.interp <- abind(sst.interp, temp.interp$z, along = 3)
  }
  
  return(sst.interp)
}

#pca/eof analysis function:
##sst.anom as [time, lat, lon]
sst.eof <- function(sst.anom, kmode){
  
  nt <- dim(sst.anom)[1]
  ny <- dim(sst.anom)[2]
  nx <- dim(sst.anom)[3]
  
  X <- matrix(sst.anom, nrow = nt, ncol = ny * nx)
  
  #get rid of NA's (masked locs)
  keep <- colSums(is.finite(X)) == nt
  X.new  <- X[, keep]
  
  svd.temp <- svd(X.new)
  
  #svd outputs
  U <- svd.temp$u[ ,1:kmode]
  D <- svd.temp$d[1:kmode]
  V <- svd.temp$v[ ,1:kmode]
  
  #outputs
  EOF.temp <- V #EOF spatial pattern
  PC.temp <- U %*% diag(D)
  per.temp <- D^2 / sum(svd.temp$d^2) 
  
  #finalize the spatial eof (pca)
  V_eof <- matrix(NA, nrow = ny * nx, ncol = kmode)
  
  #add in lsm sea data
  V_eof[keep, ] <- EOF.temp
  
  return(list(EOF = V_eof,
              PC = PC.temp,
              percent = per.temp, 
              D = D))
}

