
#functions used for cleaning data

# week53_avg()
#avg data from week 52 and 53, then overwrite week 52
##we need to create a uniform set of weeks
## data.df: only the columns with data
## data.week53: indices for epiweek(date)=53
week53_avg <- function(data.df, data.week53){
  
  #get the locations epiweek 53,
  for (j in data.week53) {
    temp.52 <- data.df[j-1, ]
    temp.53 <- data.df[j, ]
    
    data.df[j-1, ] <- colMeans(rbind(temp.52, temp.53))
  }

  return(data.df)
}



#functions for producing lag prediction matrices/dfs
pred_lags <- function(){
  
}
