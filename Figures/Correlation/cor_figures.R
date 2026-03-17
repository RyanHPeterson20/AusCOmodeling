
#create correlation plots for fig 2 and SI

suppressMessages( library(lubridate))


#data import
#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix
load("Data/lagdata.rda") #lagged data
load("Data/modeldata.rda") #resp/pred data

#load functions
source("Functions/modeling_functions.R")

#setup
#season years/weeks
season.weeks <- c(38:52, 1:14)
season.years <- unique(resp.df$year) #TODO: update response df

seasons <- c()
for (i in 1:(length(season.years)-1)) {
  temp_season <- paste0(season.years[i], "-", season.years[i+1])
  #print(temp_season)  
  seasons <- c(seasons, temp_season)
}
rm(i, temp_season)

#group weeks
SE.early <- 38:50
SE.mid <- c(51, 52, 1, 2)
SE.late <- 3:14

#get data setup
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)

SE.resp <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late) 
SE.pred <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late)

#get correlation here
SEcor_list <- list()
for (j in 1:3) {
  se_df <- as.data.frame(cbind(SE.resp[[j]], SE.pred[[j]]))
  
  nino_cor <- cor(se_df, method = "pearson")[2:53]
  #dmi_cor <- cor(se_df, method = "pearson")[54:105]
  
  wtio_cor <- cor(se_df, method = "pearson")[106:157]
  etio_cor <- cor(se_df, method = "pearson")[158:209]
  
  tsa_cor <- cor(se_df, method = "pearson")[210:261]
  aao_cor <- cor(se_df, method = "pearson")[262:313]
  olr_cor <- cor(se_df, method = "pearson")[314:365]
  
  cor_df <- data.frame(nino_cor, wtio_cor, etio_cor, tsa_cor, aao_cor, olr_cor)
  
  SEcor_list[[paste0("SEAus_", j)]] <- cor_df
}
#105:364

SE.resp.wo2019 <- resp_setup(SEresp.mat, season.weeks, SE.early, SE.mid, SE.late, j = c(1:18, 20)) 
SE.pred.wo2019 <- pred_setup(SEAus.lag, season.weeks, SE.early, SE.mid, SE.late, j = c(1:18, 20))


SEcor_list.wo2019 <- list()
for (j in 1:3) {
  se_df.wo2019 <- as.data.frame(cbind(SE.resp.wo2019[[j]], SE.pred.wo2019[[j]]))
  
  nino_cor <- cor(se_df.wo2019, method = "pearson")[2:53]
  #dmi_cor <- cor(se_df, method = "pearson")[54:105]
  
  wtio_cor <- cor(se_df.wo2019, method = "pearson")[106:157]
  etio_cor <- cor(se_df.wo2019, method = "pearson")[158:209]
  
  tsa_cor <- cor(se_df.wo2019, method = "pearson")[210:261]
  aao_cor <- cor(se_df.wo2019, method = "pearson")[262:313]
  olr_cor <- cor(se_df.wo2019, method = "pearson")[314:365]
  
  cor_df <- data.frame(nino_cor, wtio_cor, etio_cor, tsa_cor, aao_cor, olr_cor)
  
  SEcor_list.wo2019[[paste0("SEAus_", j)]] <- cor_df
}


#new correlation figures
#single cor plots
#all plot cex vals
cex.main <- 2.0
cex.label <- 1.5
cex.num <- 1.25
line.wd <- 2.40

#nino??
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, rev(SEcor_list[[1]]$nino_cor), type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd,
     lty = "4111")
axis(1, at = 1:52, labels = 52:1 , cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
abline(h = 0, lwd = 0.35, col = "gray24")
lines(1:52, rev(SEcor_list[[2]]$nino_cor), col = "cyan4", lty = "F4", lwd = line.wd)
lines(1:52, rev(SEcor_list[[3]]$nino_cor), col = "slateblue4", lty = "8212", lwd = line.wd)
points(52, SEcor_list[[1]]$nino_cor[1], pch = 21, bg = "firebrick", col = "gray4", cex = 1.25)



#single cor plots
#all plot cex vals
cex.main <- 2.0
cex.label <- 1.5
cex.num <- 1.25
line.wd <- 2.40

setwd("~/CO_AUS/AusCOmodeling/Figures/Correlation")

png(filename = "SEcor_nino.png", width = 2600, height = 2400, res = 250)
##nino
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$nino_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$nino_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$nino_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$nino_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$nino_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$nino_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "Ni\u00f1o 3.4", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
dev.off()



png(filename = "SEcor_WTIO.png", width = 2600, height = 2400, res = 250)
##wtio
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$wtio_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$wtio_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$wtio_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$wtio_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$wtio_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$wtio_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "WTIO", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
dev.off()



png(filename = "SEcor_ETIO.png", width = 2600, height = 2400, res = 250)
##etio
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$etio_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$etio_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$etio_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$etio_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$etio_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$etio_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "ETIO", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
#legend()
dev.off()




png(filename = "SEcor_TSA.png", width = 2600, height = 2400, res = 250)
##tsa
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$tsa_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$tsa_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$tsa_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$tsa_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$tsa_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$tsa_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "TSA", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
dev.off()




png(filename = "SEcor_aao.png", width = 2600, height = 2400, res = 250)
##aai
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$aao_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$aao_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$aao_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$aao_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$aao_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$aao_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "SAM (AAO)", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
dev.off()


png(filename = "SEcor_olr.png", width = 2600, height = 2400, res = 250)
##olr
par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$olr_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("Lag",  side = 1, line = 2.5, cex = cex.label)
mtext("Correlation",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$olr_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$olr_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$olr_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$olr_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$olr_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "OLR", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
dev.off()




#combined plot
#all plot cex vals
cex.main <- 2.1
cex.label <- 1.5
cex.num <- 1.5
line.wd <- 2.40

setwd("~/CO_AUS/AusCOmodeling/Figures/Correlation")

png(filename = "SEcor_all.png", width = 5100, height = 2200, res = 250)
par(mfrow = c(2, 3))
par(oma = c(4, 4, 2, 0))  
par(mar = c(2, 2, 1.5, 1.5))
##nino
#par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$nino_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$nino_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$nino_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$nino_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$nino_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$nino_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "Ni\u00f1o 3.4", cex  = cex.main, adj = 0)


##wtio

plot(1:52, SEcor_list[[2]]$wtio_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$wtio_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$wtio_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$wtio_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$wtio_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$wtio_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "WTIO", cex  = cex.main, adj = 0)


#par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$etio_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$etio_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$etio_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$etio_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$etio_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$etio_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "ETIO", cex  = cex.main, adj = 0)
#title("Nino", adj = 0, cex = 2.)
legend("topright", 
      title = "All-Data",
      inset =  c(0.25, 0),
      legend = c("Early", "Peak", "Late"),
      col = c("cyan4", "firebrick", "slateblue4"),
      x.intersp = 3,
      seg.len = 3.5,
      adj = c(0.5, 0.5),
      lty = 1,
      lwd = 2, 
      cex = 1.5)
legend("topright",
       title = "2019/2020 Withheld",
       legend = c("Early", "Peak", "Late"),
       col = c("cyan4", "firebrick", "slateblue4"),
       lty = 4,
       x.intersp = 3,
       seg.len = 3.5,
       adj = c(0.5, 0.5),
       lwd = 2,
       cex = 1.5)

##tsa
#par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$tsa_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$tsa_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$tsa_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$tsa_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$tsa_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$tsa_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "TSA", cex  = cex.main, adj = 0)


##aai
#par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$aao_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$aao_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$aao_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$aao_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$aao_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$aao_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "SAM (AAO)", cex  = cex.main, adj = 0)



##olr
#par(mar = c(4, 4, 1.5, 1.5))
plot(1:52, SEcor_list[[2]]$olr_cor, type = "l", ylim = c(-1,1),
     xaxt= "n", yaxt= "n", xlim = c(1.75, 50.5),
     ylab = "", xlab = "", col = "firebrick", lwd = line.wd)
axis(1, cex.axis = cex.num)
axis(2, at = seq(-1, 1, by = 0.25), labels = c(-1, NA, -0.5, NA, 0, NA, 0.5, NA, 1), cex.axis = cex.num)
mtext("",  side = 1, line = 2.5, cex = cex.label)
mtext("",  side = 2, line = 2.5, cex = cex.label)
lines(1:52, SEcor_list.wo2019[[2]]$olr_cor, col = "firebrick", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[1]]$olr_cor, col = "cyan4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[1]]$olr_cor, col = "cyan4", lty = 4, lwd = line.wd)
lines(1:52, SEcor_list[[3]]$olr_cor, col = "slateblue4", lty = 1, lwd = line.wd)
lines(1:52, SEcor_list.wo2019[[3]]$olr_cor, col = "slateblue4", lty = 4, lwd = line.wd)
abline(h =0, lty = 3, col = "grey30", lwd = 1.5)
text( 2, 0.95, "OLR", cex  = cex.main, adj = 0)

mtext("Correlation", side = 2, cex = cex.label, line = 2, outer = TRUE)
mtext("Lag", side = 1, cex = cex.label, line = 1.5, outer = TRUE)
dev.off()



#other correlation checks
se.peak.df <- as.data.frame(cbind(SE.resp[[2]], SE.pred[[2]]))
nino.corr <- cor(se.peak.df[,1], se.peak.df[,2:53])
cor.test(se.peak.df[,1], se.peak.df[,2])



