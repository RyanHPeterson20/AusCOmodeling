
#Hierarchical Clustering for Aus CO response anoms

#libraries
suppressMessages( library(adespatial))
suppressMessages( library(smacof)) #MDS function (not cmdscale)

#load data
setwd("~/CO_AUS/AusCOmodeling") 
load("Data/matrixdata.rda") #data as matrix
load("Data/modeldata.rda") #resp/pred data


#-- setup --#
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

#get data setup
NEresp.mat <- scale(resp.matrix[,1:29], center = TRUE, scale = FALSE)
SEresp.mat <- scale(resp.matrix[,30:58], center = TRUE, scale = FALSE)



#main
NEdist.euc <- dist(t(NEresp.mat), method = "euclidean")
SEdist.euc <- dist(t(SEresp.mat), method = "euclidean")

#clustering
NEhc.ward <- constr.hclust(NEdist.euc, method = "ward.D2", chron = TRUE)
SEhc.ward <- constr.hclust(SEdist.euc, method = "ward.D2", chron = TRUE)

#groups
NEcut3_base <- cutree(NEhc.ward, k = 3)
SEcut3_base <- cutree(SEhc.ward, k = 3)

#re-order cluster
NEhc.ward$order <- c(1:29)
SEhc.ward$order <- c(1:29)


#figures
#TODO: update figures to align with paper/SI style
setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SI_NEclust_dend.png", width = 3600, height = 1200, res = 300)
par(mar = c(5, 5, 4, 0))
stats:::plot.hclust(NEhc.ward,  hang = -1, labels = FALSE, bty = "n", sub = "", ylab = "Height",
                    xlab = "Week", axes = FALSE, cex.lab = 1.25, main = "", lwd = 1.5)
title("NE Aus : Response Cluster Dendrogram", adj = 0, cex.main = 1.33)
axis(2)
axis(1, at = 1:29, labels = c(season.weeks[c(1:29)]), 
     las = 3, gap.axis = 0, tck = 0.5, lty = 0, cex.axis = 1.1)
rect(xleft = 0.5, xright = 9.5, ybottom = 0, ytop = 67, col = rgb(1, 0, 0, 0.22), border = NA)
rect(xleft = 9.5, xright = 14.5, ybottom = 0, ytop = 67, col = rgb(0, 1, 0, 0.22), border = NA)
rect(xleft = 14.5, xright = 29.5, ybottom = 0, ytop = 67, col = rgb(0, 0, 1, 0.22), border = NA)
text(5, -5, "Early", col = "black", cex = 1.1, xpd = TRUE)
text(12, -5, "Peak", col = "black", cex = 1.1, xpd = TRUE)
text(22, -5, "Late", col = "black", cex = 1.1, xpd = TRUE)
dev.off()


setwd("~/CO_AUS/AusCOmodeling/Supporting_Information/SI_Figures")
png(filename = "SI_SEclust_dend.png", width = 3600, height = 1200, res = 300)
par(mar = c(5, 5, 4, 0))
stats:::plot.hclust(SEhc.ward,  hang = -1, labels = FALSE, bty = "n", sub = "", ylab = "Height",
                    xlab = "Week", axes = FALSE, cex.lab = 1.25, main = "", lwd = 1.5)
title("SE Aus : Response Cluster Dendrogram", adj = 0, cex.main = 1.33)
axis(2)
axis(1, at = 1:29, labels = c(season.weeks[c(1:29)]), 
     las = 3, gap.axis = 0, tck = 0.5, lty = 0, cex.axis = 1.1)
rect(xleft = 0.5, xright = 13.5, ybottom = 0, ytop = 70, col = rgb(1, 0, 0, 0.22), border = NA)
rect(xleft = 13.5, xright = 17.5, ybottom = 0, ytop = 70, col = rgb(0, 1, 0, 0.22), border = NA)
rect(xleft = 17.5, xright = 29.5, ybottom = 0, ytop = 70, col = rgb(0, 0, 1, 0.22), border = NA)
text(7, -5, "Early", col = "black", cex = 1.1, xpd = TRUE)
text(15.5, -5, "Peak", col = "black", cex = 1.1, xpd = TRUE)
text(23.5, -5, "Late", col = "black", cex = 1.1, xpd = TRUE)
dev.off()



#do some multidimensional scaling here, this might not be used in the paper
##classic mds
NE.mds <- cmdscale(NEdist.euc, k =2, eig = TRUE)
SE.mds <- cmdscale(SEdist.euc, k =2, eig = TRUE)

group.col <- c("firebrick2",
               "forestgreen",
               "royalblue3")

#NE Aus MDS
plot(NE.mds$points, pch = 16, col = group.col[NEcut3_base], 
     xlab = "Dim 1", ylab = "Dim 2")
title("NE Aus : Multidimensional Scaling", adj = 0)

#SE Aus MDS
plot(SE.mds$points, pch = 16, col = group.col[SEcut3_base],
     ylim = c(-25, 25), xlim = c(-37, 37),
     xlab = "Dim 1", ylab = "Dim 2")
title("SE Aus : Multidimensional Scaling", adj = 0)

