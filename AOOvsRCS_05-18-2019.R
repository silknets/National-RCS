#Edited 18May2019 by SCS. All RCS .csv files are from GoogleDrive, at this link:
#https://drive.google.com/open?id=1bTcSBhwrCxNNLwAAWG9pxEE29AWlDX02
#Link is to Analysis & Results, RCS Index Folder

rm(list = ls())

library("ggpubr")
library("dplyr")
library("RColorBrewer")
source('colorRampPaletteAlpha.R') #available in GitHub and here: http://www.everydayanalytics.ca/2014/03/colorRampPalette-alpha-in-R.html

# HUC-8 Plot
HUC8_Data <- read.csv("RCS_HUC8.csv")
HUC8 <- HUC8_Data$HUC8_Scale
RCS8 <- HUC8_Data$RCS
CS8 <- 1-HUC8_Data$CS

h8 <- "chocolate2" 
ha8 <- addalpha(h8, 0.7)

par(mar=c(5,5,0,1))
plot(HUC8, CS8, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="HUC-8 Watershed AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)
points(HUC8, CS8, pch=22, bg=ha8, cex=RCS8*2)


# HUC-12 Plot
HUC12_Data <- read.csv("RCS_HUC12.csv")
HUC12 <- HUC12_Data$HUC12_Scale
RCS12 <- HUC12_Data$RCS
CS12 <- 1-HUC12_Data$CS

h12 <- "cornflowerblue"
ha12 <- addalpha(h12, 0.7)

par(mar=c(5,5,0,1))
plot(HUC12, CS12, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="1km Watershed AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)
points(HUC12, CS12, pch=22, bg=ha12, cex=RCS12*2)


# 1km Buffer Plot
km1_Data <- read.csv("RCS_1km.csv")
km1 <- km1_Data$Buffer1km_Scale
RCS1 <- km1_Data$RCS
CS1 <- 1-km1_Data$CS

b1 <-"cyan3"
ba1 <- addalpha(b1, 0.7)

par(mar=c(5,5,0,1))
plot(km1, CS1, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="1km Buffer AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)+
  points(km1, CS1, pch=22, bg=ba1, cex=RCS1*2)

# 10km Buffer Plot
km10_Data <- read.csv("RCS_10km.csv")
km10 <- km10_Data$Buffer10km_Scale
RCS10 <- km10_Data$RCS
CS10 <- 1-km10_Data$CS

b10 <-"darkgoldenrod2"
ba10 <-addalpha(b10, 0.7)

par(mar=c(5,5,0,1))
plot(km10, CS10, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="10km Buffer AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)
points(km10, CS10, pch=22, bg=ba10, cex=RCS10*2)

# Code below is for a 4-panel plot of the above. Tweaks to margins may be needed
par(mfrow=c(2,2))
plot(km1, CS1, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="1km Buffer AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)+
  points(km1, CS1, pch=22, bg=ba1, cex=RCS1*2)
plot(km10, CS10, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="10km Buffer AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)+
  points(km10, CS10, pch=22, bg=ba10, cex=RCS10*2)
plot(HUC8, CS8, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="HUC-8 Watershed AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)+
  points(HUC8, CS8, pch=22, bg=ha8, cex=RCS8*2)
plot(HUC12, CS12, xlim=c(0,1), ylim=c(0,1), type="n",
     xlab="HUC-12 Watershed AOO", ylab="CS Index", cex.axis=1.5, cex.lab=1.5, las=1)+
  points(HUC12, CS12, pch=22, bg=ha12, cex=RCS12*2)

### Pooled Data and SD ####

HUC8_Data <- read.csv("RCS_HUC8.csv")
huc8 <- HUC8_Data[44:48]
colnames(huc8) <- c("CS.8", "Scaled.8",	"One_Minus_Scaled.8",	"One_Minus_CS.8",	"RCS.8")

HUC12_Data <- read.csv("RCS_HUC12.csv")
huc12 <- HUC12_Data[44:48]
colnames(huc12) <- c("CS.12", "Scaled.12",	"One_Minus_Scaled.12",	"One_Minus_CS.12",	"RCS.12")

km1_Data <- read.csv("RCS_1km.csv")
k1 <- km1_Data[50:54]
colnames(k1) <- c("CS.1", "Scaled.1",	"One_Minus_Scaled.1",	"One_Minus_CS.1",	"RCS.1")

km10_Data <- read.csv("RCS_10km.csv")
k10 <- km10_Data[50:54]
colnames(k10) <- c("CS.10", "Scaled.10",	"One_Minus_Scaled.10",	"One_Minus_CS.10",	"RCS.10")

Pooled <- cbind(huc8, huc12, k1, k10) #bind/append? all data to a single object
Pooled2 <- Pooled[,c(1,6,11,16,2,7,12,17,5,10,15,20)]
Pooled2$CS <- colMeans(Pooled2[,1:4])
Pooled2$AOO <- colMeans(Pooled2[,5:8])
Pooled2$RCS <- colMeans(Pooled2[,9:12]) 
write.csv(Pooled2, file = "Pooled Fish Data.csv") # # saved as a .csv, then relevant output is added to "FinalFish.csv", 
# which is the final list where N=128 spp. This is not an elegant solution, and I've also calculated SD of RCS in excel
# rather than in code (see below). I assume this will need to be automated in our final code.  

######## Unsuccessful SD Attempt
library(matrixStats)
Pooled2$CS.sd <- rowSds(Pooled2[,1:4], dim. =1,4)
set.seed(128)
M.CS <- as.matrix(data=Pooled2[,1:4], rnorm(128),ncol = 4, nrow = 128)
colSds(M.CS)

