# National Fishes Vulnerability Assessment Project - "RCS Dot Plot Code.R"
# Revised by Sam Silknetter, 29June2020

# This code is used only to generate the dot plot figure for the Relative Climate Sensitivity index (RCS). 
#  Can be modified to plot multiple RCS scores for each species. 

# Install and load necessary libraries and source files.
library(lattice)
library(vegan)
library(simba)
library(cluster)
library(pvclust)
library(ecodist)
library(plotrix)
library(MASS)
library(RColorBrewer)
source('colorRampPaletteAlpha.R')

# Read in RCS data
Fishes <- read.csv("RCS_05June2020.csv")
  
# RCS Dotplot code
pdf("RCS_DotPlot_29June2020.pdf", width=10, height=20)
# Make sure species are sorted by RCS values small:large (choose only one grain size). 
Fishes <- Fishes[order(Fishes[,22]),] #sorted by RCS_WS
Fishes$Y <- rank(Fishes$RCS_WS)
rarecol <- addalpha("dodgerblue1", 0.4)
rarecol2 <- addalpha("orange", 0.4)
dotchart(Fishes[,22], labels = Fishes[,1], xlim=c(1,-0.03), font=3, cex=0.8, pch=21, bg=rarecol, xlab = "RCS Value")
points(Fishes[1:144,23], Fishes[1:144,24], cex=0.8, pch=21, bg=rarecol2, type = "p")
dev.off()
