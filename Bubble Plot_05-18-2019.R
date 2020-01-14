 
########       Rarity Ranking Figures      ########
########   Last updated 18May2019 by SCS   ########


## Set source code
## Working directory not necessary now that I'm using a Project file 

source('biostats.R', encoding = 'UTF-8')
source("colorRampPaletteAlpha.R")

## Load packages
  require(vegan)
  require(simba)
  require(cluster)
  require(pvclust)
  require(ecodist)
  require(MASS)
  require(plotrix)


  
# FISHES
  
# read in data
# MAKE SURE species as sorted with smallest to largest values for rarity
Fishes <- read.csv("FinalFish.csv", header=TRUE) #Must order list by RCS (low -> high), how can I automate this process?
  
  
  # Dot plots for species rarity rankings

  pdf("Fishes_RCS_DotPlot.pdf", width=10, height=20)
  
  rarecol <- addalpha("dodgerblue1", 0.4)
  dotchart(Fishes$RCS, xlim=c(1,-0.03), font=3, labels = Fishes$Species, cex=0.8, pch=21, bg=rarecol, lcolor="black", xlab = "RCS Value")
  
  for (i in 1:nrow(Fishes)){
    lines(x=c(Fishes$RCS[i]-Fishes$RCS.sd[i],Fishes$RCS[i]+Fishes$RCS.sd[i]), y=c(i,i), lwd=1.5, col="black")
  }
  
  dev.off()