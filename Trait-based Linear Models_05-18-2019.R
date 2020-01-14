rm(list=ls()) # clears workspace

library(ggplot2)
library(dplyr)
library(MASS)
library(betareg)
library(emmeans)
library(effects)
library(multcomp)
library(ggpubr)
library(RColorBrewer)

FishData = read.csv("FinalFish.csv") #Load new data file with additional status info

FishData$Status <- as.factor(FishData$Status)
is.factor(FishData$Status)




#### Linear Models for Identifying Significant Relationships between RCS and Trait Data
# Age at maturity (in years)
MAT.l <- log10(FishData$MATUAGE)
mod_a <- lm(RCS ~ MAT.l, data=FishData)
summary(mod_a) # not significant p=0.113
shapiro.test(resid(mod_a)) # indicates that the data is non-normal p=5.842e-05
# Rerun using beta distribution for RCS response:
mod_a<- betareg(RCS ~ MAT.l, data=FishData)
summary(mod_a)
shapiro.test(resid(mod_a)) # data has now been 'normalized' p=0.1616, and thus all further models use betareg()

# Fish longevity (in years)
LONG.l <- log10(FishData$LONGEVITY)
mod_b <- betareg(RCS ~ LONG.l, data=FishData)
summary(mod_b) # Significant, p=0.0291*
Plot.longevity <- ggplot(data=FishData, aes(x=LONG.l, y=RCS))+ 
  geom_point(na.rm = TRUE)+
  stat_smooth(method = "glm", na.rm = TRUE)+
  theme_bw() + 
  xlab("log(10) Longevity")+
  ylab("RCS Index")+
  theme(axis.text.x=element_text(size=10, color='black'))+ 
  theme(panel.grid = element_blank())+
  theme(axis.text.y=element_text(size=10, color='black'))+  
  theme(panel.background = element_rect(colour = 'black', fill = 'white'))+
  theme(axis.title=element_text(size=16,face="bold"))
Plot.longevity

# Female Fecundity (# eggs total)
FEC.l <- log10(FishData$FECUNDITY)
mod_c <- betareg(RCS ~ FEC.l, data=FishData)
summary(mod_c) # Significant, p=0.0011**
Plot.fecundity <- ggplot(data=FishData, aes(x=FEC.l, y=RCS))+ 
  geom_point()+
  stat_smooth(method = "glm")+
  theme_bw() + 
  xlab("log(10) Fecundity")+
  ylab("RCS Index")+
  theme(axis.text.x=element_text(size=10, color='black'))+ 
  theme(panel.grid = element_blank())+
  theme(axis.text.y=element_text(size=10, color='black'))+  
  theme(panel.background = element_rect(colour = 'black', fill = 'white'))+
  theme(axis.title=element_text(size=16,face="bold"))
Plot.fecundity

# Mamimum reported total length
MAX.l <- log10(FishData$MAXTL)
mod_d <- betareg(RCS ~ MAX.l, data=FishData)
summary(mod_d) # Significant, p<0.0002***
Plot.MAXTL <- ggplot(data=FishData, aes(x=MAX.l, y=RCS))+ 
  geom_point()+
  stat_smooth(method = "glm")+
  theme_bw() + 
  xlab("log(10) Maximum Total Length")+
  ylab("RCS Index")+
  theme(axis.text.x=element_text(size=10, color='black'))+ 
  theme(panel.grid = element_blank())+
  theme(axis.text.y=element_text(size=10, color='black'))+  
  theme(panel.background = element_rect(colour = 'black', fill = 'white'))+
  theme(axis.title=element_text(size=16,face="bold"))
Plot.MAXTL

MAXTL = FishData$MAXTL
FECUNDITY = FishData$FECUNDITY
LONGEVITY = FishData$LONGEVITY
cor.test(FECUNDITY, LONGEVITY, data=FishData, method="pearson")
## Pearson's correlation = 0.82 for longevity and MAXTL, so only keep MAXTL (plus fecundity)




#### Additive Model with best trait predictor only (l.MAXTL)
FishData$l.MAXTL <- log10(FishData$MAXTL)
mod_1 <- betareg(RCS ~ l.MAXTL + AOO, data=FishData) #area of occupancy
summary(mod_1) #MAXTL retained as significant predictor
newdat = expand.grid(l.MAXTL = seq(min(FishData$l.MAXTL),max(FishData$l.MAXTL),by=.1),
                     AOO = c(0.05, 0.25,0.5,0.75))
newdat$yhat=predict(mod_1, newdata = newdat) #add fitted values to FishData

Plot1 <- ggplot(data=FishData, aes(x=l.MAXTL, y=RCS))+
  geom_point(aes(color=AOO), size=2.5)+
  geom_line(data=subset(newdat, AOO==0.05),aes(x=l.MAXTL, y=yhat,col=AOO), size=1)+
  geom_line(data=subset(newdat, AOO==0.25),aes(x=l.MAXTL, y=yhat,col=AOO), size=1)+
  geom_line(data=subset(newdat, AOO==0.5),aes(x=l.MAXTL, y=yhat,col=AOO), size=1)+
  geom_line(data=subset(newdat, AOO==0.75),aes(x=l.MAXTL, y=yhat,col=AOO), size=1)+
  theme_bw() + 
  theme(legend.position="right")+
  xlab("log(10) Maximum Total Length")+
  ylab("RCS Index")+
  theme(axis.text.x=element_text(size=10, color='black'))+ 
  theme(panel.grid = element_blank())+
  theme(axis.text.y=element_text(size=10, color='black'))+  
  theme(panel.background = element_rect(colour = 'black', fill = 'white'))+
  theme(axis.title=element_text(size=16,face="bold"))

mid<-(FishData$AOO=0.5)
Plot1+scale_color_gradient2(midpoint=mid, low="lawngreen", mid="cyan3",
                            high="royalblue1", space ="Lab" )