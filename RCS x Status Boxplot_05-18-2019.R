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


#### Boxplot (by Protection/Conservation Status)
#### 0=No Protection, 1=SGCN, 2=Threatened, 3=Endangered

# Tried to reorder, was unsuccessful using code below - thoughts on how to classify?
#FishData$Status <- gsub("0", "None", FishData$Status)
#FishData$Status <- gsub("1", "SGCN", FishData$Status)
#FishData$Status <- gsub("2", "Threatened", FishData$Status)
#FishData$Status <- gsub("3", "Endangered", FishData$Status)
#Boxplot=factor(FishData$Status, levels=c("None", "SGCN", "Threatened", "Endangered"))
Status.mod <- betareg(RCS ~ Status, data=FishData)
summary(Status.mod)
Boxplot<-ggplot(FishData, aes(Status, RCS)) +
  geom_boxplot()+ 
  theme_classic() +
  xlab("Status")+
  ylab("RCS")+
  scale_y_continuous()+
  theme(axis.text.x=element_text(size=18, color='black'))+  
  theme(axis.text.y=element_text(size=14, color='black'))+  
  theme(legend.position="none")+
  theme(axis.line = element_line(colour = 'black', size = 1.0))+
  theme(panel.background = element_rect(colour = 'black', fill = 'white'))+   
  theme(axis.title=element_text(size=20,face="bold"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
Boxplot
