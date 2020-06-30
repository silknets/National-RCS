# National Fishes Vulnerability Assessment Project - "ARC_Climate Sensitivity and RCS Code_SCS.R"
# Revised by Sam Silknetter, 29June2020

# This code calculates Climate Sensitivity (CS) from standard deviation values, merges prior calculations into
#  an RCS output, and calculates Relative Climate Sensitivity index (RCS) for each species and range metric. 

# Install and load necessary libraries.
library(scales)
library(readr)
library(tidyverse)
library(rgbif)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(adehabitatHR)

#1a. Import standard deviations for climatic data (indicate climate breath for each species).
Ppt_SD <- read.csv(file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Ppt_SD.csv", row.names = 1)
Tmax_SD <- read.csv(file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Tmax_SD.csv", row.names = 1)
Tmin_SD <- read.csv(file = "/home/silknets/NFVAP/PRISM/Tmin_Results/tmin_SD.csv", row.names = 1)
TmaxAug_SD <- read.csv(file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/tmaxAug_SD.csv", row.names = 1)
TminJan_SD <- read.csv(file = "/home/silknets/NFVAP/PRISM/TminJan_Results/tminJan_SD.csv", row.names = 1)

#1b. Scale standard deviations for each climatic variable (n=5) and grain (n=2) between 0 and 1.
Ppt_SD$BUF_ppt_scaled <- ((Ppt_SD$Buffer - min(Ppt_SD$Buffer))
 /(max(Ppt_SD$Buffer, na.rm=TRUE) - min(Ppt_SD$Buffer, na.rm=TRUE)))
Ppt_SD$WS_ppt_scaled <- ((Ppt_SD$Watershed - min(Ppt_SD$Watershed))
 /(max(Ppt_SD$Watershed, na.rm=TRUE) - min(Ppt_SD$Watershed, na.rm=TRUE)))

Tmax_SD$BUF_tmax_scaled <- ((Tmax_SD$Buffer - min(Tmax_SD$Buffer))
                          /(max(Tmax_SD$Buffer, na.rm=TRUE) - min(Tmax_SD$Buffer, na.rm=TRUE)))
Tmax_SD$WS_tmax_scaled <- ((Tmax_SD$Watershed - min(Tmax_SD$Watershed))
                          /(max(Tmax_SD$Watershed, na.rm=TRUE) - min(Tmax_SD$Watershed, na.rm=TRUE)))

Tmin_SD$BUF_tmin_scaled <- ((Tmin_SD$Buffer - min(Tmin_SD$Buffer))
                          /(max(Tmin_SD$Buffer, na.rm=TRUE) - min(Tmin_SD$Buffer, na.rm=TRUE)))
Tmin_SD$WS_tmin_scaled <- ((Tmin_SD$Watershed - min(Tmin_SD$Watershed))
                          /(max(Tmin_SD$Watershed, na.rm=TRUE) - min(Tmin_SD$Watershed, na.rm=TRUE)))

TmaxAug_SD$BUF_tmaxAug_scaled <- ((TmaxAug_SD$Buffer - min(TmaxAug_SD$Buffer))
                          /(max(TmaxAug_SD$Buffer, na.rm=TRUE) - min(TmaxAug_SD$Buffer, na.rm=TRUE)))
TmaxAug_SD$WS_tmaxAug_scaled <- ((TmaxAug_SD$Watershed - min(TmaxAug_SD$Watershed))
                          /(max(TmaxAug_SD$Watershed, na.rm=TRUE) - min(TmaxAug_SD$Watershed, na.rm=TRUE)))

TminJan_SD$BUF_tminJan_scaled <- ((TminJan_SD$Buffer - min(TminJan_SD$Buffer))
                          /(max(TminJan_SD$Buffer, na.rm=TRUE) - min(TminJan_SD$Buffer, na.rm=TRUE)))
TminJan_SD$WS_tminJan_scaled <- ((TminJan_SD$Watershed - min(TminJan_SD$Watershed))
                          /(max(TminJan_SD$Watershed, na.rm=TRUE) - min(TminJan_SD$Watershed, na.rm=TRUE)))


#1c. Read in AOO dataframe as RCS Data.
RCS_Data <- read.csv(file = "/home/silknets/NFVAP/AOO Output_03April2020.csv", row.names = 2)
RCS_Data <- RCS_Data[,-1]

#1d. Calculate the mean of the 5 scaled standard deviations to derive a mean relative climate breath for each species.
RCS_Data$BUF_CS <- ((Tmax_SD$BUF_tmax_scaled + Ppt_SD$BUF_ppt_scaled 
                     + TmaxAug_SD$BUF_tmaxAug_scaled + Tmin_SD$BUF_tmin_scaled 
                     + TminJan_SD$BUF_tminJan_scaled)/5)

RCS_Data$WS_CS <- ((Tmax_SD$WS_tmax_scaled + Ppt_SD$WS_ppt_scaled 
                    + TmaxAug_SD$WS_tmaxAug_scaled + Tmin_SD$WS_tmin_scaled 
                    + TminJan_SD$WS_tminJan_scaled)/5)

#1e. Scale AOO values/grain between 0 and 1.
RCS_Data$WS_AOO_scaled <- ((RCS_Data$huc12_area - min(RCS_Data$huc12_area))
  /(max(RCS_Data$huc12_area, na.rm=TRUE) - min(RCS_Data$huc12_area, na.rm=TRUE)))
RCS_Data$BUF_AOO_scaled <- ((RCS_Data$dissolved_buffer_1km - min(RCS_Data$dissolved_buffer_1km))
  /(max(RCS_Data$dissolved_buffer_1km, na.rm=TRUE) - min(RCS_Data$dissolved_buffer_1km, na.rm=TRUE)))

#1g. Subtract the scaled AOO values from 1 (so that low values = commonness, high values = vulnerability.
RCS_Data$AOO_WS_adj <- (1 - RCS_Data$WS_AOO_scaled)
RCS_Data$CS_WS_adj <- (1 - RCS_Data$WS_CS)
RCS_Data$AOO_BUF_adj <- (1 - RCS_Data$BUF_AOO_scaled)
RCS_Data$CS_BUF_adj <- (1 - RCS_Data$BUF_CS)

#1h. Calculate Relative Climate Sensitivity Index by taking average of "1-AOO" and "1-CS".
#  Where large values of RCS indicate species with small AOO and low range of climate variables.
RCS_Data$RCS_WS <- ((RCS_Data$AOO_WS_adj + RCS_Data$CS_WS_adj)/2)
RCS_Data$RCS_BUF <- ((RCS_Data$AOO_BUF_adj + RCS_Data$CS_BUF_adj)/2)

#1i. Exports RCS data table.
write.csv(RCS_Data, file = "/home/silknets/NFVAP/RCS_05June2020.csv")
