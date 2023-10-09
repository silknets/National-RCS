# National Fishes Vulnerability Assessment Project - "ARC_RCS Calculation.R"
# Revised by Sam Silknetter, 10June2021

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

PATH_MW <- "/home/silknets/GitHub/National-RCS/PRISM/Moving_Window/"

# Import standard deviations for climatic data (indicate climate breath for each species).
Climate_SD <- read.csv(file = paste0(PATH_MW, "Climate_SD.csv"), row.names = 1)

# Scale standard deviations for each climatic variable (n=5) between 0 and 1.
Climate_SD$ppt_scaled <- ((Climate_SD$ppt - min(Climate_SD$ppt))
                          /(max(Climate_SD$ppt, na.rm=TRUE) - min(Climate_SD$ppt, na.rm=TRUE)))
Climate_SD$tmax_scaled <- ((Climate_SD$tmax - min(Climate_SD$tmax))
                          /(max(Climate_SD$tmax, na.rm=TRUE) - min(Climate_SD$tmax, na.rm=TRUE)))
Climate_SD$tmin_scaled <- ((Climate_SD$tmin - min(Climate_SD$tmin))
                          /(max(Climate_SD$tmin, na.rm=TRUE) - min(Climate_SD$tmin, na.rm=TRUE)))
Climate_SD$tmaxAug_scaled <- ((Climate_SD$tmaxAug - min(Climate_SD$tmaxAug))
                          /(max(Climate_SD$tmaxAug, na.rm=TRUE) - min(Climate_SD$tmaxAug, na.rm=TRUE)))
Climate_SD$tminJan_scaled <- ((Climate_SD$tminJan - min(Climate_SD$tminJan))
                          /(max(Climate_SD$tminJan, na.rm=TRUE) - min(Climate_SD$tminJan, na.rm=TRUE)))

# Read in AOO dataframe as RCS Data. Set row.names to species and remove empty column 1. 
RCS_Data <- read.csv(file = "/home/silknets/AOO Output_07June2021.csv", row.names = 2)[-1]

# Calculate the mean of the 5 scaled standard deviations to derive a mean relative climate breath for each species.
RCS_Data$CS <- ((Climate_SD$ppt_scaled + Climate_SD$tmax_scaled +
  Climate_SD$tmin_scaled + Climate_SD$tmaxAug_scaled + Climate_SD$tminJan_scaled)/5)

# Scale watershed AOO values between 0 and 1.
RCS_Data$AOO <- ((RCS_Data$huc12_area - min(RCS_Data$huc12_area))
  /(max(RCS_Data$huc12_area, na.rm=TRUE) - min(RCS_Data$huc12_area, na.rm=TRUE)))

# Subtract the scaled AOO values from 1 (so that low values = geographic commonness, high values = rarity).
RCS_Data$AOO_adj <- (1 - RCS_Data$AOO)
RCS_Data$CS_adj <- (1 - RCS_Data$CS)

# Calculate Relative Climate Sensitivity (RCS) Index by taking average of "1-AOO" and "1-CS",
#   where large values of RCS indicate species with small AOO and low range of climate variables.
RCS_Data$RCS <- ((RCS_Data$AOO_adj + RCS_Data$CS_adj)/2)

# Export RCS data table.
write.csv(RCS_Data, file = "/home/silknets/NFVAP/RCS_10June2021.csv")