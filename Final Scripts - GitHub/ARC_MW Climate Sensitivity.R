# National Fishes Vulnerability Assessment Project - "ARC_MW Climate Sensitivity.R"
# Revised by Sam Silknetter, 09June2021
# This code generates standard deviations of climate variables using a 30-year moving window.

library(prism)
library(zoo)
library(tidyverse)
library(rgdal)
library(sf)
library(rgeos)
library(dplyr)
library(Matrix.utils)
library(Hmisc)


# Set data paths for input data. 
PATH_Occupied_HUC12 <- "/home/silknets/GitHub/National-RCS/Occupied_HUCs/"
PATH_MW_Data <- "/home/silknets/GitHub/National-RCS/PRISM/Moving_Window/"

# Create a list of input shapefiles (or just unique occupied HUCs?) for loop.
FILES_watershed <- list.files(path=PATH_Occupied_HUC12)

# Read in moving window climate data for all variables and ensure HUC-12 ID is a 12-digit character.
ppt_MW <- read.csv(file = paste0(PATH_MW_Data, "ppt_MW_09June2021.csv"), header=TRUE, check.names = FALSE)
ppt_MW <- ppt_MW[,-1]
ppt_MW$huc12 <- as.character(ppt_MW$huc12)
ppt_MW$huc12 <- if_else(nchar(ppt_MW$huc12)==11, paste0("0", ppt_MW$huc12), ppt_MW$huc12)

tmax_MW <- read.csv(file = paste0(PATH_MW_Data, "tmax_MW_09June2021.csv"), header=TRUE, check.names = FALSE)
tmax_MW <- tmax_MW[,-1]
tmax_MW$huc12 <- as.character(tmax_MW$huc12)
tmax_MW$huc12 <- if_else(nchar(tmax_MW$huc12)==11, paste0("0", tmax_MW$huc12), tmax_MW$huc12)

tmin_MW <- read.csv(file = paste0(PATH_MW_Data, "tmin_MW_09June2021.csv"), header=TRUE, check.names = FALSE)
tmin_MW <- tmin_MW[,-1]
tmin_MW$huc12 <- as.character(tmin_MW$huc12)
tmin_MW$huc12 <- if_else(nchar(tmin_MW$huc12)==11, paste0("0", tmin_MW$huc12), tmin_MW$huc12)

tmaxAug_MW <- read.csv(file = paste0(PATH_MW_Data, "tmaxAug_MW_09June2021.csv"), header=TRUE, check.names = FALSE)
tmaxAug_MW <- tmaxAug_MW[,-1]
tmaxAug_MW$huc12 <- as.character(tmaxAug_MW$huc12)
tmaxAug_MW$huc12 <- if_else(nchar(tmaxAug_MW$huc12)==11, paste0("0", tmaxAug_MW$huc12), tmaxAug_MW$huc12)

tminJan_MW <- read.csv(file = paste0(PATH_MW_Data, "tminJan_MW_09June2021.csv"), header=TRUE, check.names = FALSE)
tminJan_MW <- tminJan_MW[,-1]
tminJan_MW$huc12 <- as.character(tminJan_MW$huc12)
tminJan_MW$huc12 <- if_else(nchar(tminJan_MW$huc12)==11, paste0("0", tminJan_MW$huc12), tminJan_MW$huc12)

# Use pivot_longer() to switch from wide to long format across the five variables (a:e)
a <- ppt_MW %>% 
  pivot_longer(-huc12, names_to='year', values_to='ppt')
b <- tmax_MW %>% 
  pivot_longer(-huc12, names_to='year', values_to='tmax')
c <- tmin_MW %>% 
  pivot_longer(-huc12, names_to='year', values_to='tmin')
d <- tmaxAug_MW %>% 
  pivot_longer(-huc12, names_to='year', values_to='tmaxAug')
e <- tminJan_MW %>% 
  pivot_longer(-huc12, names_to='year', values_to='tminJan')

# Join climate variables into a single, static data frame. 
PRISM_MW <- a %>%
  right_join(b, by=c("huc12","year"))%>%
  right_join(c, by=c("huc12","year"))%>%
  right_join(d, by=c("huc12","year"))%>%
  right_join(e, by=c("huc12","year"))

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Climate_SD <- data.frame(matrix(NA, nrow = 139, ncol = 5))
colnames(Climate_SD) <- c('ppt','tmax','tmin','tmaxAug','tminJan')
row.names(Climate_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-4))

# For loop to populate a dataframe of climate variables for each species's AOO. 
i=1
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-4)
  spp_occ <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  names(spp_occ)[names(spp_occ) == "huc.name"] <- "huc12" # Make sure colnames match.
  spp_occ$year <- as.numeric(spp_occ$year)
  spp_occ$year <- if_else(spp_occ$year<1924, 1924, spp_occ$year)
  spp_occ$year <- if_else(spp_occ$year>2017, 2017, spp_occ$year)
  spp_occ$year <- as.character(spp_occ$year)
  spp_ppt_MW <- PRISM_MW %>% 
    right_join(spp_occ, by=c("huc12","year"))
  j=1
  for(j in 1:5){
    Climate_SD[i,j] <- sapply(spp_ppt_MW[,2+j], sd, na.rm = TRUE)
  }
  write.csv(spp_ppt_MW, file = paste0(PATH_MW_Data, "National/", dat.name, ".csv"), row.names = F)
}

# Write CSV of climate variable (N=5) standard devaitions for all focal species. 
write.csv(Climate_SD, file = paste0(PATH_MW_Data, "Climate_SD.csv"), row.names = T)