# National Fishes Vulnerability Assessment Project - "ARC_Climate Sensitivity_SCS.R"
# Revised by Sam Silknetter, 29June2020

# This code generates standard deviations of climate variables.

# This code downloads PRISM climate data and requires a HPC.
library(prism)
library(zoo)
library(tidyverse)
library(rgdal)
library(sf)
library(rgeos)
library(dplyr)
library(Matrix.utils)
library(Hmisc)


# Set data paths for input data. # All available through https://github.com/silknets/National-RCS.
PATH_SHP_HUC12_AOO <- "/home/silknets/NFVAP/Shapefiles/HUC12"
PATH_SHP_1km <- "/home/silknets/NFVAP/Shapefiles/Dissolved 1km Buffer"
PATH_SHP_HUC12_All <- "/home/silknets/NFVAP/Shapefiles/HUC12_All.rds"
PATH_PRISM_ppt <- "/home/silknets/NFVAP/PRISM/Ppt/"
PATH_PRISM_tmax <- "/home/silknets/NFVAP/PRISM/Tmax/"
PATH_PRISM_tmin <- "/home/silknets/NFVAP/PRISM/Tmin/"
PATH_PRISM_tmaxAug <- "/home/silknets/NFVAP/PRISM/TmaxAug/"
PATH_PRISM_tminJan <- "/home/silknets/NFVAP/PRISM/TminJan/"
PATH_Occupied_HUC12 <- "/home/silknets/NFVAP/Occupied HUCs/"
PATH_NFVAP <- "/home/silknets/NFVAP/"

# Create a list of input shapefiles for loop.
FILES_watershed <- list.files(path=PATH_SHP_HUC12_AOO, pattern = "\\.shp$")
FILES_buffer <- list.files(path=PATH_SHP_1km, pattern = "\\.shp$")

# Load Watershed shapefiles (as .rds) from path.
HUC12_All <- readRDS(PATH_SHP_HUC12_All)

# Create Mean and Standard Deviation functions to be used in script.
  #Standard Mean and SD functions.
mean_1 <- function(x, na.rm = TRUE) {
  mean(x, na.rm=TRUE)
}

sd_1 = function(x, ...){
  v = var(x, ...)
  l = sum(!is.na(x))
  sqrt(v*(l-1)/l)
}

  # SD function to exclude non-zero values for binary for loop.
sd_2 = function(x, ...){
  v = var(x[!x == 0], ...)
  l = sum(!is.na(x))
  sqrt(v*(l-1)/l)
}


#### PRECIPITATION CALCS ####

# Set path and load 'raster stack' to variable-specific prism data. 
options(prism.path = PATH_PRISM_ppt) 
PRISM_ppt <- prism_stack(ls_prism_data()) # Note: Ensure prism.path is set to correct climate variable

# Mean and Standard Deviation Calculation
mean_ppt <- raster::calc(PRISM_ppt, fun = mean_1, na.rm = TRUE)
  #sd_ppt <- calc(PRISM_ppt, fun = sd_1, na.rm=TRUE)
#saveRDS(mean_ppt, file = "/home/silknets/NFVAP/ppt_mean.rds")
  #saveRDS(sd, file = "/home/silknets/NFVAP/ppt_sd.rds")
#mean_ppt <- readRDS(file = "/home/silknets/NFVAP/ppt_mean.rds")
  #sd_ppt <- readRDS(file = "/home/silknets/NFVAP/ppt_sd.rds")

# Extract and save mean precipitation (ppt) dataframe for all watersheds. 
  #HUC12_ppt_sd <- raster::extract(sd_ppt, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
  #saveRDS(HUC12_ppt_sd, file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Watershed/HUC12_ppt_sd")

#Create a dataframe with all 12-digit HUCs and mean precipitation across years. 
HUC12_ppt_mean <- raster::extract(mean_ppt, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
HUC12_ppt_mean$HUC_ID <- HUC12_All@data$HUC12 
names(HUC12_ppt_mean)[names(HUC12_ppt_mean) == "layer"] <- "mean"
HUC12_ppt_mean <- HUC12_ppt_mean [ , c(3, 2)] 
  saveRDS(HUC12_ppt_mean, file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Watershed/HUC12_ppt_mean")
  HUC12_ppt_mean <- readRDS("/home/silknets/NFVAP/PRISM/Ppt_Results/Watershed/HUC12_ppt_mean")

# Make a binary dataframe of occupied watersheds per species.
Ppt_Watershed_Occupied <- data.frame(matrix(NA, nrow = 100537, ncol = 145))
colnames(Ppt_Watershed_Occupied) <- c("HUC_ID", substr(FILES_watershed, 1, nchar(FILES_watershed)-10))
Ppt_Watershed_Occupied$HUC_ID <- HUC12_ppt_mean$HUC_ID

# For loop to populate binary dataframe. 
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Occupied_HUCs <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  Ppt_Watershed_Occupied[, dat.name] <- as.integer(Ppt_Watershed_Occupied[1:100537,1] %in% Occupied_HUCs$Occupied_HUC)
}

write.csv(Ppt_Watershed_Occupied, file = paste0("/home/silknets/NFVAP/data_test.csv"), row.names=FALSE)

# Use matrix algebra to create a dataframe of SD per species. 
Ppt_SD_table <- Ppt_Watershed_Occupied [,2:145] * c(HUC12_ppt_mean [,2])

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Watershed_Ppt_SD <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Watershed_Ppt_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Watershed_Ppt_SD[, dat.name] <- sd_2(Ppt_SD_table[, dat.name], na.rm=TRUE)
}
saveRDS(Watershed_Ppt_SD, file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Watershed/Watershed_Ppt_SD")

# Extract precipitation data from each species 1km buffer AOO. 
Ppt_Buffer <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Ppt_Buffer) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  spp_BUF <- readOGR(PATH_SHP_1km, dat.name) # Read in dissolved shapefile.
  spp_BUF <- gUnaryUnion(spp_BUF, id=NULL)
  dat.name <- substr(dat.name, 1, nchar(dat.name)-4)
  BUF_ppt_mean <- raster::extract(mean_ppt, spp_BUF, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  BUF_ppt_mean <- na.omit(BUF_ppt_mean) #There should be no NAs, potential AOO issue with lakes?
  unlist(lapply(BUF_ppt_mean, sd_1))
  Ppt_Buffer[, dat.name] <- sqrt(wtd.var(BUF_ppt_mean$layer, BUF_ppt_mean$weight, na.rm = TRUE, normwt = TRUE))
}

write.csv(Ppt_Buffer, file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Buffer/Ppt_Buffer.csv", row.names=FALSE)
Ppt_SD <- Ppt_Buffer [,-145]
Ppt_SD[2,] <- Watershed_Ppt_SD[1,]
row.names(Ppt_SD) <- c("Buffer", "Watershed")
Ppt_SD <- t(Ppt_SD)
write.csv(Ppt_SD, file = "/home/silknets/NFVAP/PRISM/Ppt_Results/Ppt_SD.csv", row.names=TRUE)


#### MAXIMUM TEMPERATURE CALCS ####

# Set path and load 'raster stack' to variable-specific prism data. 
options(prism.path = PATH_PRISM_tmax) 
PRISM_tmax <- prism_stack(ls_prism_data()) # Note: Ensure prism.path is set to correct climate variable

# Mean and Standard Deviation Calculation
mean_tmax <- calc(PRISM_tmax, fun = mean_1, na.rm = TRUE)
#sd_tmax <- calc(PRISM_tmax, fun = sd_1, na.rm=TRUE)
saveRDS(mean_tmax, file = "/home/silknets/NFVAP/Tmax_mean.rds")
#saveRDS(sd_tmax, file = "/home/silknets/NFVAP/Tmax_sd.rds")
#mean_tmax <- readRDS(file = "/home/silknets/NFVAP/Tmax_mean.rds")
#sd_tmax <- readRDS(file = "/home/silknets/NFVAP/tmax_sd.rds")

# Save Mean and SD Tmax rasters for watersheds. 
#HUC12_tmax_sd <- raster::extract(sd_tmax, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
#saveRDS(HUC12_tmax_sd, file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Watershed/HUC12_tmax_sd")

#write.csv(HUC12_tmax_mean, file = paste0(PATH_NFVAP, "tmax_mean_.csv"), row.names=FALSE)

#Create a dataframe with all 12-digit HUCs and mean precipitation across years. 
HUC12_tmax_mean <- raster::extract(mean_tmax, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
HUC12_tmax_mean$HUC_ID <- HUC12_All@data$HUC12 
names(HUC12_tmax_mean)[names(HUC12_tmax_mean) == "layer"] <- "mean"
HUC12_tmax_mean <- HUC12_tmax_mean [ , c(3, 2)] 
saveRDS(HUC12_tmax_mean, file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Watershed/HUC12_tmax_mean")
HUC12_tmax_mean <- readRDS("/home/silknets/NFVAP/PRISM/Tmax_Results/Watershed/HUC12_tmax_mean")

# Make a binary dataframe of occupied watersheds per species.
Tmax_Watershed_Occupied <- data.frame(matrix(NA, nrow = 100537, ncol = 145))
colnames(Tmax_Watershed_Occupied) <- c("HUC_ID", substr(FILES_watershed, 1, nchar(FILES_watershed)-10))
Tmax_Watershed_Occupied$HUC_ID <- HUC12_tmax_mean$HUC_ID

# For loop to populate binary dataframe. 
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Occupied_HUCs <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  Tmax_Watershed_Occupied[, dat.name] <- as.integer(Tmax_Watershed_Occupied[1:100537,1] %in% Occupied_HUCs$Occupied_HUC)
}

write.csv(Tmax_Watershed_Occupied, file = paste0("/home/silknets/NFVAP/data_test_tmax.csv"), row.names=FALSE)

# Use matrix algebra to create a dataframe of SD per species. 
Tmax_SD_table <- Tmax_Watershed_Occupied [,2:145] * c(HUC12_tmax_mean [,2])

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Watershed_Tmax_SD <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Watershed_Tmax_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Watershed_Tmax_SD[, dat.name] <- sd_2(Tmax_SD_table[, dat.name], na.rm=TRUE)
}
saveRDS(Watershed_Tmax_SD, file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Watershed/Watershed_Tmax_SD")

# Extract precipitation data from each species 1km buffer AOO. 
Tmax_Buffer <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Tmax_Buffer) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  spp_BUF <- readOGR(PATH_SHP_1km, dat.name) # Read in dissolved shapefile.
  spp_BUF <- gUnaryUnion(spp_BUF, id=NULL)
  dat.name <- substr(dat.name, 1, nchar(dat.name)-4)
  BUF_tmax_mean <- raster::extract(mean_tmax, spp_BUF, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  BUF_tmax_mean <- na.omit(BUF_tmax_mean) #There should be no NAs, potential AOO issue with lakes?
  unlist(lapply(BUF_tmax_mean, sd_1))
  Tmax_Buffer[, dat.name] <- sqrt(wtd.var(BUF_tmax_mean$layer, BUF_tmax_mean$weight, na.rm = TRUE, normwt = TRUE))
}

write.csv(Tmax_Buffer, file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Buffer/Tmax_Buffer.csv", row.names=FALSE)
Tmax_SD <- Tmax_Buffer [,-145]
Tmax_SD[2,] <- Watershed_Tmax_SD[1,]
row.names(Tmax_SD) <- c("Buffer", "Watershed")
Tmax_SD <- t(Tmax_SD)
write.csv(Tmax_SD, file = "/home/silknets/NFVAP/PRISM/Tmax_Results/Tmax_SD.csv", row.names=TRUE)






#### MINIMUM TEMPERATURE CALCS ####

# Set path and load 'raster stack' to variable-specific prism data. 
options(prism.path = PATH_PRISM_tmin)
PRISM_tmin <- prism_stack(ls_prism_data()) # Note: Ensure prism.path is set to correct climate variable

# Mean and Standard Deviation Calculation
mean_tmin <- calc(PRISM_tmin, fun = mean_1, na.rm = TRUE)
#sd_tmin <- calc(PRISM_tmin, fun = sd_1, na.rm=TRUE)
saveRDS(mean_tmin, file = "/home/silknets/NFVAP/tmin_mean.rds")
#saveRDS(sd_tmin, file = "/home/silknets/NFVAP/tmin_sd.rds")
#mean_tmin <- readRDS(file = "/home/silknets/NFVAP/tmin_mean.rds")
#sd_tmin <- readRDS(file = "/home/silknets/NFVAP/tmin_sd.rds")

# Save Mean and SD Tmax rasters for watersheds. 
#HUC12_tmin_sd <- raster::extract(sd_tmin, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
#saveRDS(HUC12_tmin_sd, file = "/home/silknets/NFVAP/PRISM/Tmin_Results/Watershed/HUC12_tmin_sd")

#write.csv(HUC12_tmin_mean, file = paste0(PATH_NFVAP, "tmin_mean_.csv"), row.names=FALSE)

#Create a dataframe with all 12-digit HUCs and mean precipitation across years. 
HUC12_tmin_mean <- raster::extract(mean_tmin, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
HUC12_tmin_mean$HUC_ID <- HUC12_All@data$HUC12 
names(HUC12_tmin_mean)[names(HUC12_tmin_mean) == "layer"] <- "mean"
HUC12_tmin_mean <- HUC12_tmin_mean [ , c(3, 2)] 
saveRDS(HUC12_tmin_mean, file = "/home/silknets/NFVAP/PRISM/Tmin_Results/Watershed/HUC12_tmin_mean")
HUC12_tmin_mean <- readRDS("/home/silknets/NFVAP/PRISM/Tmin_Results/Watershed/HUC12_tmin_mean")

# Make a binary dataframe of occupied watersheds per species.
tmin_Watershed_Occupied <- data.frame(matrix(NA, nrow = 100537, ncol = 145))
colnames(tmin_Watershed_Occupied) <- c("HUC_ID", substr(FILES_watershed, 1, nchar(FILES_watershed)-10))
tmin_Watershed_Occupied$HUC_ID <- HUC12_tmin_mean$HUC_ID

# For loop to populate binary dataframe. 
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Occupied_HUCs <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  tmin_Watershed_Occupied[, dat.name] <- as.integer(tmin_Watershed_Occupied[1:100537,1] %in% Occupied_HUCs$Occupied_HUC)
}

write.csv(tmin_Watershed_Occupied, file = paste0("/home/silknets/NFVAP/data_test_tmin.csv"), row.names=FALSE)

# Use matrix algebra to create a dataframe of SD per species. 
tmin_SD_table <- tmin_Watershed_Occupied [,2:145] * c(HUC12_tmin_mean [,2])

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Watershed_tmin_SD <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Watershed_tmin_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Watershed_tmin_SD[, dat.name] <- sd_2(tmin_SD_table[, dat.name], na.rm=TRUE)
}
saveRDS(Watershed_tmin_SD, file = "/home/silknets/NFVAP/PRISM/Tmin_Results/Watershed/Watershed_tmin_SD")

# Extract precipitation data from each species 1km buffer AOO. 
tmin_Buffer <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(tmin_Buffer) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  spp_BUF <- readOGR(PATH_SHP_1km, dat.name) # Read in dissolved shapefile.
  spp_BUF <- gUnaryUnion(spp_BUF, id=NULL)
  dat.name <- substr(dat.name, 1, nchar(dat.name)-4)
  BUF_tmin_mean <- raster::extract(mean_tmin, spp_BUF, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  BUF_tmin_mean <- na.omit(BUF_tmin_mean) #There should be no NAs, potential AOO issue with lakes?
  unlist(lapply(BUF_tmin_mean, sd_1))
  tmin_Buffer[, dat.name] <- sqrt(wtd.var(BUF_tmin_mean$layer, BUF_tmin_mean$weight, na.rm = TRUE, normwt = TRUE))
}

write.csv(tmin_Buffer, file = "/home/silknets/NFVAP/PRISM/Tmin_Results/Buffer/tmin_Buffer.csv", row.names=FALSE)
tmin_SD <- tmin_Buffer [,-145]
tmin_SD[2,] <- Watershed_tmin_SD[1,]
row.names(tmin_SD) <- c("Buffer", "Watershed")
tmin_SD <- t(tmin_SD)
write.csv(tmin_SD, file = "/home/silknets/NFVAP/PRISM/Tmin_Results/tmin_SD.csv", row.names=TRUE)





#### AUGUST MAXIMUM TEMPERATURE CALCS ####

# Set path and load 'raster stack' to variable-specific prism data. 
options(prism.path = PATH_PRISM_tmaxAug) 
PRISM_tmaxAug <- prism_stack(ls_prism_data()) # Note: Ensure prism.path is set to correct climate variable

# Mean and Standard Deviation Calculation
mean_tmaxAug <- calc(PRISM_tmaxAug, fun = mean_1, na.rm = TRUE)
#sd_tmaxAug <- calc(PRISM_tmaxAug, fun = sd_1, na.rm=TRUE)
saveRDS(mean_tmaxAug, file = "/home/silknets/NFVAP/tmaxAug_mean.rds")
#saveRDS(sd_tmaxAug, file = "/home/silknets/NFVAP/tmaxAug_sd.rds")
#mean_tmaxAug <- readRDS(file = "/home/silknets/NFVAP/tmaxAug_mean.rds")
#sd_tmaxAug <- readRDS(file = "/home/silknets/NFVAP/tmaxAug_sd.rds")

# Save Mean and SD Tmax rasters for watersheds. 
#HUC12_tmaxAug_sd <- raster::extract(sd_tmaxAug, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
#saveRDS(HUC12_tmaxAug_sd, file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/Watershed/HUC12_tmaxAug_sd")

#write.csv(HUC12_tmaxAug_mean, file = paste0(PATH_NFVAP, "tmaxAug_mean_.csv"), row.names=FALSE)

#Create a dataframe with all 12-digit HUCs and mean precipitation across years. 
HUC12_tmaxAug_mean <- raster::extract(mean_tmaxAug, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
HUC12_tmaxAug_mean$HUC_ID <- HUC12_All@data$HUC12 
names(HUC12_tmaxAug_mean)[names(HUC12_tmaxAug_mean) == "layer"] <- "mean"
HUC12_tmaxAug_mean <- HUC12_tmaxAug_mean [ , c(3, 2)] 
saveRDS(HUC12_tmaxAug_mean, file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/Watershed/HUC12_tmaxAug_mean")
HUC12_tmaxAug_mean <- readRDS("/home/silknets/NFVAP/PRISM/TmaxAug_Results/Watershed/HUC12_tmaxAug_mean")

# Make a binary dataframe of occupied watersheds per species.
tmaxAug_Watershed_Occupied <- data.frame(matrix(NA, nrow = 100537, ncol = 145))
colnames(tmaxAug_Watershed_Occupied) <- c("HUC_ID", substr(FILES_watershed, 1, nchar(FILES_watershed)-10))
tmaxAug_Watershed_Occupied$HUC_ID <- HUC12_tmaxAug_mean$HUC_ID

# For loop to populate binary dataframe. 
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Occupied_HUCs <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  tmaxAug_Watershed_Occupied[, dat.name] <- as.integer(tmaxAug_Watershed_Occupied[1:100537,1] %in% Occupied_HUCs$Occupied_HUC)
}

write.csv(tmaxAug_Watershed_Occupied, file = paste0("/home/silknets/NFVAP/data_test_tmaxAug.csv"), row.names=FALSE)

# Use matrix algebra to create a dataframe of SD per species. 
tmaxAug_SD_table <- tmaxAug_Watershed_Occupied [,2:145] * c(HUC12_tmaxAug_mean [,2])

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Watershed_tmaxAug_SD <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Watershed_tmaxAug_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Watershed_tmaxAug_SD[, dat.name] <- sd_2(tmaxAug_SD_table[, dat.name], na.rm=TRUE)
}
saveRDS(Watershed_tmaxAug_SD, file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/Watershed/Watershed_tmaxAug_SD")

# Extract precipitation data from each species 1km buffer AOO. 
tmaxAug_Buffer <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(tmaxAug_Buffer) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  spp_BUF <- readOGR(PATH_SHP_1km, dat.name) # Read in dissolved shapefile.
  spp_BUF <- gUnaryUnion(spp_BUF, id=NULL)
  dat.name <- substr(dat.name, 1, nchar(dat.name)-4)
  BUF_tmaxAug_mean <- raster::extract(mean_tmaxAug, spp_BUF, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  BUF_tmaxAug_mean <- na.omit(BUF_tmaxAug_mean) #There should be no NAs, potential AOO issue with lakes?
  unlist(lapply(BUF_tmaxAug_mean, sd_1))
  tmaxAug_Buffer[, dat.name] <- sqrt(wtd.var(BUF_tmaxAug_mean$layer, BUF_tmaxAug_mean$weight, na.rm = TRUE, normwt = TRUE))
}

write.csv(tmaxAug_Buffer, file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/Buffer/tmaxAug_Buffer.csv", row.names=FALSE)
tmaxAug_SD <- tmaxAug_Buffer [,-145]
tmaxAug_SD[2,] <- Watershed_tmaxAug_SD[1,]
row.names(tmaxAug_SD) <- c("Buffer", "Watershed")
tmaxAug_SD <- t(tmaxAug_SD)
write.csv(tmaxAug_SD, file = "/home/silknets/NFVAP/PRISM/TmaxAug_Results/tmaxAug_SD.csv", row.names=TRUE)






#### JANUARY MINIMUM TEMPERATURE CALCS ####

# Set path and load 'raster stack' to variable-specific prism data. 
options(prism.path = PATH_PRISM_tminJan) 
PRISM_tminJan <- prism_stack(ls_prism_data()) # Note: Ensure prism.path is set to correct climate variable

# Mean and Standard Deviation Calculation
mean_tminJan <- calc(PRISM_tminJan, fun = mean_1, na.rm = TRUE)
#sd_tminJan <- calc(PRISM_tminJan, fun = sd_1, na.rm=TRUE)
saveRDS(mean_tminJan, file = "/home/silknets/NFVAP/tminJan_mean.rds")
#saveRDS(sd_tminJan, file = "/home/silknets/NFVAP/tminJan_sd.rds")
#mean_tminJan <- readRDS(file = "/home/silknets/NFVAP/tminJan_mean.rds")
#sd_tminJan <- readRDS(file = "/home/silknets/NFVAP/tminJan_sd.rds")

# Save Mean and SD Tmax rasters for watersheds. 
#HUC12_tminJan_sd <- raster::extract(sd_tminJan, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
#saveRDS(HUC12_tminJan_sd, file = "/home/silknets/NFVAP/PRISM/TminJan_Results/Watershed/HUC12_tminJan_sd")

#write.csv(HUC12_tminJan_mean, file = paste0(PATH_NFVAP, "tminJan_mean_.csv"), row.names=FALSE)

#Create a dataframe with all 12-digit HUCs and mean precipitation across years. 
HUC12_tminJan_mean <- raster::extract(mean_tminJan, HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
HUC12_tminJan_mean$HUC_ID <- HUC12_All@data$HUC12 
names(HUC12_tminJan_mean)[names(HUC12_tminJan_mean) == "layer"] <- "mean"
HUC12_tminJan_mean <- HUC12_tminJan_mean [ , c(3, 2)] 
saveRDS(HUC12_tminJan_mean, file = "/home/silknets/NFVAP/PRISM/TminJan_Results/Watershed/HUC12_tminJan_mean")
HUC12_tminJan_mean <- readRDS("/home/silknets/NFVAP/PRISM/TminJan_Results/Watershed/HUC12_tminJan_mean")

# Make a binary dataframe of occupied watersheds per species.
tminJan_Watershed_Occupied <- data.frame(matrix(NA, nrow = 100537, ncol = 145))
colnames(tminJan_Watershed_Occupied) <- c("HUC_ID", substr(FILES_watershed, 1, nchar(FILES_watershed)-10))
tminJan_Watershed_Occupied$HUC_ID <- HUC12_tminJan_mean$HUC_ID

# For loop to populate binary dataframe. 
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Occupied_HUCs <- read.table(file = paste0(PATH_Occupied_HUC12, dat.name, ".txt"), sep = ",", header = T, colClasses = "character")
  tminJan_Watershed_Occupied[, dat.name] <- as.integer(tminJan_Watershed_Occupied[1:100537,1] %in% Occupied_HUCs$Occupied_HUC)
}

write.csv(tminJan_Watershed_Occupied, file = paste0("/home/silknets/NFVAP/data_test_tminJan.csv"), row.names=FALSE)

# Use matrix algebra to create a dataframe of SD per species. 
tminJan_SD_table <- tminJan_Watershed_Occupied [,2:145] * c(HUC12_tminJan_mean [,2])

# Build dataframe to be populated with the SD of watershed precipitation per species. 
Watershed_tminJan_SD <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(Watershed_tminJan_SD) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-10)
  Watershed_tminJan_SD[, dat.name] <- sd_2(tminJan_SD_table[, dat.name], na.rm=TRUE)
}
saveRDS(Watershed_tminJan_SD, file = "/home/silknets/NFVAP/PRISM/TminJan_Results/Watershed/Watershed_tminJan_SD")

# Extract precipitation data from each species 1km buffer AOO. 
tminJan_Buffer <- data.frame(matrix(NA, nrow = 1, ncol = 144))
colnames(tminJan_Buffer) <- c(substr(FILES_watershed, 1, nchar(FILES_watershed)-10))

for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  spp_BUF <- readOGR(PATH_SHP_1km, dat.name) # Read in dissolved shapefile.
  spp_BUF <- gUnaryUnion(spp_BUF, id=NULL)
  dat.name <- substr(dat.name, 1, nchar(dat.name)-4)
  BUF_tminJan_mean <- raster::extract(mean_tminJan, spp_BUF, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  BUF_tminJan_mean <- na.omit(BUF_tminJan_mean) #There should be no NAs, potential AOO issue with lakes?
  unlist(lapply(BUF_tminJan_mean, sd_1))
  tminJan_Buffer[, dat.name] <- sqrt(wtd.var(BUF_tminJan_mean$layer, BUF_tminJan_mean$weight, na.rm = TRUE, normwt = TRUE))
}

write.csv(tminJan_Buffer, file = "/home/silknets/NFVAP/PRISM/TminJan_Results/Buffer/tminJan_Buffer.csv", row.names=FALSE)
tminJan_SD <- tminJan_Buffer [,-145]
tminJan_SD[2,] <- Watershed_tminJan_SD[1,]
row.names(tminJan_SD) <- c("Buffer", "Watershed")
tminJan_SD <- t(tminJan_SD)
write.csv(tminJan_SD, file = "/home/silknets/NFVAP/PRISM/TminJan_Results/tminJan_SD.csv", row.names=TRUE)






#### CAN ABBY EXPLAIN CLEAN FUNCTION?
# Doing the work of the pivot table 
#clean_dataset <- function(df) {
#  df <- as.data.frame(df)
#  df$year <- substr(df$eventDate, start = 1, stop = 4)
#  df$year <- paste0("_", df$year)
#  df_final <- df %>%
#    gather(key = "prism_name", value = "value", 15:134) %>%
#    rowwise() %>%
#    filter(grepl(pattern = year, x = prism_name))
#  return(df_final)
#}

#### SEEMS LIKE A RULE FOR THE MOVING WINDOW?

# Read in .rds files from output and limit the data to only the instances
# where a point occurs in a particular year, eliminating data that do no follow that rule
#PATH_PRISM_ppt_Results <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results"
#filenames <- list.files(path = PATH_PRISM_ppt_Results, full.names = T)
#ldf <- lapply(filenames, readRDS)
#res <- lapply(ldf, clean_dataset)

# Calculate standard deviation on the values for each species and climate variable combination
#### ABBY, ANY SUCCESS WITH THIS CODE?

res <- lapply("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results/Acantharchus pomotis.rds", readRDS)
stdev <- lapply(res[[i]]$value, sd)
stdev <- c()
for (i in 1:length(res)){
  stdev[[i]] <- sd(res[[i]]$value, na.rm = T)# unsure why there are some NAs, not all sp-clim_var have them
  stdev <- list.append(stdev, res[[i]]$name[1])
  stdev <- list.append(stdev, res[[i]]$prism_name[1])
} #need to add in sp and clim_var to list somehow
stdev


###############################

#### CHUNKS BELOW FROM ALB CODE

options(prism.path = "/lustre/projects/css/csas/albenson/prismtmp/ppt")

# Create raster stack for only the years of PRISM data needed
RS <- prism_stack(ls_prism_data()) #raster data
proj4string(RS)<-CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")

#### ANY REASON WHY BUFFERED .SHP WOULDN"T WORK INSTEAD?
# Run 1km buffers for each point in the species shapefile and extract PRISM data within
# those buffers
bufferSize_ls <- 1000
myextractfun <- function(b){
  result <- raster::extract(RS, finalpoints_sp, buffer = b, fun= mean, na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE, sp = TRUE)
}

result <- pbdLapply(bufferSize_ls, myextractfun)
finalize()

# Save result as an RDS
saveRDS(result, file = "ppt_buffers.rds")
