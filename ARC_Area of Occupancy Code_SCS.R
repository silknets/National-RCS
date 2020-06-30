# National Fishes Vulnerability Assessment Project - "ARC_Area of Occupancy Code_SCS.R"
# Revised by Sam Silknetter, 29June2020

# This code calculates Area of Occupancy (AOO) for each species and range metric. 

# Install necessary libraries.
library(raster)
library(rgbif)
library(rgdal)
library(sp)
library(rgeos)
library(scales)

# Set data paths.
PATH_HUC12 <- "/home/silknets/NFVAP/Shapefiles/HUC12_All.rds"
PATH_FocalSpecies_OccurrenceData <- "/home/silknets/NFVAP/Focal Species/"
PATH_SHP_HUC12 <- "/home/silknets/NFVAP/Shapefiles/HUC12"
# PATH_SHP_1km <- "/home/silknets/NFVAP/Shapefiles/1km Buffer" #Should be obsolete! 6/29 SCS
PATH_SHP_dis1km <- "/home/silknets/NFVAP/Shapefiles/Dissolved 1km Buffer/"
PATH_Occupied_HUC12 <- "/home/silknets/NFVAP/Occupied HUCs/"

# Set spatial coordinate reference system (CRS).
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# Read in fixed data.
huc12 <- readRDS(PATH_HUC12)
proj4string(huc12) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
huc12_wgs84 <- spTransform(huc12, crs.geo)

# Create empty AOO data table to be populated.
AOOs <- data.frame(scientific_name = character(), buffer_1km = numeric(), dissolved_buffer_1km = numeric(), 
        huc12_area = numeric(), mean_Rank = numeric(), range = numeric(), sd_Rank = numeric(), StandardizedMean = numeric(), 
        cv = numeric(), logBuffer1km = numeric(), logHUC12 = numeric(), stringsAsFactors = F)

# Create a list of occurrence data files for all focal species. 
FILES <- list.files(path=PATH_FocalSpecies_OccurrenceData)

# Loop to cycle through all focal species (N = 144).
#for(i in 1:1){ ##Hint for testing with one species, set i=1
for(i in 1:length(FILES)){
  geodata <- read.csv(paste(PATH_FocalSpecies_OccurrenceData,"/", FILES[i], sep = ""), header=TRUE)
  species <- geodata$species[1] # Identify the species name for loop outputs 

  # Convert from 'geodata' CSV to an OGR object. 
  dat_csv <- as.data.frame(geodata)
  dat_coord <- dat_csv[c("decimalLongitude", "decimalLatitude")] # Create data frame of longitude and latitude for all entries.
  dat_sp <- SpatialPointsDataFrame(dat_coord, dat_csv, proj4string = crs.geo) # Save spatial dataframe with correct projection.
  
  # Set column 1 in AOOs to species name.
  AOOs[i,1] <- as.character(dat_sp$species[1])  
  
  # Calculate area (km^2) of occupied watersheds per species. 
  dat_sp$HUC12 <- over(dat_sp, huc12_wgs84)$HUC12 # Store the HUC12 name as an attribute of the fish data.
  Number_of_HUC12s <- data.frame(unique(dat_sp$HUC12)) #Identify unique watersheds occupied by species [i]
  names(Number_of_HUC12s) <- "Occupied_HUC"
  
  # Write csv as '.txt' to preserve leading zeroes. Use read.table(file, sep = ",") to read it in later. 
  write.csv(Number_of_HUC12s, file = paste0(PATH_Occupied_HUC12, species, ".txt"), row.names = FALSE) 
  huc12_sp <- huc12[huc12$HUC12 %in% Number_of_HUC12s$unique.dat_sp.HUC12.,]  # Extract unique HUC12 data for the species.
  total_area_huc12 <- sum(huc12_sp$AreaSqKm) # Sum the total area (square kilometer) for each unique HUC12.
  AOOs[i,]$huc12_area <- total_area_huc12
  
  # Generate 1km point buffers. You must use a projected CRS for function gBuffer(). For CRS, we use: USA Contiguous albers equal area. 
  geodata_Albers <- spTransform(dat_sp, crs.albers)
  
  # Create 1 km buffer and calculate the total area occupied per species.
  sp_buffer_1km <- gBuffer(geodata_Albers, width = 1, byid= TRUE)
  AOOs[i,]$buffer_1km <- gArea(sp_buffer_1km, byid = FALSE) #to be deleted, but keeping for now as a check. 
  dissolved_1km_buffer <- gUnaryUnion(sp_buffer_1km, id=NULL)
  AOOs[i,]$dissolved_buffer_1km <- gArea(dissolved_1km_buffer, byid = FALSE)
  
  # Save the spatial dataframes as ESRI Shapefiles. 
  raster::shapefile(huc12_sp, file = paste0(PATH_SHP_HUC12, species, "_HUC12.shp"), overwrite=TRUE) #Note overwrite=T
  raster::shapefile(dissolved_1km_buffer, file = paste0(PATH_SHP_dis1km, species, "_1km.shp"), overwrite=TRUE)
  #raster::shapefile(sp_buffer_1km, file = paste0(PATH_SHP_1km, species, "_1km.shp"), overwrite=TRUE) #Should be obsolete! 6/29 SCS
}

# Calculate AOO rank for each range metric and focal species.
AOOs$rank1km <- rank(AOOs$dissolved_buffer_1km)
AOOs$rank_huc12 <- rank(AOOs$huc12_area)

for(i in 1:length(FILES)){
  # Calculate and store mean of the 2 AOO ranks in the mean column.
  AOOs[i,]$mean_Rank <- mean(c(AOOs[i,]$rank1km, AOOs[i,]$rank_huc12), na.rm=TRUE)

  # Calculate and store the range of the 2 AOO ranks in the range column.
  AOOs[i,]$range <- abs((AOOs[i,12] - AOOs[i,13])) #$rank1km, AOOs[i,]$rank_huc12), na.rm=TRUE)

  # Calculate and store standard deviation of the 2 AOO ranks in the sd column.
  AOOs[i,]$sd_Rank <- sd(c(AOOs[i,]$rank1km, AOOs[i,]$rank_huc12), na.rm=TRUE)

  # Scale mean rank between 0 and 1
  AOOs$StandardizedMean <- rescale(AOOs$mean_Rank, to =c(0,1))

  # Calculate and store coefficient of variation (cv) for rankings.
  AOOs$cv <- (AOOs$sd_Rank/AOOs$mean_Rank)*100

  # Calculate and store logarithms (base 10) of AOOs for both range metrics.
  AOOs$logBuffer1km <- log10(AOOs$dissolved_buffer_1km)
  AOOs$logHUC12 <- log10(AOOs$huc12_area)
}

# Write CSV with date in "_01Jan2020" format.
write.csv(AOOs, file = "AOO Output_29June2020.csv")
