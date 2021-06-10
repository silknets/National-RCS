# National Fishes Vulnerability Assessment Project - "ARC_Area of Occupancy.R"
# Revised by Sam Silknetter, 07June2021

# This code calculates Area of Occupancy (AOO) for each species and range metric. 

# Install necessary libraries.
library(raster)
library(rgbif)
library(rgdal)
library(sp)
library(rgeos)
library(scales)

# Set data paths.
PATH_HUC12 <-  "/work/cascades/silknets/singularity/HUC12_All.rds"
PATH_FocalSpecies_OccurrenceData <- "/home/silknets/GitHub/National-RCS/Focal Species/"
PATH_SHP_HUC12 <- "/home/silknets/GitHub/National-RCS/Shapefiles/HUC12/National/"
PATH_Occupied_HUC12 <- "/home/silknets/GitHub/National-RCS/Occupied_HUCs/"

# Set spatial coordinate reference system (CRS).
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# Read in fixed data.
huc12 <- readRDS(PATH_HUC12)
proj4string(huc12) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
huc12_wgs84 <- spTransform(huc12, crs.geo)

# Create empty AOO data table to be populated.
AOOs <- data.frame(scientific_name = character(), huc12_area = numeric(), stringsAsFactors = F)

# Create a list of occurrence data files for all focal species. 
FILES <- list.files(path=PATH_FocalSpecies_OccurrenceData)

# Loop to cycle through all focal species (N = 139).
i=1
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
  #dat_sp$HUC12 <- over(dat_sp, huc12_wgs84) # Store the HUC12 name as an attribute of the fish data.
  HUC_ID <- over(dat_sp, huc12_wgs84) # Get watershed data for all occurrence points
  dat_sp$huc.name <- HUC_ID$huc12 # Append HUC-12 name
  dat_sp$huc.areasqkm <- HUC_ID$areasqkm # Append HUC-12 area in km^2
  Number_of_HUC12s <- data.frame(unique(dat_sp$huc.name)) #Identify unique watersheds occupied by species [i]
  names(Number_of_HUC12s) <- "Occupied_HUC"
  huc12_sp <- huc12[huc12$huc12 %in% Number_of_HUC12s$Occupied_HUC,]  # Extract unique HUC12 data for the species.
  total_area_huc12 <- sum(huc12_sp$areasqkm) # Sum the total area (square kilometer) for each unique HUC12.
  AOOs[i,]$huc12_area <- total_area_huc12
  
  # Identify unique watersheds occupied per year
  unique_hucs <- as.data.frame(dat_sp)
  unique_hucs <- unique(unique_hucs[c("huc.name", "year")]) #Identify unique watersheds occupied by species [i]
  # Write csv as '.txt' to preserve leading zeroes. Use read.table(file, sep = ",") to read it in later. 
  write.csv(unique_hucs, file = paste0(PATH_Occupied_HUC12, species, ".txt"), row.names = FALSE) 

  # Save the spatial dataframes as ESRI Shapefiles. 
  raster::shapefile(huc12_sp, file = paste0(PATH_SHP_HUC12, species, "_HUC12.shp"), overwrite=TRUE) #Note overwrite=T
}

# Write CSV to current directry with date in "_01Jan2020" format.
write.csv(AOOs, file = "AOO Output_07June2021.csv")