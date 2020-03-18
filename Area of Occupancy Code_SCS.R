# Area of Occupancy Code for the National Fishes Vulnerability Assessment - "Area of Occupancy Code_SCS.R"
# Revised by Sam Silknetter, 18March2020

# install necessary libraries
library(raster)
library(rgbif)
library(rgdal)
library(sp)
library(rgeos)

# Set data paths.
PATH_HUC12 <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/HUC12"
PATH_HUC8 <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/HUC8"
Species_Data <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Species_Data"

# Set spatial CRS.
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# Read in fixed data.
huc12 <- readOGR(dsn = PATH_HUC12, layer = "HUC12")
proj4string(huc12) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
huc12_wgs84 <- spTransform(huc12, crs.geo)

# Create end data table.
AOOs <- data.frame(scientific_name = character(), buffer_1km = numeric(), huc12_area = numeric(), stringsAsFactors = F)

# Create a list of all species data. 
FILES <- list.files(path=Species_Data)

# For loop to cycle through Species_Data candidate list (N = 159).
#for(i in 1:10){
for(i in 1:159){
  geodata <- read.csv(paste(Species_Data,"/", FILES[i], sep = ""), header=TRUE) 

  # Convert from 'geodata' CSV to an OGR object. 
  dat_csv <- as.data.frame(geodata)
  dat_coord <- dat_csv[c("decimalLongitude", "decimalLatitude")] # Create data frame of longitude and latitude for all entries.
  dat_sp <- SpatialPointsDataFrame(dat_coord, dat_csv, proj4string = crs.geo) # Save spatial dataframe with correct projection.
  
  AOOs[i,1] <- as.character(dat_sp$species[1]) # Set column 1 in AOOs to species name. 
  
  # Calculate HUC12 area
  dat_sp$HUC12 <- over(dat_sp, huc12_wgs84)$HUC12 # Store the HUC12 name as an attribute of the fish data.
  Number_of_HUC12s <- data.frame(unique(dat_sp$HUC12))
  huc12_sp <- huc12[huc12$HUC12 %in% Number_of_HUC12s$unique.dat_sp.HUC12.,]  # Extract unique HUC12 data for the species.
  total_area_huc12 <- sum(huc12_sp$AreaSqKm) # Sum the total area (square kilometer) for each unique HUC12.
  AOOs[i,]$huc12_area <- total_area_huc12
  
  # Generate 1km point buffers. You must use a projected CRS for function gBuffer(). For CRS, we use: USA Contiguous albers equal area 
  geodata_Albers <- spTransform(dat_sp, crs.albers)
  
  # Create 1 km buffer and add the total area to the AOOs dataframe.
  sp_buffer_1km <- gBuffer(geodata_Albers, width = 1, byid= TRUE) 
  AOOs[i,]$buffer_1km <- gArea(sp_buffer_1km, byid = FALSE)
  
  # Save the spatial dataframes as ESRI Shapefiles. 
  #species <- geodata$species[1] # Identify the species name for CSV output. 
  #setwd("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Shapefiles/HUC-12") # Note: shapefile() doesn't allow dsn, so use setwd() instead. 
  #raster::shapefile(huc12_sp, filename = paste0(species, "_HUC12.shp"), overwrite=TRUE)
  #setwd("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Shapefiles/1km Buffer")
  #raster::shapefile(sp_buffer_1km, filename = paste0(species, "_1km.shp"), overwrite=TRUE)
  #setwd("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS") #Reset working directory before returning to the begging of the loop. 
}

AOOs$rank1km <- rank(AOOs$buffer_1km)
AOOs$rank_huc12 <- rank(AOOs$huc12_area)

#Write CSV with date in "_01Jan2020" format
write.csv(AOOs, file = "AOO Output_2_16Mar2020.csv")

#### Finish Here




geodata <- read.csv("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Species_Data/Acantharchus pomotis.csv", header=TRUE)

AOOs[i,1] <- as.character(geodata$species[1])

#-- Calculate HUC12 area
geodata$HUC12 <- over(geodata, huc12_wgs84)$HUC12 #store the HUC12 name as an attribute of the fish data
Number_of_HUC12s <- data.frame(unique(geodata$HUC12))
huc12_sp <- huc12[huc12$HUC12 %in% Number_of_HUC12s$unique.geodata.HUC12.,]  #extract unique HUC12 data for the species
total_area_huc12 <- sum(huc12_sp$AreaSqKm) #sum the total area (square kilometer) for each unique HUC12's
AOOs[i,]$huc12_area <- total_area_huc12

#-- Generate 1km point buffers
# Have to project data into a projected CRS because gBuffer will not work on geographic data.
# Mims team chose USA Contiguous albers equal area
geodata_Albers <- spTransform(geodata, crs.albers)

#-- create 1 km buffer and add the total area to the AOO dataframe
sp_buffer_1km <- gBuffer(geodata_Albers, width = 1, byid= TRUE) #create a buffer of 1 km
AOOs[i,]$buffer_1km <- gArea(sp_buffer_1km, byid = FALSE)



for (i in 1:nrow(SpeciesList)){
  # Using the scientificName as the search parameter means we'll get back any records for synonyms (ex: searching "Ameiurus
  # serracanthus" brings back records for that as well as "Ictalurus serracanthus")
  GBIFSpecies <- occ_search(scientificName = SpeciesList[i,]$SciName, hasCoordinate = T, geometry = "POLYGON ((-125.15625 48.48749,
                            -125.20020 42.71473, -124.89258 40.27953, -123.26660 37.30028, -120.62988 33.72434, -117.42188 32.54681, -114.87305 32.50976,
                            -111.04980 31.31610, -108.28125 31.35364, -108.23730 31.76554, -106.56738 31.84023, -105.38086 30.78904, -104.72168 29.99300,
                            -103.13965 28.76766, -102.87598 29.30556, -102.08496 29.80252, -101.33789 29.64987, -100.19531 28.11075, -99.31641 26.54922,
                            -96.98730 25.79989, -97.03125 26.90248, -95.00977 28.80617, -92.54883 29.03696, -89.16504 28.22697, -88.06641 29.38218,
                            -85.38574 29.19053, -83.80371 27.91677, -83.62793 24.64702, -81.78223 23.76524, -79.93652 24.04646, -79.05762 28.07198,
                            -80.24414 30.41078, -79.93652 31.91487, -76.55273 33.90690, -74.66309 35.03000, -75.19043 37.02010, -72.99316 40.11169,
                            -70.83984 40.64730, -69.34570 40.97990, -69.56543 42.84375, -68.86230 43.26121, -67.28027 43.80282, -66.66504 44.52784,
                            -67.45605 45.58329, -67.80762 46.98025, -68.51074 47.51720, -69.12598 47.45781, -70.22461 46.37725, -71.27930 45.21300,
                            -72.46582 45.33670, -74.31152 45.27489, -76.68457 43.96119, -77.56348 43.64403, -79.01367 43.51669, -78.88184 42.74701,
                            -80.72754 42.48830, -82.57324 41.86956, -83.01270 42.71473, -82.13379 43.48481, -82.57324 45.30580, -83.54004 45.82880,
                            -84.85840 45.89001, -84.72656 46.83013, -85.86914 47.33882, -88.15430 48.13677, -89.64844 48.01932, -90.92285 48.25394,
                            -92.76855 48.48749, -94.79004 48.86471, -95.05371 49.49667, -95.36133 49.00905, -106.87500 49.06667, -115.22461 49.03787,
                            -122.69531 48.95137, -123.31055 48.22467, -125.15625 48.48749))", limit = 100000, fields = c("species","decimalLatitude",
                                                                                                                         "decimalLongitude","issues","eventDate",
                                                                                                                         "geodeticDatum","datasetName",
                                                                                                                         "occurrenceID", "occurrenceStatus",
                                                                                                                         "country", "countryCode","scientificName",
                                                                                                                         "locality","dataset","catalogNumber",
                                                                                                                         "institutionCode"))
  

  #### Final Points Code Here (see comment above) ####
  
  #7-- Calculate HUC12 area
  geodata$HUC12 <- over(geodata, huc12_wgs84)$HUC12 #store the HUC12 name as an attribute of the fish data
  Number_of_HUC12s <- data.frame(unique(geodata$HUC12))
  huc12_sp <- huc12[huc12$HUC12 %in% Number_of_HUC12s$unique.geodata.HUC12.,]  #extract unique HUC12 data for the species
  total_area_huc12 <- sum(huc12_sp$AreaSqKm) #sum the total area (square kilometer) for each unique HUC12's
  
  #8-- Generate buffers for 1km, 5km, 10km, 20km
  # Have to project data into a projected CRS because gBuffer will not work on geographic data.
  # Mims team chose USA Contiguous albers equal area
  geodata_Albers <- spTransform(geodata, crs.albers)
  AOOs[i,]$scientific_name <- dat[1,]$species 
  
  ### create 1 km buffer and add the total area to the AOO dataframe
  sp_buffer_1km <- gBuffer(geodata_Albers, width = 1, byid= TRUE) #create a buffer of 1 km
  AOOs[i,]$buffer_1km <- gArea(sp_buffer_1km, byid = FALSE)  
  
  ### create 5 km buffer
  #sp_buffer_5km <- gBuffer(finalpoints_Albers, width = 5, byid = F)
  #AOOs[i,]$buffer_5km <- gArea(sp_buffer_5km, byid = F)
  
  ### create 10 km buffer
  #sp_buffer_10km <- gBuffer(finalpoints_Albers, width = 10, byid = F)
  #AOOs[i,]$buffer_10km <- gArea(sp_buffer_10km, byid = F)
  
  ### create 20 km buffer
  #sp_buffer_20km <- gBuffer(finalpoints_Albers, width = 20, byid = F)
  #AOOs[i,]$buffer_20km <- gArea(sp_buffer_20km, byid = F)
  
  #9-- Generate minimum convex polygon
  ### need only the lat/lon, can't have other information for the mcp functions in adeHabitatHR and can't seem to
  ### subset the spatial data same as a normal data frame so need to convert to a normal data frame, subset out
  ### lat/lon then turn it back into a spatial data frame
  # fp_sp_xy <- as.data.frame(finalpoints_sp)
  # fp_sp_xy <- fp_sp_xy[c("decimalLatitude", "decimalLongitude")]
  # fp_sp_xy <- SpatialPoints(fp_sp_xy)
  # proj4string(fp_sp_xy) <- crs.geo
  # fp_sp_xy_albers <- spTransform(fp_sp_xy, crs.albers)
  # mcp <- mcp(fp_sp_xy_albers, percent = 100)
  # mcp_area <- mcp.area(fp_sp_xy_albers ,unout  = "km2", percent = 100, plotit = F) # this doesn't seem to be working
  
  #10-- Build AOO / MCP data table
  # AOOs[4,]$mcp_area <- mcp_area
  #AOOs[i,]$huc8_area <- total_area_huc8
  AOOs[i,]$huc12_area <- total_area_huc12
}

AOOs$rank1km <- rank(AOOs$buffer_1km)
AOOs$rank_huc12 <- rank(AOOs$huc12_area)

#Write CSV with date in "_01Jan2020" format
write.csv(AOOs, file = "AOO Output_02Mar2020.csv")

