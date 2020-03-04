# ALB / SCS revised version of the National Fishes Vulnerability Assessment "GBIF data filtering and AOO estimation_11_13_2018.R"
# Sam Silknetter 03March2020

# Install and load necessary libraries. 
###### SCS to remove unnecessary libraries
library(readr)
library(tidyverse)
library(rgbif)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(adehabitatHR)
library(ggpubr)
library(ggplot2)
library(mapdata)
library(maps)
library(maptools)
library(RColorBrewer)
library(mapproj)
library(ggmap)
library(geosphere)

# Set data paths for spatial data. All available through https://github.com/silknets/National-RCS.
PATH_NatureServeFishRanges <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/NatureServe Shapefiles - All species" 
PATH_Estuary <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/merged estuary"
PATH_StudyExtent <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Region Shapefiles"

# Set a local path to save CSV outputs in this script. 
PATH_SpeciesData <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Species_Data/"

# Set spatial coordinate reference system (CRS).
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# Read in fixed spatial data. See Table S[X] in Silknetter et al. for additional details on spatial data.  
estuary <- readOGR(dsn = PATH_Estuary, layer="Merged_estuaries")
proj4string(estuary) <- crs.geo # Warning expected using this function - spTransform() used in subsequent line to address.
reproj_estuary <- spTransform(estuary, crs.geo)

study_extent <- readOGR(dsn = PATH_StudyExtent, layer = "Study_Extent") # Limit occurences to those within the Study Extent shapefile.
reproj_study_extent <- spTransform(study_extent, crs.geo)

region_east <- readOGR(dsn = PATH_StudyExtent, layer = "East") # Limit occurences to those within the East Region shapefile.
reproj_region_east <- spTransform(region_east, crs.geo)

region_miss <- readOGR(dsn = PATH_StudyExtent, layer = "Mississippi") # Limit occurences to those within the Mississippi Region shapefile.
reproj_region_miss <- spTransform(region_miss, crs.geo)

region_gulf <- readOGR(dsn = PATH_StudyExtent, layer = "Gulf") # Limit occurences to those within the Gulf Region shapefile.
reproj_region_gulf <- spTransform(region_gulf, crs.geo)

region_west <- readOGR(dsn = PATH_StudyExtent, layer = "West") # Limit occurences to those within the West Region shapefile.
reproj_region_west <- spTransform(region_west, crs.geo)

# Import candidate species list (N=159 species). File available through https://github.com/silknets/National-RCS.
SpeciesList <- read_csv("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/CandidateSpeciesList_2020.03.02.csv") 

# Create an empty data frame to be populated with occurrence points after each numbered step (N=10).
ColumnCount <- 10 # The number of columns for the Filter dataframe. Must match the number of 'dimnames' strings entered below. 
Filters <- data.frame(matrix(vector(), 0, ColumnCount, dimnames=list(c(), c("Species", "All_Data", "GBIF_Issues", 
    "Study_Extent", "Estuary", "NatureServe", "East", "Mississippi", "Gulf", "West"))), stringsAsFactors=F)

# Pull global occurrence data for all species in candidate list. 
RecordLimit = 199000 # This is the maximum number of occurrence points/species allowed by the occsearch() function.

for (i in 1:nrow(SpeciesList)){
  
# Using the scientificName as the search parameter means we'll get back any records for synonyms (ex: searching "Ameiurus
# serracanthus" brings back records for that as well as "Ictalurus serracanthus").
GBIFSpecies <- occ_search(scientificName = SpeciesList[i,]$SciName, hasCoordinate = T, limit = RecordLimit, fields = c("species", "decimalLatitude",
    "decimalLongitude", "issues", "eventDate", "geodeticDatum","datasetName", "occurrenceID", "occurrenceStatus", "country", "countryCode", 
    "scientificName", "locality", "dataset", "catalogNumber", "institutionCode"))
GBIFSpeciesData <- as.data.frame(GBIFSpecies$data) #Save GBIF search records as a dataframe.

# Column 1 - Species: Add species name (Species) to 'Filters' dataframe.
Filters[i,1]  <- as.character(GBIFSpeciesData$species[1]) 

# Column 2 - All_Data: Add the number of global occurrence points in GBIF to 'Filters' dataframe.
Filters[i,2] <- as.numeric(nrow(GBIFSpeciesData)) 

# Column 3 - GBIF_Issues: Filter records that have the gbif issues COUNTRY_COORDINATE_MISMATCH, RECORDED_DATE_MISMATCH, ZERO_COORDINATE, and
# IDENTIFIED_DATE_UNLIKELY. 
dat <- GBIFSpeciesData %>%
  occ_issues(-cucdmis, -rdatm, -zerocd, -iddatunl) # By using the minus sign [-], entries with these issues are removed from global data.
dat_coord <- dat[c("decimalLongitude", "decimalLatitude")] # Create data frame of longitude and latitude for all valid entries.
Filters[i,3] <- nrow(dat_coord)

# Column 4 - Study_Extent: Filter occurrences that are outside the study extent (conterminous United States).
extent <- SpatialPointsDataFrame(dat_coord, dat, proj4string = crs.geo) # Save spatial dataframe with correct projection.
extent_clipped <- extent[reproj_study_extent,]
Filters[i,4] <- nrow(extent_clipped)

# Column 5 - Estuary: Filter out estuarine occurrence points. 
estuary_subset <- extent_clipped[reproj_estuary,] #Subsets occurrence points to those of estuary.
estuary_pts <- as.data.frame(estuary_subset) #Changes spatial points to a dataframe.
estuary_final <- extent_clipped[(!extent_clipped$occurrenceID %in% estuary_pts$occurrenceID),] #Removes occurrence records in estuaries.
Filters[i,5] <- nrow(estuary_final)

# Column 6 - NatureServe: Filter out occurrences that are outside the native range for the species as determined by NatureServe.
range <- readOGR(dsn = PATH_NatureServeFishRanges, layer = paste(SpeciesList[i,]$Genus, SpeciesList[i,]$Species, sep = "_"))
proj4string(range) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
reproj_range <- spTransform(range, crs.geo)
range_clipped <- estuary_final[reproj_range,]  
Filters[i,6] <- nrow(range_clipped) #This is the final step to filter occurence point data. Columns 7-10 just clip this data by region.

# Save filtered occurrence points to the SpeciesData path for downstream analyses.
species <- range_clipped$species[1] #Identify the species name for CSV output. 
write.csv(range_clipped, file = paste0(PATH_SpeciesData, species, ".csv"), row.names=FALSE)

# Column 7 - East: Clip occurence points to include only those present in the East Region.
occ_east <- range_clipped[reproj_region_east,]
Filters[i,7] <- nrow(occ_east)

# Column 8 - Miss: Clip occurence points to include only those present in the Mississippi Region.
occ_miss <- range_clipped[reproj_region_miss,]
Filters[i,8] <- nrow(occ_miss)

# Column 9 - Gulf: Clip occurence points to include only those present in the Gulf Region.
occ_gulf <- range_clipped[reproj_region_gulf,]
Filters[i,9] <- nrow(occ_gulf)

# Column 10 - West: Clip occurence points to include only those present in the West Region.
occ_west <- range_clipped[reproj_region_west,]
Filters[i,10] <- nrow(occ_west)
}

write.csv(Filters, file = "All_Filters_02March2020.csv") #Write a CSV output file.