# ALB / SCS revised version of the National Fishes Vulnerability Assessment "GBIF data filtering and AOO estimation_11_13_2018.R"
# Sam Silknetter 4Feb2020

# install necessary libraries
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
library(rgeos)
library(geosphere)

# Set data paths
PATH_NatureServeFishRanges <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/NatureServe Shapefiles - All species"
PATH_ESTUARY <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/merged estuary"
PATH_StudyExtent <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Region Shapefiles"

# Set spatial CRS
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# Read in fixed data
estuary <- readOGR(dsn = PATH_ESTUARY, layer="Merged_estuaries")
proj4string(estuary) <- crs.geo # Warning expected - spTransform used next to address
reproj_estuary <- spTransform(estuary, crs.geo)

study_extent <- readOGR(dsn = PATH_StudyExtent, layer = "Study Extent") # Limit occurences to those within the Study Extent shapefile
reproj_study_extent <- spTransform(study_extent, crs.geo)

region_east <- readOGR(dsn = PATH_StudyExtent, layer = "East") # Limit occurences to those within the East Region shapefile
reproj_region_east <- spTransform(region_east, crs.geo)

region_miss <- readOGR(dsn = PATH_StudyExtent, layer = "Mississippi") # Limit occurences to those within the Mississippi Region shapefile
reproj_region_miss <- spTransform(region_miss, crs.geo)

region_gulf <- readOGR(dsn = PATH_StudyExtent, layer = "Gulf") # Limit occurences to those within the Gulf Region shapefile
reproj_region_gulf <- spTransform(region_gulf, crs.geo)

region_west <- readOGR(dsn = PATH_StudyExtent, layer = "West") # Limit occurences to those within the West Region shapefile
reproj_region_west <- spTransform(region_west, crs.geo)

# Import candidate species list, which can be found in the 
# NFVAP - National RCS Google Drive Space in the Species Selection Folder: "MergedSpeciesList_2020.01.16alb.csv".
SpeciesList <- read_csv("G:/Shared drives/NFVAP - National RCS/Species Selection/MergedSpeciesList_2020.01.27.csv")

# Create an empty data frame to be populated with occurrence point data by filtering step
Filters <- data.frame(matrix(vector(), 0, 10, dimnames=list(c(), c("Species", "All_Data", "GBIF_Issues", 
    "Study_Extent", "Estuary", "NatureServe", "East", "Mississippi", "Gulf", "West"))), stringsAsFactors=F)

# Pull global occurrence data for all species in candidate list. Limit=199k, the max for this function
for (i in 1:nrow(SpeciesList)){
  
# Using the scientificName as the search parameter means we'll get back any records for synonyms (ex: searching "Ameiurus
# serracanthus" brings back records for that as well as "Ictalurus serracanthus")
GBIFSpecies <- occ_search(scientificName = SpeciesList[i,]$SciName, hasCoordinate = T, limit = 199000, fields = c("species", "decimalLatitude",
    "decimalLongitude", "issues", "eventDate", "geodeticDatum","datasetName", "occurrenceID", "occurrenceStatus", "country", "countryCode", 
    "scientificName", "locality", "dataset", "catalogNumber", "institutionCode"))
GBIFSpeciesData <- as.data.frame(GBIFSpecies$data) #Save search records as a dataframe

#1--  Add species name (Species) and the number of global occurrences in GBIF (All_Data) to 'Filters' dataframe
Filters[i,1]  <- as.character(GBIFSpeciesData$species[1]) # Save species name in column 1 of row [i] 
Filters[i,2] <- as.numeric(nrow(GBIFSpeciesData)) # Save the number of global occurrence points in column 2 of row [i]

#2-- Filter records that have the gbif issues COUNTRY_COORDINATE_MISMATCH, RECORDED_DATE_MISMATCH, ZERO_COORDINATE, and
# IDENTIFIED_DATE_UNLIKELY. 
dat <- GBIFSpeciesData %>%
  occ_issues(-cucdmis, -rdatm, -zerocd, -iddatunl) # By using the minus sign [-], entries with these issues are removed from global data
dat_coord <- dat[c("decimalLongitude", "decimalLatitude")] # Create data frame of longitude and latitude for all valid entries 
datfsp <- SpatialPointsDataFrame(dat_coord, dat, proj4string = crs.geo) # Save spatial dataframe with correct projection
# Save # of occurrence points after filtering for GBIF Issues
Filters[i,3] <- nrow(datfsp)

#3-- Filter occurrences that are outside the study extent (within BOTH conterminous US and 4x delineated regions)
clipped_occurrences_by_extent <- datfsp[reproj_study_extent,]
# Save # of occurrence points after filtering for Study Extent
Filters[i,4] <- nrow(clipped_occurrences_by_extent)

#4-- Filter out estuarine occurrence points. 
# Estuary shapefile was downloaded from the EPA website https://www.epa.gov/hesc/estuary-data-mapper-edm 
# and the Atlantic, Pacific, and Gulf of Mexico estuaries were merged into a single layer.
sub_est <- clipped_occurrences_by_extent[reproj_estuary,] #subsets occurrence points to those of estuary
est_pts <- as.data.frame(sub_est) #changes spatial points to a dataframe for mapping with ggplot2
finalpoints_sp <- clipped_occurrences_by_extent[(!clipped_occurrences_by_extent$occurrenceID %in% est_pts$occurrenceID),] #takes out occurrence records in estuaries
# Saving # occurrence point data after filtering points in EPA-mapped estuaries
Filters[i,5] <- nrow(finalpoints_sp)

#5-- Filter out occurrences that are outside the native range for the species as determined by NatureServe. Data were downloaded
# from here http://www.natureserve.org/conservation-tools/data-maps-tools/digital-distribution-native-us-fishes-watershed
dat_sp_range <- readOGR(dsn = PATH_NatureServeFishRanges, layer = paste(SpeciesList[i,]$Genus, SpeciesList[i,]$Species, sep = "_"))
proj4string(dat_sp_range) <- CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83")
reproj_dat_sp_range <- spTransform(dat_sp_range, crs.geo)
clipped_occurrences_by_range <- finalpoints_sp[reproj_dat_sp_range,]
# Saving occurrence point data after filtering records outside of a species' native range
Filters[i,6] <- nrow(clipped_occurrences_by_range)

#6-- Clip occurence points to include only those present in the East Region
occ_east <- datfsp[reproj_region_east,]
# Save # of occurrence points after filtering for Study Extent
Filters[i,7] <- nrow(occ_east)

#7-- Clip occurence points to include only those present in the Mississippi Region
occ_miss <- datfsp[reproj_region_miss,]
# Save # of occurrence points after filtering for Study Extent
Filters[i,8] <- nrow(occ_miss)

#8-- Clip occurence points to include only those present in the East Region
occ_gulf <- datfsp[reproj_region_gulf,]
# Save # of occurrence points after filtering for Study Extent
Filters[i,9] <- nrow(occ_gulf)

#9-- Clip occurence points to include only those present in the East Region
occ_west <- datfsp[reproj_region_west,]
# Save # of occurrence points after filtering for Study Extent
Filters[i,10] <- nrow(occ_west)
}
write.csv(Filters, file = "Filters.csv") #output the file as csv
