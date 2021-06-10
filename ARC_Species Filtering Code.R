# Species Filtering Code for the National Fishes Vulnerability Assessment - "Species Filtering Code.R"
# Revised by Sam Silknetter, 07June2021

# Install and load necessary libraries.
library(readr)
library(rgbif)
library(rgdal)
library(sp)
library(rgeos)


# Set data paths for spatial data. All available through https://github.com/silknets/National-RCS.
PATH_Estuary <- "/home/silknets/GitHub/National-RCS/merged estuary"
PATH_StudyExtent <- "/home/silknets/GitHub/National-RCS/Region Shapefiles/"
# NatureServe shapefiles had to be saved using subdirectories in GitHub. Download locally into a single folder for path below. 
PATH_NatureServeFishRanges <- "/home/silknets/GitHub/National-RCS/NatureServe Shapefiles - Candidate Species" 
PATH_NationalOccurrenceData <- "/home/silknets/GitHub/National-RCS/Focal Species/" 

# Set a local path to save CSV outputs in this script. 
PATH_East <- "/home/silknets/GitHub/National-RCS/Regional_Occurrence_Data/East/"
PATH_Miss <- "/home/silknets/GitHub/National-RCS/Regional_Occurrence_Data/Miss/"
PATH_Gulf <- "/home/silknets/GitHub/National-RCS/Regional_Occurrence_Data/Gulf/"
PATH_West <- "/home/silknets/GitHub/National-RCS/Regional_Occurrence_Data/West/"

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
SpeciesList <- read_csv("/home/silknets/GitHub/National-RCS/Focal Species List.csv") 

# Create an empty data frame to be populated with occurrence points after each numbered step (N=10).
ColumnCount <- 10 # The number of columns for the Filter dataframe. Must match the number of 'dimnames' strings entered below. 
Filters <- data.frame(matrix(vector(), 0, ColumnCount, dimnames=list(c(), c("Species", "All_Data", "GBIF_Issues", 
                                                                            "Study_Extent", "Estuary", "NatureServe", "East", "Mississippi", "Gulf", "West"))), stringsAsFactors=F)

# Pull global occurrence data for all species in candidate list. 
RecordLimit = 100000 # This is the maximum number of occurrence points/species allowed by the occsearch() function.

i=1
for (i in 1:nrow(SpeciesList)){
  
  # Using the scientificName as the search parameter means we'll get back any records for synonyms (ex: searching "Ameiurus
  # serracanthus" brings back records for that as well as "Ictalurus serracanthus").
  GBIFSpecies <- occ_search(scientificName = SpeciesList[i,]$scientific_name, hasCoordinate = T, limit = RecordLimit, fields = c("species", "decimalLatitude", "decimalLongitude", "issues", "eventDate", "year", "geodeticDatum", "datasetName", "occurrenceID", "occurrenceStatus", "country", "countryCode", "locality", "dataset", "catalogNumber", "institutionCode"))
  GBIFSpeciesData <- as.data.frame(GBIFSpecies$data) #Save GBIF search records as a dataframe.
  
  ###  Code below re-runs search for species with number of occurrences > RecordLimit by limiting search to the U.S. only. 
  ###  If true, Filters table will not reflect global occurrence data for species [i]. 
  #GBIFSpecies <- occ_search(scientificName = SpeciesList[i,]$SciName, hasCoordinate = T, limit = RecordLimit, country = 'US', fields = c("species", "decimalLatitude", "decimalLongitude", "issues", "eventDate", "year", "geodeticDatum", "datasetName", "occurrenceID", "occurrenceStatus", "country", "countryCode", "locality", "dataset", "catalogNumber", "institutionCode"))
  #GBIFSpeciesData <- as.data.frame(GBIFSpecies$data)
  
  # Column 1 - Species: Add species name (Species) to 'Filters' dataframe.
  Filters[i,1]  <- as.character(GBIFSpeciesData$species[1]) 
  
  # Column 2 - All_Data: Add the number of global occurrence points in GBIF to 'Filters' dataframe.
  Filters[i,2] <- as.numeric(nrow(GBIFSpeciesData)) 
  
  # Column 3 - GBIF_Issues: Filter records that have the gbif issues COUNTRY_COORDINATE_MISMATCH, RECORDED_DATE_MISMATCH, ZERO_COORDINATE, and
  # IDENTIFIED_DATE_UNLIKELY. 
  GBIFSpecies <- occ_issues(GBIFSpecies, -cucdmis, -rdatm, -zerocd, -iddatunl)
  dat <- as.data.frame(GBIFSpecies$data) #Save issue-free GBIF search records as a dataframe.
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
  
  # Save filtered occurrence points at the national scale to the National_Occurrence_Data path for downstream analyses.
  species <- range_clipped$species[1] #Identify the species name for CSV output. 
  if (Filters[i,6] >= 50) {
    write.csv(range_clipped, file = paste0(PATH_NationalOccurrenceData, species, ".csv"), row.names=FALSE)} 
  
  # Column 7 - East: Clip occurence points to include only those present in the East Region.
  occ_east <- range_clipped[reproj_region_east,]
  if ( Filters[i,6] >= 50 && nrow(occ_east) >= 10 ) {
    write.csv(occ_east, file = paste0(PATH_East, species, ".csv"), row.names=FALSE)}
  Filters[i,7] <- nrow(occ_east)
  
  # Column 8 - Miss: Clip occurence points to include only those present in the Mississippi Region.
  occ_miss <- range_clipped[reproj_region_miss,]
  if ( Filters[i,6] >= 50 && nrow(occ_miss) >= 10 ) {
    write.csv(occ_miss, file = paste0(PATH_Miss, species, ".csv"), row.names=FALSE)}
  Filters[i,8] <- nrow(occ_miss)
  
  # Column 9 - Gulf: Clip occurence points to include only those present in the Gulf Region.
  occ_gulf <- range_clipped[reproj_region_gulf,]
  if ( Filters[i,6] >= 50 && nrow(occ_gulf) >= 10 ) {
    write.csv(occ_gulf, file = paste0(PATH_Gulf, species, ".csv"), row.names=FALSE)}
  Filters[i,9] <- nrow(occ_gulf)
  
  # Column 10 - West: Clip occurence points to include only those present in the West Region.
  occ_west <- range_clipped[reproj_region_west,]
  if ( Filters[i,6] >= 50 && nrow(occ_west) >= 10 ) {
    write.csv(occ_west, file = paste0(PATH_West, species, ".csv"), row.names=FALSE)}
  Filters[i,10] <- nrow(occ_west)
  
  # Additional 'exception' step to increase representation in the West region. 
  if (nrow(occ_west) >= 25) {
    write.csv(range_clipped, file = paste0(PATH_NationalOccurrenceData, species, ".csv"), row.names=FALSE)
    write.csv(occ_west, file = paste0(PATH_West, species, ".csv"), row.names=FALSE)} 
} 

write.csv(Filters, file = "All_Filters_07June2021.csv") #Write a CSV output file.
