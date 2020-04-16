# National Fishes Vulnerability Assessment Project - "PRISM_ppt_SCS.R"
# Revised by Sam Silknetter, 13April2020

# This code extracts annual precipitation climate data from PRISM for 1km buffered occurrence points.

library(prism)
library(raster) 
library(sp) 
library(pbdMPI)
library(rgdal)
library(ff)
library(rlist)

# Set data paths for input data. # All available through https://github.com/silknets/National-RCS.
PATH_SHP_HUC12_All <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/HUC12/"
PATH_SHP_HUC12_AOO <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Shapefiles/HUC12/"
PATH_SHP_1km <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Shapefiles/Dissolved 1km Buffer/"
PATH_PRISM_ppt <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt"
options(prism.path = PATH_PRISM_ppt) # Set path to variable-specific prism data.

# Create a list of input shapefiles for loop.
FILES_buffer <- list.files(path=PATH_SHP_1km, pattern = "\\.shp$")
FILES_watershed <- list.files(path=PATH_SHP_HUC12_AOO, pattern = "\\.shp$")

# Load 'raster stack' of all PRISM ppt data.  
RS <- prism_stack(ls_prism_data()) # Raster data pulled from prism.path. 
proj4string(RS)<-CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")
#prism_years <- t(ls_prism_data())
#UseCores <- detectCores() -1 #detects one minus the max number of cores
#cl <- makeCluster(UseCores) #create copies of R to run parallel
#registerDoParallel(cl) #regist the cluster for parallel processing

# Extract precipitation data for each HUC-12 watershed in the study extent
HUC12_All <- readOGR(PATH_SHP_HUC12_All)
HUC12_ppt <- extract(RS,HUC12_All, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE)
saveRDS(HUC12_ppt, file = "G:/Shared drives/NFVAP - National RCS/R Code/HUC12_All.rds")
# Remove?
#write.csv(ext_ID1, file =paste0("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/", dat.name,".csv"), row.names=FALSE) 

# Pull precipitation data for each watershed occupied by each species.
for(i in 1:length(FILES_watershed)){
  dat.name <- substr(FILES_watershed[i], 1, nchar(FILES_watershed[i])-4) # Use shapefile name without file extensions.
  Spp_AOO <- readOGR(PATH_SHP_HUC12_AOO, dat.name) # Read in shapefile of the species occupied watersheds.
  ext_WS <- extract(HUC12_ppt,species, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  saveRDS(ext_ID1, file = paste0("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results/Watershed", dat.name, ".rds"))
  # Remove?
  #write.csv(ext_ID1, file =paste0("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results/Watershed", dat.name,".csv"), row.names=FALSE) 
}

# Extract precipitation data from each species 1km buffer AOO. 
#for(i in 3:4){
for(i in 1:length(FILES_buffer)){
  dat.name <- substr(FILES_buffer[i], 1, nchar(FILES_buffer[i])-4) # Use shapefile name without file extensions.
  species <- readOGR("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/Shapefiles/Dissolved 1km Buffer", dat.name) # Read in dissolved shapefile.
  ext_BUF <- extract(RS,species, fun= mean ,na.rm = T, weights = TRUE, small = TRUE, method = 'simple', df = TRUE) # Removed buffer b/c of input shapefile.
  saveRDS(ext_BUF, file = paste0("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results/Buffer", dat.name, ".rds"))
  # Remove?
  #write.csv(ext_BUF, file =paste0("G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/PRISM/ppt/Results/Buffer", dat.name,".csv"), row.names=FALSE) 
}


#### CAN ABBY EXPLAIN CLEAN FUNCTION?

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