
###description


#Initial step takes data from GBIF and renames it to the correct specie
#Take the renamed GBIF files through a loop to clean missing values in longitude, latitude, and year. Return only US entries
#Filter by 50 or more observations in the data files
#Take the filtered data through a loop to clip by estuary and Nature Serve shape files. These are the Final location data
#"HUC8 AOO" & "HUC12 AOO" take each individual location file and filter it through the HUCs. Outputting only occurrence points that are in the HUC
#"1,5,10,20 KM BUFFER AOO" plot the occurrence points on a map of the US and creat buffer around the points
#"MCP AOO" use the occurrence points to create a minimum convex polygon for each specie
#Individual Code by S Yamada and Y Chen
#Compliled and edited by Y Chen
#23 JULY 2018
rm(list=ls())

#install.packages(package name) ; run this command for packages that are not installed
library(ggpubr)
library(sf)
library(ggplot2)
library(mapdata)
library(sp)
library(maps)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(mapproj)
library(ggmap)
library(rgeos)
library(dplyr)
library(geosphere)
library(adehabitatHR)

#working directory for raw data
WD_GBIF <- "C:/Users/MimsLab/Documents/Ye/GBIF"
#working directory for GBIF renamed
WD_GBIFrenamed <- "C:/Users/MimsLab/Documents/Ye/GBIF renamed/"
#working directory for initial cleaned data from #3 for loop
WD_FIRSTCLEANED <- "C:/Users/MimsLab/Documents/Ye/first cleaned data"
#working directory for 50 or more first cleaned data
WD_50FIRSTCLEANED <- "C:/Users/MimsLab/Documents/Ye/50 or more first cleaned data"
#working directory for final cleaned data
WD_FINALDATA <- "C:/Users/MimsLab/Documents/Ye/Final location data"


#path for GBIF raw data. 
PATH_GBIF <- "C:/Users/MimsLab/Documents/Ye/GBIF"
#path for GBIF renamed files
PATH_GBIFrename <- "C:/Users/MimsLab/Documents/Ye/GBIF renamed"
#path for nature serve shaped files. 
PATH_NATURESERVE <- "C:/Users/MimsLab/Documents/Ye/shape files/"
#path for first cleaned data
PATH_FIRSTCLEANED <- "C:/Users/MimsLab/Documents/Ye/first cleaned data"
#path for estuary shaped files
PATH_ESTUARY <- "C:/Users/MimsLab/Documents/Ye/merged estuary/Merged_estuaries.shp"
#path for final location data
PATH_FINALDATA <- "C:/Users/MimsLab/Documents/Ye/Final location data"
#path for HUC8 data
PATH_HUC8 <- "C:/Users/MimsLab/Documents/Ye/HUC8"
#path for HUC12 data
PATH_HUC12 <- "C:/Users/MimsLab/Documents/Ye/HUC12"

#path to save GBIF RENAMED
SAVED_GBIFrenamed <- "C:/Users/MimsLab/Documents/Ye/GBIF renamed/"
#path to save first cleaned data
SAVED_FIRSTCLEANED <- "C:/Users/MimsLab/Documents/Ye/first cleaned data/"
#path to save 50 or more first cleaned data
SAVED_50FIRSTCLEANED <- "C:/Users/MimsLab/Documents/Ye/50 or more first cleaned data/"
#path to save final location data
SAVED_FINALDATA <- "C:/Users/MimsLab/Documents/Ye/Final location data/"

#path to save shapefiles from HUC8 analysis
SAVESHAPED_HUC8 <- "C:/Users/MimsLab/Documents/Ye/unique HUC8 data/shapefiles HUC8"
#path to save extracted HUC8 data from occurrence points as csv
SAVEDCSV_HUC8 <- "C:/Users/MimsLab/Documents/Ye/unique HUC8 data/"
#path to save total area of HUC8 by specie, includes the name of the file to be saved as
SAVEDAREA_HUC8 <- "C:/Users/MimsLab/Documents/Ye/unique HUC8 data/total area/total_area_huc8.csv"

#path to save shapefiles from HUC12 analysis
SAVEDSHAPED_HUC12 <- "C:/Users/MimsLab/Documents/Ye/unique HUC12 data/shapefiles HUC12"
#path to save extracted HUC12 data from occurrence points as csv
SAVEDCSV_HUC12 <- "C:/Users/MimsLab/Documents/Ye/unique HUC12 data/"
#path tp save total area of HUC12 by specie, includes the name of the file to be saved as
SAVEDAREA_HUC12 <- "C:/Users/MimsLab/Documents/Ye/unique HUC12 data/total area/total_area_huc12.csv"

#path to save 1 km buffer map
SAVEDBUFFER_1KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/1 km buffer/"
#path to save 1 km buffer shape files
SAVEDSHAPED_1KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/1 km buffer/1km buffer shapefiles"
#path to save 5 km buffer map
SAVEDBUFFER_5KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/5 km buffer/"
#path to save 5 km buffer shapefiles
SAVEDSHAPED_5KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/5 km buffer/5km buffer shapefiles"
#path to save 10 km buffer map
SAVEDBUFFER_10KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/10 km buffer/"
#path to save 10 km shape files
SAVEDSHAPED_10KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/10 km buffer/10km buffer shapefiles"
#path to save 20 km buffer map
SAVEDBUFFER_20KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/20 km buffer/"
#path to save 20 km shape files
SAVEDSHAPeD_20KM <- "C:/Users/MimsLab/Documents/Ye/buffer map/20 km buffer/20km buffer shapefiles"

#path to save 1 km buffer area per specie, with name of the csv is saved as
SAVEDAREA_1KM <- "C:/Users/MimsLab/Documents/Ye/total area from buffer/1 km area.csv"
#path to save 5 km buffer area per specie, with name of the csv is saved as
SAVEDAREA_5KM <- "C:/Users/MimsLab/Documents/Ye/total area from buffer/5 km area.csv"
#path to save 10 km buffer area per specie, with name of the csv is saved as
SAVEDAREA_10KM <- "C:/Users/MimsLab/Documents/Ye/total area from buffer/10 km area.csv"
#path to save 20 km buffer area per specie, with name of the csv is saved as
SAVEDAREA_20KM <- "C:/Users/MimsLab/Documents/Ye/total area from buffer/20 km area.csv"

#path to save MCP maps
SAVEDMCP_MAP <- "C:/Users/MimsLab/Documents/Ye/MCP maps/"
#path tp save MCP specie shapefiles
SAVEDSHAPED_MCP <- "C:/Users/MimsLab/Documents/Ye/MCP maps/shapefiles MCP"
#path to save MCP area by specie, with name for csv to be saved as
SAVEDAREA_MCP <- "C:/Users/MimsLab/Documents/Ye/MCP maps/total area MCP/mcp all.csv"

#minimum observation count
min_obs <- 50

#change path to own before running
setwd(WD_GBIF) #set working directory to path of raw data

#1
FILES = list.files(path = PATH_GBIF) #path of raw data
for(i in 1:length(FILES)){
  datfile <- read.delim(FILES[i], na.strings = c("NA", " ", "")) #read in the file
  Scientific.Name <- datfile[2,10] #find the specie name
  write.csv(datfile, file = paste0(SAVED_GBIFrenamed, Scientific.Name, ".csv"), row.names = FALSE) #save the file with the specie name
}

setwd(WD_GBIFrenamed) #path for the renamed files
#gets maps needed for subsetting and plotting
US <- map_data("state")#converts state map to dataframe for mapping with ggplot2
usa <- map("usa") #gets map for subsetting entries. Map of the lower 48 obtained through Package ‘maps’
#version 3.3.0 (Becker et al. 2018) in Program R (R Core Team 2016);
main <- map_data("usa", region=c("main"))#subsets map points to only mainland, no islands

#converts from dataframe to spatial polygon
df <- data.frame(main$long, main$lat) #dataframe of longitude and latitude 
p <- Polygon(df)
ps<- Polygons(list(p), ID=1)
USAsp <- SpatialPolygons(list(ps), proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")) #spatial polygon of the desired location
#from https://rstudio-pubs-static.s3.amazonaws.com/202536_7a122ff56e9f4062b6b012d9921afd80.html  


FILES1 = list.files(path = PATH_GBIFrename) #path of renamed raw data 
ext = NULL # create empty list

#2
for(i in 1:length(FILES1)){
  
  datfile <- read.csv(FILES1[i], header=TRUE) #read in the file from  the list
  datfile1 <- datfile[!is.na(datfile$decimallatitude),] #delete rows with missing latitude
  datfile2 <- datfile1[!is.na(datfile1$decimallongitude),] #delete rows with missing longitude
  datfile3 <- datfile2[!is.na(datfile2$year),] #delete rows with missing year
  
  #find issue and delete those rows
  df1 <- datfile3[!grepl("COUNTRY_COORDINATE_MISMATCH", datfile3$issue),]#COUNTRY_COORDINATE_MISMATCH
  df2 <- df1[!grepl("IDENTIFIED_DATE_UNLIKELY", df1$issue),]#IDENTIFIED_DATE_UNLIKELY
  df3 <- df2[!grepl("RECORDED_DATE_MISMATCH", df2$issue),]#RECORDED_DATE_MISMATCH
  df4 <- df3[!grepl("ZERO_COORDINATE", df3$issue),]#ZERO_COORDINATE
  df5 <- df4[!is.na(df4$gbifid),]#delete all NA values in gbifid column
  df6 <- df5[!is.na(df5$countrycode),]#delete all NA values in countrycode column
  df7 <- df6[grepl("US", df6$countrycode),] #returns only US entries

  specieName <- as.character(df1$species[1]) #specie name for file name
  USentries <- sum(df1$countrycode=="US") ## of US entries
  ext <- rbind(ext, data.frame(specieName, USentries)) #combines data frame with previous data frame
  write.csv(df7, file = paste0(SAVED_FIRSTCLEANED, specieName, ".csv"), row.names=FALSE) #output the file as csv

}


#3
FILES_NEW =  NULL #create empty list 
FILES2 = list.files(path = PATH_FIRSTCLEANED)
setwd(WD_FIRSTCLEANED) #path for the renamed files
#keep only files with more than 50 observations
for( i in 1:length(FILES2)){
  
  dat <- read.csv(FILES2[i], header=TRUE)
  row_count <- nrow(dat)
  
  if(row_count > min_obs){ #skip the header, count the file to to check for greater than 50 observations
    FILES_NEW[i] <- FILES2[i] #put the  > 50 in a few list
  }
}

FILES_NEW <- na.omit(FILES_NEW) #get rid of the NULL that was created when creating FILES_NEW
#manually added C. insignis back into list of 50 or more observation for representation
FILES_NEW[130] <- "Catostomus insignis.csv"
as.character(FILES_NEW)

for( i in 1:length(FILES_NEW)){
  datfile <- read.csv(FILES_NEW[i], header=TRUE) #read in the file from  the list
  specieName <- as.character(datfile$species[1]) #specie name for file name
  write.csv(datfile, file = paste0(SAVED_50FIRSTCLEANED, specieName, ".csv"), row.names=FALSE) #output the file as csv
  
}

#compare data from above loop to shapefiles 
FILES_SHP <- list.files(path="C:\\Users\\MimsLab\\Documents\\Ye\\shape files\\", pattern = "\\.shp$")
FILES_SHP1 <-sub("_", " ", FILES_SHP)
FILES_SHP1 <-sub(".shp", "", FILES_SHP1)

FILES_NEW1 <- sub(".csv","", FILES_NEW)

Both_listSHP_list130 <- FILES_SHP1 %in% FILES_NEW1
Both_listSHP_list130 <- as.data.frame(cbind(Both_listSHP_list130, FILES_SHP))
Both_listSHP_list130 <- Both_listSHP_list130[!(Both_listSHP_list130$Both_listSHP_list130 == "FALSE"),]
Both_listSHP_list130[1] <- NULL
Both_listSHP_list130[130,] <- "Catostomus_insignis.shp"
FILES_SHPfinal <- as.vector(t(Both_listSHP_list130))

ext = NULL # create empty list

#4
for (i in 1:length(FILES_SHPfinal)){ #loop
  maps <- st_read(paste(PATH_NATURESERVE,FILES_SHPfinal[i],sep=""), layer=substr(FILES_SHPfinal[i], 1, nchar(FILES_SHPfinal[i])-4)) #read in the shape files
   #downloads estuary shapefiles and combines them
  estuary <- st_read(PATH_ESTUARY, layer="merged_estuaries") # read in the estuary shaped file
  est <- st_transform(maps, 4326)
  ests <- as(est, "Spatial") #turning ests into a spatial object
  estsp <- as(ests, "SpatialPolygons") #turning estsp into a spatial polygon
  
  estsp <- spTransform(estsp, CRS="+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84") #transform to desire coordinate system
  proj4string(estsp) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")
  
 
  #downloads estuary shapefiles and combines them
  #*change to loop through maps*
  map1 <- st_transform(maps, 4326) #trasnform to desired coordinate
  map2 <- as(map1, "Spatial") #turn map1 into a spatial object
  map3 <- as(map2, "SpatialPolygons") #turn map2 into a spatal polygon
  
  map3 <- spTransform(map3, CRS="+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84") #projected to correct coordinate system
  proj4string(map3) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")
  
  #uploads csv, converts to SpatialPointsDataFrame for subsetting points to lower 48
  setwd(WD_50FIRSTCLEANED) #path to 50 or more first cleaned data from above loop
  
  dat.name <- substr(FILES_SHPfinal[i], 1, nchar(FILES_SHPfinal[i])-4) #take the shape file name
  dat.name2 <- strsplit(dat.name, "[_]") #get rid of underscores
  dat.name3 <- sapply(dat.name2, paste, collapse =" ") #add a space between names
  dat <- read.csv(paste(dat.name3,".csv", sep=""), header=TRUE) # read in the file with the new name
  elim <- dat[grep("Estuary",dat$locality),] #subsets those records with Estuary in column "locality"
  noest <- dat[!dat$gbifid %in% elim$gbifid,] #eliminates those records with Estuary in column "locality"
  datf <- data.frame(dat$decimallongitude, dat$decimallatitude, dat$year) #data frame of longitude, latitude, and year
  datfsp <- SpatialPointsDataFrame(datf, dat, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))#spatial data frame with correct projection
                                                               
  pts_subset <- datfsp[USAsp,]#subsets occurrence points to those of mainland lower 48 from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  pointsUS <- as.data.frame(datfsp)#changes spatial points to a dataframe for mapping with ggplot2
  
  #subset estuarine records
  subsetpt <- pts_subset[estsp,]#subsets occurrence points to those of estuary
  pointsest <- as.data.frame(subsetpt)#changes spatial points to a dataframe for mapping with ggplot2
  final <- pointsUS[(pointsUS$gbifid %in% pointsest$gbifid),] #takes out occurrence records in estuaries
  
  #cleans up extra columns in dataframe and writes file with subsetted records
  final$dat.decimallatitude <- NULL 
  final$dat.decimallongitude <- NULL
  final$dat.year <- NULL
  
  specieName <- as.character(final[2,10]) #get specie name for title of plot
  write.csv(final, file=paste0(SAVED_FINALDATA, dat.name3, ".csv"), row.names = FALSE) #save final data to new location
  
}

#calculate areas of unique HUC8 for each specie

setwd(WD_FINALDATA)
#read in the HUC8 shapefile
HUC8 <- readOGR(PATH_HUC8, "HUC8")
HUC8 <- spTransform(HUC8, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))
#read in final location data
FILES_FINAL = list.files(path = PATH_FINALDATA)

total_area_huc8 = NULL
specie_name = NULL 

#5
for(i in 1:length(FILES_FINAL)){
  dat <- read.csv(FILES_FINAL[i], header=TRUE) #read each file
  dat1 <- data.frame(dat$decimallongitude, dat$decimallatitude) #create a data frame of long and lat 
  
  #turning the data into a spatial dataframe with desired coordinate system
  dat1dfsp <- SpatialPointsDataFrame(dat1, dat, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # tell R that fish coordinates are in the same lat/lon reference system as the HUC8 data
  dat1dfsp <- spTransform(dat1dfsp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))
  
  dat$HUC8 <- over(dat1dfsp, HUC8)$HUC8 #store the HUC8 name as an attribute of the fish data 
  
  dat = dat[, c(1,45, 2:44)] #rearrange HUC8 to be the 2nd column
  
  Number_of_HUC8s <- data.frame(unique(dat$HUC8)) #number of unique HUC8's
  
  HUC8specie = HUC8[HUC8$HUC8 %in% Number_of_HUC8s$unique.dat.HUC8.,]  #extract unique HUC8 data for the specie 
  
  specieName <- as.character(dat$species[1]) #get specie name for title of plot
  
  #save HUC8specie as a shape file with the specie name
  writeOGR(obj = HUC8specie, dsn =SAVESHAPED_HUC8, 
           layer = specieName, driver = "ESRI Shapefile")
  
  write.csv(HUC8specie, file=paste0(SAVEDCSV_HUC8, specieName, ".csv"), row.names = FALSE)
  
  total_area_huc8[[i]] <- sum(HUC8specie$AreaSqKm) #sum the total area (square kilometer) for each unique HUC8's
  
  specie_name[[i]] <- specieName #list of specie names
  
}

area_all_huc8 <- as.data.frame(cbind(specie_name, total_area_huc8)) #combined specie and its total area

write.csv(area_all_huc8, file = SAVEDAREA_HUC8)  #save the dataframe

#calculate areas of unique HUC12 of each specie

#read in the HUC12 shapefile 
HUC12 <- readOGR(PATH_HUC12, "HUC12")
HUC12 <- spTransform(HUC12, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) #transform to correct coordinate system
#read in final location data
FILES_FINAL = list.files(path = PATH_FINALDATA)

total_area_huc12 = NULL # empty list  
specie_name = NULL #reset to empty list

#6
for(i in 1:length(FILES_FINAL)){
  dat <- read.csv(FILES_FINAL[i], header=TRUE) #read each file
  dat1 <- data.frame(dat$decimallongitude, dat$decimallatitude) #create a data frame of long and lat 
  
  #turning the data into a spatial dataframe with correct coordinate system
  dat1dfsp <- SpatialPointsDataFrame(dat1, dat, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  
  # tell R that fish coordinates are in the same lat/lon reference system as the HUC8 data
  dat1dfsp <- spTransform(dat1dfsp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  dat$HUC12 <- over(dat1dfsp, HUC12)$HUC12 #store the HUC12 name as an attribute of the fish data 
  
  dat = dat[, c(1,45, 2:44)] #rearrange HUC12 to be the 2nd column
  
  Number_of_HUC12s <- data.frame(unique(dat$HUC12)) #number of unique HUC12's
  
  HUC12specie = HUC12[HUC12$HUC12 %in% Number_of_HUC12s$unique.dat.HUC12.,]  #extract unique HUC12 data for the specie 
  
  specieName <- as.character(dat$species[1]) #get specie name for title of plot
  
  #save HUC12specie as a shape file with the specie name
  writeOGR(obj = HUC12specie, dsn =SAVEDSHAPED_HUC12, 
           layer = specieName, driver = "ESRI Shapefile")
  
  write.csv(HUC12specie, file=paste0(SAVEDCSV_HUC12, specieName, ".csv"), row.names = FALSE)
  
  total_area_huc12[[i]] <- sum(HUC12specie$AreaSqKm) #sum the total area (square kilometer) for each unique HUC12's
  
  specie_name[[i]] <- specieName #list of specie names
  
}

area_all_huc12 <- as.data.frame(cbind(specie_name, total_area_huc12)) #combine specie with its total area

write.csv(area_all_huc12, file = SAVEDAREA_HUC12) #save the dataframe


#Generate buffers for 1km, 5km, 10km, 20km

US <- map_data("state")#converts state map to dataframe for mapping with ggplot2
usa <- map("usa") #gets map for subsetting entries
main <- map_data("usa", region=c("main"))#subsets map points to only mainland, no islands

df <- data.frame(main$long, main$lat) #dataframe of longitude and latitude
p <- Polygon(df) #draws the df from the line above
ps<- Polygons(list(p), ID=1) 
USAsp <- SpatialPolygons(list(ps), proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")) #turn ps into a spatial polygon

FILES_BUFFER <- list.files(path=PATH_FINALDATA)#path for buffer clipped files
ext=NULL[]

total_area_1km = NULL #create empty list
total_area_5km = NULL #create empty list
total_area_10km = NULL #create empty list
total_area_20km = NULL #create empty list

specie_name = NULL #create empty list

# for loop to generate buffers for each specie 

#7
for (i in 1:length(FILES_BUFFER)){
  #1km
  dat <-read.csv(FILES_BUFFER[i], header=TRUE) #read in file from list
  
  dat1 <- dat
  datf <- data.frame(dat$decimallongitude, dat$decimallatitude) #create dataframe of longitude and latitude
  datfsp <- SpatialPointsDataFrame(datf,dat1, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))#create spatial df with correct projection
                                                                 
  d_mrc <- spTransform(datfsp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")) 
  
  d_mrc_buffer_range <- gBuffer(d_mrc, width = 1, byid= FALSE) #create a buffer of 1 km 
  
  #project back to original spatial dataframe for plotting
  d_mrc_buffer <- spTransform(d_mrc_buffer_range, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
                                                      
  d_mrc_buffer_fort <- fortify(d_mrc_buffer) #convert to dataframe
  
  pts_subset <- datfsp[USAsp,]#subsets occurrence points to those of mainland lower 48 from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  pointsUS <- as.data.frame(pts_subset)#changes spatial points to a dataframe for mapping with ggplot2
  
  specieName <- dat$species[1] #get specie name for title of plot
  
  #subset estuarine records
  subsetpt <- pts_subset#[estsp,]#subsets occurrence points to those of estuary
  pointsest <- as.data.frame(subsetpt)#changes spatial points to a dataframe for mapping with ggplot2
  final <- pointsUS#[!pointsUS$gbifid %in% pointsest$gbifid,] #takes out occurrence records in estuaries
  
  #cleans up extra columns in dataframe and writes file with subsetted records
  final$dat1.decimallatitude <- NULL
  final$dat1.decimallongitude <- NULL
  final$dat1.year <- NULL
  
  #graph the data points with buffer
  ggplot()+
    geom_polygon(data=US, aes(x=long, y=lat, group=group), fill=NA, color="black")+ #USmap with state boundaries
    geom_point(data=pointsUS,shape=21, color="gray37",alpha=0.4, aes(x=pointsUS$decimallongitude, y=pointsUS$decimallatitude, fill=pointsUS$year))+ #plot points of occurrence
    scale_fill_gradientn(colors=brewer.pal(11, "Spectral"))+ #fills in the points with palette Spectral, limit 11 divisions 
    labs(fill="Year")+ #labels legend title
    geom_path(data=d_mrc_buffer_fort, aes(long, lat, group=group), color="red") +
    coord_map("albers", lat0=30, lat1=40) + #designates coordinate system as albers, used parameters found on website https://rud.is/b/2015/07/24/a-path-towards-easier-map-projection-machinations-with-ggplot2/
    ggtitle(specieName) #title for plot      
  
  ggsave(file=paste0(SAVEDBUFFER_1KM, specieName, ".pdf")) #saves plot as pdf file
  
  total_area_1km[[i]] <-  gArea(d_mrc_buffer_range, byid = FALSE) #generate list of area by specie
  specie_name[[i]] <- specieName #list of specie name
  
  #save d_mrc as shapefile with specie name
  writeOGR(obj = d_mrc, dsn =SAVEDSHAPED_1KM, 
           layer = specieName, driver = "ESRI Shapefile")
  
  #5km
  dat <-read.csv(FILES_BUFFER[i], header=TRUE) #read in the files from list
  
  dat1 <- dat
  datf <- data.frame(dat$decimallongitude, dat$decimallatitude) #create dataframe of longitude and latitude
  datfsp <- SpatialPointsDataFrame(datf,dat1, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")) 
  
  d_mrc <- spTransform(datfsp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")) #project to correct coordinate system to generate buffer
  
  d_mrc_buffer_range <- gBuffer(d_mrc, width = 5, byid= FALSE) #create buffer of size 5km
  
  #transform back to original spatial data frame coordinates for plotting
  d_mrc_buffer <- spTransform(d_mrc_buffer_range, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
                                                      
  d_mrc_buffer_fort <- fortify(d_mrc_buffer) #convert to dataframe
  
  pts_subset <- datfsp[USAsp,]#subsets occurrence points to those of mainland lower 48 from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  pointsUS <- as.data.frame(pts_subset)#changes spatial points to a dataframe for mapping with ggplot2
  
  
  #subset estuarine records
  subsetpt <- pts_subset#[estsp,]#subsets occurrence points to those of estuary
  pointsest <- as.data.frame(subsetpt)#changes spatial points to a dataframe for mapping with ggplot2
  final <- pointsUS#[!pointsUS$gbifid %in% pointsest$gbifid,] #takes out occurrence records in estuaries
  
  #cleans up extra columns in dataframe and writes file with subsetted records
  final$dat1.decimallatitude <- NULL
  final$dat1.decimallongitude <- NULL
  final$dat1.year <- NULL
  specieName <- dat$species[1]
  
  ggplot()+
    geom_polygon(data=US, aes(x=long, y=lat, group=group), fill=NA, color="black")+ #USmap with state boundaries
    geom_point(data=pointsUS,shape=21, color="gray37",alpha=0.4, aes(x=pointsUS$decimallongitude, y=pointsUS$decimallatitude, fill=pointsUS$year))+ #plot points of occurrence
    scale_fill_gradientn(colors=brewer.pal(11, "Spectral"))+ #fills in the points with palette Spectral, limit 11 divisions 
    labs(fill="Year")+ #labels legend title
    geom_path(data=d_mrc_buffer_fort, aes(long, lat, group=group), color="red") +
    coord_map("albers", lat0=30, lat1=40) + #designates coordinate system as albers, used parameters found on website https://rud.is/b/2015/07/24/a-path-towards-easier-map-projection-machinations-with-ggplot2/
    ggtitle(specieName) #title for plot      
  
  ggsave(file=paste0(SAVEDBUFFER_5KM, specieName, ".pdf")) #saves plot as pdf file
  
  total_area_5km[[i]] <-  gArea(d_mrc_buffer_range, byid = FALSE) #generate list of area by specie
  specie_name[[i]] <- specieName #list of specie name
  
  #save object d_mrc (already transformed to desired coordinate) as a shape file
  writeOGR(obj = d_mrc, dsn =SAVEDSHAPED_5KM, 
           layer = specieName, driver = "ESRI Shapefile")
  
  #10km
  dat <-read.csv(FILES_BUFFER[i], header=TRUE)
  
  dat1 <- dat
  datf <- data.frame(dat$decimallongitude, dat$decimallatitude) #create df of longitude and latitude
  
  #create a spatial data frame from long/lat
  datfsp <- SpatialPointsDataFrame(datf,dat1, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")) 
  
  d_mrc <- spTransform(datfsp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")) #project to correct coordinate system
  
  d_mrc_buffer_range <- gBuffer(d_mrc, width = 10, byid= FALSE) #create buffer of size 10km
  
  #transform buffer back to original spatial data frame for plotting
  d_mrc_buffer <- spTransform(d_mrc_buffer_range, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
                                                      
  d_mrc_buffer_fort <- fortify(d_mrc_buffer)
  
  pts_subset <- datfsp[USAsp,]#subsets occurrence points to those of mainland lower 48 from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  pointsUS <- as.data.frame(pts_subset)#changes spatial points to a dataframe for mapping with ggplot2
  
  #subset estuarine records
  subsetpt <- pts_subset#[estsp,]#subsets occurrence points to those of estuary
  pointsest <- as.data.frame(subsetpt)#changes spatial points to a dataframe for mapping with ggplot2
  final <- pointsUS#[!pointsUS$gbifid %in% pointsest$gbifid,] #takes out occurrence records in estuaries
  
  #cleans up extra columns in dataframe and writes file with subsetted records
  final$dat1.decimallatitude <- NULL
  final$dat1.decimallongitude <- NULL
  final$dat1.year <- NULL
  specieName <- dat$species[1]
  
  ggplot()+
    geom_polygon(data=US, aes(x=long, y=lat, group=group), fill=NA, color="black")+ #USmap with state boundaries
    geom_point(data=pointsUS,shape=21, color="gray37",alpha=0.4, aes(x=pointsUS$decimallongitude, y=pointsUS$decimallatitude, fill=pointsUS$year))+ #plot points of occurrence
    scale_fill_gradientn(colors=brewer.pal(11, "Spectral"))+ #fills in the points with palette Spectral, limit 11 divisions 
    labs(fill="Year")+ #labels legend title
    geom_path(data=d_mrc_buffer_fort, aes(long, lat, group=group), color="red") +
    coord_map("albers", lat0=30, lat1=40) + #designates coordinate system as albers, used parameters found on website https://rud.is/b/2015/07/24/a-path-towards-easier-map-projection-machinations-with-ggplot2/
    ggtitle(specieName) #title for plot      
  
  ggsave(file=paste0(SAVEDBUFFER_10KM, specieName, ".pdf")) #saves plot as pdf file
  
  total_area_10km[[i]] <-  gArea(d_mrc_buffer_range, byid = FALSE) #generate list of area by specie
  specie_name[[i]] <- specieName #list of specie name
 
  #save d_mrc as a shapefile with specie name
  writeOGR(obj = d_mrc, dsn =SAVEDSHAPED_10KM,
           layer = specieName, driver = "ESRI Shapefile")
   
  #20km
  dat <-read.csv(FILES_BUFFER[i], header=TRUE)
  
  dat1 <- dat
  datf <- data.frame(dat$decimallongitude, dat$decimallatitude) #create df of longitude and latitude
  #create spatial data frame iwht datf and dat1 with desired coordinate system
  datfsp <- SpatialPointsDataFrame(datf,dat1, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84")) 
  
  #transform to new desired coordinate system to generate buffer
  d_mrc <- spTransform(datfsp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")) 
  
  d_mrc_buffer_range <- gBuffer(d_mrc, width = 20, byid= FALSE) #create buffer of size 20km
  
  #transform back to original spatial data frame for plotting
  d_mrc_buffer <- spTransform(d_mrc_buffer_range, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84"))
                                               
  #create a dataframe from d_mrc_buffer       
  d_mrc_buffer_fort <- fortify(d_mrc_buffer)
  
  pts_subset <- datfsp[USAsp,]#subsets occurrence points to those of mainland lower 48 from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
  pointsUS <- as.data.frame(pts_subset)#changes spatial points to a dataframe for mapping with ggplot2
  
  #subset estuarine records
  subsetpt <- pts_subset#[estsp,]#subsets occurrence points to those of estuary
  pointsest <- as.data.frame(subsetpt)#changes spatial points to a dataframe for mapping with ggplot2
  final <- pointsUS#[!pointsUS$gbifid %in% pointsest$gbifid,] #takes out occurrence records in estuaries
  
  #cleans up extra columns in dataframe and writes file with subsetted records
  final$dat1.decimallatitude <- NULL
  final$dat1.decimallongitude <- NULL
  final$dat1.year <- NULL
  specieName <- dat$species[1]
  
  ggplot()+
    geom_polygon(data=US, aes(x=long, y=lat, group=group), fill=NA, color="black")+ #USmap with state boundaries
    geom_point(data=pointsUS,shape=21, color="gray37",alpha=0.4, aes(x=pointsUS$decimallongitude, y=pointsUS$decimallatitude, fill=pointsUS$year))+ #plot points of occurrence
    scale_fill_gradientn(colors=brewer.pal(11, "Spectral"))+ #fills in the points with palette Spectral, limit 11 divisions 
    labs(fill="Year")+ #labels legend title
    geom_path(data=d_mrc_buffer_fort, aes(long, lat, group=group), color="red") +
    coord_map("albers", lat0=30, lat1=40) + #designates coordinate system as albers, used parameters found on website https://rud.is/b/2015/07/24/a-path-towards-easier-map-projection-machinations-with-ggplot2/
    ggtitle(specieName) #title for plot      
  
  ggsave(file=paste0(SAVEDBUFFER_20KM, specieName, ".pdf")) #saves plot as pdf file
  
  total_area_20km[[i]] <-  gArea(d_mrc_buffer_range, byid = FALSE) #generate list of area by specie
  specie_name[[i]] <- specieName #list of specie name
  
  #save d_mrc as a shapefile with specie name
  writeOGR(obj = d_mrc, dsn =SAVEDSHAPeD_20KM,
           layer = specieName, driver = "ESRI Shapefile")
}

area_1Km <- as.data.frame(cbind(specie_name, total_area_1km)) #bind specie name and area to create data frame

write.csv(area_1Km, file = SAVEDAREA_1KM) #save the dataframe

area_5Km <- as.data.frame(cbind(specie_name, total_area_5km)) #bind specie name and area to create data frame

write.csv(area_5Km, file = SAVEDAREA_5KM) #save the dataframe

area_10Km <- as.data.frame(cbind(specie_name, total_area_10km)) #bind specie name and area to create data frame

write.csv(area_10Km, file = SAVEDAREA_10KM) #save the dataframe

area_20Km <- as.data.frame(cbind(specie_name, total_area_20km)) #bind specie name and area to create data frame

write.csv(area_20Km, file = SAVEDAREA_20KM) #save the dataframe

#Output the MCP graph for each specie
setwd(WD_FINALDATA)

FILES_MCP <- list.files(path = PATH_FINALDATA) #path where you store the final cleaned data
specie_name <- NULL #empty list  
mcp_area <- NULL #empty list

#8
for(i in 1:length(FILES_MCP)){
  datfile <- read.delim(FILES_MCP[i], na.strings = c("NA", " ", ""), sep=",")
  specieName1 <- as.character(datfile[2,10])
  dfxy <- data.frame(datfile$decimallongitude, datfile$decimallatitude)
  
  dfxy <- SpatialPoints(dfxy)
  
  proj4string(dfxy) <- CRS('+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84')
  
  dfxy_alber <- spTransform(dfxy,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  cp <- mcp(dfxy_alber, percent = 100)
  
  df_points <- as.data.frame(dfxy_alber)
  
  pdf(file = paste0(SAVEDMCP_MAP, specieName1, ".pdf"))
  
  plot(cp, main = specieName1)
  points(df_points, col="blue", pch = 20)
  
  dev.off()
  
  writeOGR(obj = cp, dsn =SAVEDSHAPED_MCP,
           layer = specieName1, driver = "ESRI Shapefile")
  
  mcp_area[[i]] <- mcp.area(dfxy_alber ,unout  = "km2", percent = 100, plotit = FALSE)
  specie_name[[i]] <- specieName1
}

mcp_area1 <- data.frame(matrix(unlist(mcp_area), nrow = length(mcp_area), byrow = T)) #turn the list into a dataframe
names(mcp_area1)[1] <- "MCP area (Km^2)"
mcp_all <- as.data.frame(cbind(specie_name, mcp_area1))
write.csv(mcp_all, file = SAVEDAREA_MCP) #save the dataframe




