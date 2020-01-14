rm(list=ls())

library(rgeos)
library(rgdal)

#This code calculates area of occupancy for each species based on NatureServe distribution maps
#Version 3.0 - "Digital Distribution of Native U.S. Fishes by Watershed"
#http://www.natureserve.org/conservation-tools/data-maps-tools/digital-distribution-native-us-fishes-watershed

PATH_NATURESERVE <- "NatureServeMapsProjected"
FILES_SHP <- list.files(path="G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/NatureServeMapsProjected/", pattern = ".shp$")
WD_NATURESERVE <- "G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/NatureServeMapsProjected/"
setwd(WD_NATURESERVE)

specie_list <- NULL
area_list <- NULL

for (i in 1:length(FILES_SHP)){
  dat.name <- substr(FILES_SHP[i], 1, nchar(FILES_SHP[i])-4)
  specie <- readOGR(dsn="G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/NatureServeMapsProjected", layer=dat.name)
  specie <- spTransform(specie, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))
  area_list[i] <- gArea(specie)
  specie_list[i] <- dat.name
  
}
combined <- cbind(specie_list, area_list)
write.csv(combined, file=paste0("G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/nature serve area.csv"), row.names = FALSE) #save final data to new location
