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