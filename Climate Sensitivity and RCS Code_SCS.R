# National Fishes Vulnerability Assessment Project - "Climate Sensitivity and RCS Code_SCS.R"
# Revised by Sam Silknetter, 13April2020

# This code calculates Relative Climate Sensitivity index (RCS) for each species and range metric. 
#  Can be modified to calculate RCS for other AOO scales. Resulting output from each section exported 
#  as an independent .csv file for the RCS using that range size metric.  

# Install and load necessary libraries.
library(scales)
library(readr)
library(tidyverse)
library(rgbif)
library(sf)
library(rgdal)
library(sp)
library(rgeos)
library(adehabitatHR)

PATH_RCS <- "G:/Shared drives/NFVAP - National RCS/R Code/National-RCS/RCS"

## For HUC 12

#1a. Imports standard deviations for climatic data that indicate climate breath for each species at the HUC12 level.
StandardDevs_HUC12 <- read.csv(file="./HUC12_PRISM_CompleteSD.csv")

#1b. Scales standard deviations for each climatic variable (n=5) between 0 and 1.
StandardDevs_HUC12$SD_TMax_Scale <- ((StandardDevs_HUC12$matrix_sdtmax - min(StandardDevs_HUC12$matrix_sdtmax))
      /(max(StandardDevs_HUC12$matrix_sdtmax, na.rm=TRUE) - min(StandardDevs_HUC12$matrix_sdtmax, na.rm=TRUE)))

StandardDevs_HUC12$SD_ppt_Scale <- ((StandardDevs_HUC12$matrix_sdppt - min(StandardDevs_HUC12$matrix_sdppt))
      /(max(StandardDevs_HUC12$matrix_sdppt, na.rm=TRUE) - min(StandardDevs_HUC12$matrix_sdppt, na.rm=TRUE)))

StandardDevs_HUC12$SD_TMaxAug_Scale <- ((StandardDevs_HUC12$matrix_sdtmaxaug - min(StandardDevs_HUC12$matrix_sdtmaxaug))
      /(max(StandardDevs_HUC12$matrix_sdtmaxaug, na.rm=TRUE) - min(StandardDevs_HUC12$matrix_sdtmaxaug, na.rm=TRUE)))

StandardDevs_HUC12$SD_TMin_Scale <- ((StandardDevs_HUC12$matrix_sdtmin - min(StandardDevs_HUC12$matrix_sdtmin))
      /(max(StandardDevs_HUC12$matrix_sdtmin, na.rm=TRUE) - min(StandardDevs_HUC12$matrix_sdtmin, na.rm=TRUE)))

StandardDevs_HUC12$SD_TMinJan_Scale <- ((StandardDevs_HUC12$matrix_sdtminjan - min(StandardDevs_HUC12$matrix_sdtminjan))
      /(max(StandardDevs_HUC12$matrix_sdtminjan, na.rm=TRUE) - min(StandardDevs_HUC12$matrix_sdtminjan, na.rm=TRUE)))

#1c. Calculate the mean of the 5 scaled standard deviations to derive a mean relative climate breath for each species.
StandardDevs_HUC12$CS <- ((StandardDevs_HUC12$SD_TMax_Scale + StandardDevs_HUC12$SD_ppt_Scale 
                           + StandardDevs_HUC12$SD_TMaxAug_Scale + StandardDevs_HUC12$SD_TMin_Scale 
                           + StandardDevs_HUC12$SD_TMinJan_Scale)/5)

#1d. Rename dataframe columns to match, and thus that the tables can be merged.
names(AOOs)[1]<-"Species"
names(StandardDevs_HUC12)[1] <-"Species"

#1e. Merge AOO and relative climate breadth dataframes.
RCS_Data_HUC12 <- merge(AOOs,StandardDevs_HUC12, by.x="Species")

#1f. Scales AOOs defined by HUC12 (km) between 0 and 1.
RCS_Data_HUC12$HUC12_Scale <- ((RCS_Data_HUC12$total_area_huc12 - min(RCS_Data_HUC12$total_area_huc12))
       /(max(RCS_Data_HUC12$total_area_huc12, na.rm=TRUE) - min(RCS_Data_HUC12$total_area_huc12, na.rm=TRUE)))

#1g. Scaled AOO for HUC12 and CS values substracted from 1.
RCS_Data_HUC12$One_Minus_ScaledHUC12 <- (1 - RCS_Data_HUC12$HUC12_Scale)
RCS_Data_HUC12$One_Minus_CSHUC12 <- (1 - RCS_Data_HUC12$CS)

#1h. Calculate Relative Climate Sensitivity Index by taking average of "1-AOO" and "1-CS".
#  Where large values of RCS indicate species with small AOO and low range of climate variables.
RCS_Data_HUC12$RCS <- ((RCS_Data_HUC12$One_Minus_ScaledHUC12 + RCS_Data_HUC12$One_Minus_CSHUC12)/2)

#1i. Exports RCS data table containing RCS values for the HUC 12 scale.
write.csv(RCS_Data_HUC12, file = paste0(PATH_RCS, "RCS_1km_09Jan2020.csv"), row.names=FALSE)


######## For 1km Point Buffers ########

MCM, I think reorganizing this section is important so that it matches the workflow used for  HUC-12. For example, Step 2a pulls a file called "complete_sd" for the HUC, yet for point buffers we pull all variables individually. Also, I feel like the step number/letter combinations should match, except where that would affect order of operations. 

An alternative is to use a loop to have the code run the same protocol for both input datasets, but I'm not sure how to go about that. 

2a. Imports standard deviations for climatic data that indicate climate breath for each species at the HUC12 level
```{r}
SD_1km_ppt <- read.csv(file="./1km_PRISM/sd_ppt.csv")

SD_1km_tmax <- read.csv(file="./1km_PRISM/sd_tmax.csv")

SD_1km_tmaxAug <- read.csv(file="./1km_PRISM/sd_tmaxAug.csv")

SD_1km_tmin <- read.csv(file="./1km_PRISM/sd_tmin.csv")

SD_1km_tminJan <- read.csv(file="./1km_PRISM/sd_tminJan.csv")

SD_1km_all <- data.frame(SD_1km_ppt, SD_1km_tmax, SD_1km_tmaxAug, SD_1km_tmin, SD_1km_tminJan)
SD_1km_all <- SD_1km_all[ -c(3,5,7,9) ]

colnames(SD_1km_all)[colnames(SD_1km_all)=="matrix_sd"] <- "SD_1km_ppt"
colnames(SD_1km_all)[colnames(SD_1km_all)=="matrix_sd.1"] <- "SD_1km_tmax"
colnames(SD_1km_all)[colnames(SD_1km_all)=="matrix_sd.2"] <- "SD_1km_tmaxAug"
colnames(SD_1km_all)[colnames(SD_1km_all)=="matrix_sd.3"] <- "SD_1km_tmin"
colnames(SD_1km_all)[colnames(SD_1km_all)=="matrix_sd.4"] <- "SD_1km_tminJan"
```

2b. Scales standard deviations for each climatic variable (n=5) between 0 and 1
```{r}
SD_1km_all$SD_ppt_Scale <- ((SD_1km_all$SD_1km_ppt - min(SD_1km_all$SD_1km_ppt))/(max(SD_1km_all$SD_1km_ppt, na.rm=TRUE) - min(SD_1km_all$SD_1km_ppt, na.rm=TRUE)))

SD_1km_all$SD_TMax_Scale <- ((SD_1km_all$SD_1km_tmax - min(SD_1km_all$SD_1km_tmax))/(max(SD_1km_all$SD_1km_tmax, na.rm=TRUE) - min(SD_1km_all$SD_1km_tmax, na.rm=TRUE)))

SD_1km_all$SD_TMaxAug_Scale <- ((SD_1km_all$SD_1km_tmaxAug - min(SD_1km_all$SD_1km_tmaxAug))/(max(SD_1km_all$SD_1km_tmaxAug, na.rm=TRUE) - min(SD_1km_all$SD_1km_tmaxAug, na.rm=TRUE))) 

SD_1km_all$SD_TMin_Scale <- ((SD_1km_all$SD_1km_tmin - min(SD_1km_all$SD_1km_tmin))/(max(SD_1km_all$SD_1km_tmin, na.rm=TRUE) - min(SD_1km_all$SD_1km_tmin, na.rm=TRUE))) 

SD_1km_all$SD_TMinJan_Scale <- ((SD_1km_all$SD_1km_tminJan - min(SD_1km_all$SD_1km_tminJan))/(max(SD_1km_all$SD_1km_tminJan, na.rm=TRUE) - min(SD_1km_all$SD_1km_tminJan, na.rm=TRUE)))
```

2c. Calculates the mean of the 5 scaled standard deviations to derive a mean relative climate breath for each species
```{r}
SD_1km_all$CS <- ((SD_1km_all$SD_TMax_Scale + SD_1km_all$SD_ppt_Scale + SD_1km_all$SD_TMaxAug_Scale + SD_1km_all$SD_TMin_Scale + SD_1km_all$SD_TMinJan_Scale)/5)
```

2d. Rename columns in AOO dataframe and relative climate breadth dataframe so that they match, and thus that the tables can be merged. **Not sure why the species names have "_PT" at the end. I assume that is to distinguish them as 'point' data (i.e. 1, 10km point buffers), but I don't see why that's necessary. Assuming it is not, I will rename the outputs from the previous script file (), as well as condense SD into a single file. 
```{r}
names(SD_1km_all)[1] <-"Species"
```

2e. Merge AOO dataframe and relative climate breadth dataframe. 
```{r}
SD_1km_all$Species <- All_AOO$Species 
RCS_Data_1km <- merge(All_AOO,SD_1km_all, by.x="Species")
```

2f. Scales AOOs defined by 1km (km) between 0 and 1
```{r}
RCS_Data_1km$Buffer1km_Scale <- ((RCS_Data_1km$total_area_1km - min(RCS_Data_1km$total_area_1km))/(max(RCS_Data_1km$total_area_1km, na.rm=TRUE) - min(RCS_Data_1km$total_area_1km, na.rm=TRUE))) #for 1km
```

2g. Scaled AOO for 1km and CS values substracted from 1
```{r}
RCS_Data_1km$One_Minus_Scaled1km <- (1 - RCS_Data_1km$Buffer1km_Scale)
RCS_Data_1km$One_Minus_CS1km <- (1 - RCS_Data_1km$CS)
```

2h. Calculate Relative Climate Sensitivity Index by taking average of "1-AOO" and "1-CS" (large values of RCS indicate species with small AOO and low range of climate variables)
```{r}
RCS_Data_1km$RCS <- ((RCS_Data_1km$One_Minus_Scaled1km + RCS_Data_1km$One_Minus_CS1km)/2)
```

2i. Exports RCS data table containing RCS values for the 1km point buffer scale
```{r}
write.csv(RCS_Data_1km,"./Output Files/RCS_1km_09Jan2020.csv")
```