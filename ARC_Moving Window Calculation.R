# National Fishes Vulnerability Assessment Project - "ARC_Moving Window Calculation.R"
# Revised by Sam Silknetter, 09June2021
# Script to create a moving window output for all PRISM variables.

library(sp)
library(rgdal)
library(maps)
library(zoo)
library(reshape2)


#### Precipitation [ppt]
# Set path. 
PATH_ppt <- "/work/cascades/silknets/singularity/Annual_Watershed_Data/HUC12_ppt_Combined.csv"

# Read in the PRISM data from .csv.
dat_prism_ppt <- read.csv(PATH_ppt, header=TRUE, check.names = FALSE) 

# Transpose dataframe and remove hucID column.
RW_min_ppt <- as.data.frame(t(dat_prism_ppt))[-1,]

# Use rollapply() to generate the moving window.
RW_min_ppt <- as.data.frame(rollapply(RW_min_ppt, width = 30, by = 1, FUN = mean, na.rm = TRUE, align = "right", partial=T))

# Transpose the matrix back to original format.
RW_min_ppt <- as.data.frame(t(RW_min_ppt))

# Remove first 29 columns, which correspond to incomplete windows (1895-1924).
RW_min_ppt <- RW_min_ppt[,30:123]

# Name columns as years and re-assign HUC-12 ID from original file.
names(RW_min_ppt) <- c(1924:2017)
RW_min_ppt$huc12 <- dat_prism_ppt$huc12 

write.csv(RW_min_ppt, file = "ppt_MW_09June2021.csv")
rm(list = ls())



#### Maximum Temperature [tmax]
# Set path.
PATH_tmax <- "/work/cascades/silknets/singularity/Annual_Watershed_Data/HUC12_tmax_Combined.csv"

# Read in the PRISM data from .csv.
dat_prism_tmax <- read.csv(PATH_tmax, header=TRUE, check.names = FALSE) 

# Transpose dataframe and remove hucID column.
RW_min_tmax <- as.data.frame(t(dat_prism_tmax))[-1,]

# Use rollapply() to generate the moving window.
RW_min_tmax <- as.data.frame(rollapply(RW_min_tmax, width = 30, by = 1, FUN = max, na.rm = TRUE, align = "right", partial=T))

# Transpose the matrix back to original format.
RW_min_tmax <- as.data.frame(t(RW_min_tmax))

# Remove first 29 columns, which correspond to incomplete windows (1895-1924).
RW_min_tmax <- RW_min_tmax[,30:123]

# Name columns as years and re-assign HUC-12 ID from original file.
names(RW_min_tmax) <- c(1924:2017)
RW_min_tmax$huc12 <- dat_prism_tmax$huc12

write.csv(RW_min_tmax, file = "tmax_MW_09June2021.csv")
rm(list = ls())



#### Minimum Temperature [tmin]
# Set path.
PATH_tmin <- "/work/cascades/silknets/singularity/Annual_Watershed_Data/HUC12_tmin_Combined.csv"

# Read in the PRISM data from .csv.
dat_prism_tmin <- read.csv(PATH_tmin, header=TRUE, check.names = FALSE) 

# Transpose dataframe and remove hucID column.
RW_min_tmin <- as.data.frame(t(dat_prism_tmin))[-1,] 

# Use rollapply() to generate the moving window.
RW_min_tmin <- as.data.frame(rollapply(RW_min_tmin, width = 30, by = 1, FUN = min, na.rm = TRUE, align = "right", partial=T))

# Transpose the matrix back to original format.
RW_min_tmin <- as.data.frame(t(RW_min_tmin))

# Remove first 29 columns, which correspond to incomplete windows (1895-1924).
RW_min_tmin <- RW_min_tmin[,30:123]

# Name columns as years and re-assign HUC-12 ID from original file.
names(RW_min_tmin) <- c(1924:2017)
RW_min_tmin$huc12 <- dat_prism_tmin$huc12

write.csv(RW_min_tmin, file = "tmin_MW_09June2021.csv")
rm(list = ls())



#### Maximum Temperature August [tmaxAug]
# Set path.
PATH_tmaxAug <- "/work/cascades/silknets/singularity/Annual_Watershed_Data/HUC12_tmax_Aug_Combined.csv"

# Read in the PRISM data from .csv.
dat_prism_tmaxAug <- read.csv(PATH_tmaxAug, header=TRUE, check.names = FALSE) 

# Transpose dataframe and remove hucID column.
RW_min_tmaxAug <- as.data.frame(t(dat_prism_tmaxAug))[-1,] 

# Use rollapply() to generate the moving window.
RW_min_tmaxAug <- as.data.frame(rollapply(RW_min_tmaxAug, width = 30, by = 1, FUN = max, na.rm = TRUE, align = "right", partial=T))

# Transpose the matrix back to original format.
RW_min_tmaxAug <- as.data.frame(t(RW_min_tmaxAug))

# Remove first 29 columns, which correspond to incomplete windows (1895-1924).
RW_min_tmaxAug <- RW_min_tmaxAug[,30:123]

# Name columns as years and re-assign HUC-12 ID from original file.
names(RW_min_tmaxAug) <- c(1924:2017)
RW_min_tmaxAug$huc12 <- dat_prism_tmaxAug$huc12

write.csv(RW_min_tmaxAug, file = "tmaxAug_MW_09June2021.csv")
rm(list = ls())



#### Minimum Temperature January [tminJan]
# Set path.
PATH_tminJan <- "/work/cascades/silknets/singularity/Annual_Watershed_Data/HUC12_tmin_Jan_Combined.csv"

# Read in the PRISM data from .csv.
dat_prism_tminJan <- read.csv(PATH_tminJan, header=TRUE, check.names = FALSE) 

# Transpose dataframe and remove hucID column.
RW_min_tminJan <- as.data.frame(t(dat_prism_tminJan))[-1,]

# Use rollapply() to generate the moving window.
RW_min_tminJan <- as.data.frame(rollapply(RW_min_tminJan, width = 30, by = 1, FUN = min, na.rm = TRUE, align = "right", partial=T))

# Transpose the matrix back to original format.
RW_min_tminJan <- as.data.frame(t(RW_min_tminJan))

# Remove first 29 columns, which correspond to incomplete windows (1895-1924).
RW_min_tminJan <- RW_min_tminJan[,30:123]

# Name columns as years and re-assign HUC-12 ID from original file.
names(RW_min_tminJan) <- c(1924:2017)
RW_min_tminJan$huc12 <- dat_prism_tminJan$huc12

write.csv(RW_min_tminJan, file = "tminJan_MW_09June2021.csv")