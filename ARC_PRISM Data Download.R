# National Fishes Vulnerability Assessment Project - "ARC_PRISM Data Download.R"
# Revised by Sam Silknetter, 01June2021

# This code is used to download PRISM climate data for the climate variables of choice. 
# PRISM dataset descriptons: https://prism.oregonstate.edu/documents/PRISM_datasets.pdf

# Load PRISM library (Version 0.2.0 or newer is required).
library(prism)

# Set dowload directory to ensure proper archival of PRISM data.
prism_set_dl_dir("/home/silknets/NFVAP/PRISM")


# Download PRISM Data for use with the NFVAP.
options(prism.path = "/home/silknets/NFVAP/PRISM/Tmax/")
get_prism_annual(type = "tmax", year = 1895:2017, keepZip = FALSE)

options(prism.path = "/home/silknets/NFVAP/PRISM/Tmin/")
get_prism_annual(type = "tmin", year = 1895:2017, keepZip = FALSE)

options(prism.path = "/home/silknets/NFVAP/PRISM/Ppt/")
get_prism_annual(type = "ppt", year = 1895:2017, keepZip = FALSE)

options(prism.path = "/home/silknets/NFVAP/PRISM/TmaxAug/")
get_prism_monthlys(type = "tmax", year = 1895:2017, mon = 8, keepZip = FALSE)

options(prism.path = "/home/silknets/NFVAP/PRISM/TminJan/")
get_prism_monthlys(type = "tmin", year = 1895:2017, mon = 1, keepZip = FALSE)



# Download additional PRISM Data for ScienceBase data product release. 
options(prism.path = "/work/cascades/silknets/singularity/PRISM/tdmean/")
get_prism_annual(type = "tdmean", year = 1895:1911, keepZip = FALSE)

options(prism.path = "/work/cascades/silknets/singularity/PRISM/vpdmin/")
get_prism_annual(type = "vpdmin", year = 1895:2017, keepZip = FALSE)

options(prism.path = "/work/cascades/silknets/singularity/PRISM/vpdmax/")
get_prism_annual(type = "vpdmax", year = 1895:2017, keepZip = FALSE)