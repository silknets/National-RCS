# National-Fishes-Vulnerability-Assessment-Project
National Fishes Vulnerability Assessment Project

===============================

Sam Silknetter (SCS) Updates 06/10/2021

-------------------------------

# Purpose

This repository contains R code for analyzing species rarity and climate sensitivity for [N species to be added upon final publication, approximately 140] fish species native to the contiguous (Lower 48) United States. Observations of species occurrence are accessed through the Global Biodiversity Information Facility (GBIF) using the rgbif package. Occurrence points are subject to a set of filters. Once filtered, occurrence points are then used to determine area of occupancy (AOO) for at the grain size of occupied watersheds (USGS HUC-12 sub-basins). The PRISM climate model (AN81m)[1] is then used to determine species climate sensitivity (CS) for each species' grain-specific AOO. Climate variables examined include annual precipitation (ppt), daily maximum temperature (tmax), daily minimum temperature (tmin), monthly maximum temperature in August (tmaxAug), and monthly minimum temperature in January (tminJan). At each grain size, CS and AOO metrics are combined into a relative rarity and climate sensitivity index (RCS)[2].

Additional planned analyses will use the FishTraits[3] database to assess relationships between geographical (RCS) and functional (traits-based) measures of sensitivity. Upon completion of the traits-based analyses, a more detailed purpose and scope will be provided.

-------------------------------

# Scope

This National Fishes Vulnerability Assessment Project provides a relative assessment of geographical and functional climate sensitivity for a subset of native freshwater fishes across the conterminous United States.

-------------------------------

# Intended Uses

The analyses herein are intended for use by wildlife managers and conservation practitioners to identify species that have high relative, intrinsic sensitivity to changes in climate. By using relative metrics that can be applied across different spatial scales, these assessments of geographical and functional sensitivity allow for direct comparisons between species with variable data availability, including species that are both well- and poorly-studied. 

-------------------------------

# References

[1] PRISM Datasets - http://www.prism.oregonstate.edu/documents/PRISM_datasets.pdf

[2] Mims, M. C., D. H. Olson, D. S. Pilliod, and J. B. Dunham. 2018. Functional and geographic components of risk for climate sensitive vertebrates in the Pacific Northwest, USA. Biological Conservation 228:183-194.

[3] FishTraits - https://www.sciencebase.gov/catalog/item/5a7c6e8ce4b00f54eb2318c0

===============================

# RCS Script Workflow

-------------------------------

Script 1: "ARC_Species Filtering Code.R"

Code to download and filter GBIF occurrence points for a list of candidate species. All fixed data required by the script is now housed through the GitHub repository at https://github.com/silknets/National-RCS. Output files are filtered occurrence points for each candidate species that passes through the filtering steps with a minimum number of occurrence points at the national scale (see code and/or manuscript for additional details). This reduced list of fishes is referred to as the 'focal species list', and is used throughout the remainder of the scripts.  

-------------------------------

Script 2: "ARC_Area of Occupancy.R"

Code to calculate area of occupancy (AOO) at the HUC12 watershed/sub-basin grain for all focal species. All fixed data required by the script is now housed through the GitHub repository at https://github.com/silknets/National-RCS. Outputs from Script 1 (Species Filtering) include all candidate species, but AOO code should only include species with > 50 filtered occurrence points plus 'exception species' to account for geographic representation in the West region. Output files include shapefiles of occupied watersheds, a CSV with AOO values for all species, and a TXT file of all occupied HUCs per year with duplicates removed. 

-------------------------------

Script 3: "ARC_PRISM Data Download.R"

This code downloads PRISM data for the 5 climate variables of interest from 1895-2017, which reflect the maximum range of stable records. Download performed using library(prism), and using the most current version (0.2.0 at time of writing) is strongly recommended. Data may also be retrieved through the PRISM website - see https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf for additional details.  

-------------------------------

Script 4: "ARC_Moving Window Calculation.R"

This script relies on input data from a ScienceBase data release (Silknetter et al.; in prep), as this data product provides PRISM climate data extracted for all HUC12 sub-basins in the conterminous U.S. This script takes the PRISM x HUC data and generates a 30-year moving window for all five climate variables used for the NFVAP. Because 29 years of prior data are required to create the moving window, the output is a .csv file of moving window climate data from 1924:2017. 

-------------------------------

Script 5: "ARC_MW Climate Sensitivity.R"

This code relies on occupied HUCs per year from Script #2 and moving window data for all climate variables from Script #4. This code builds a dataframe of climate values associated with each occupied HUC (by year). Columns of the dataframe represent the climate conditions for each climate variable. The code then calculates standard deviations (SD) of each climate variable for all focal species. Outputs include a CSV of climate data for each species, as well as a master table of all climate SDs. 

-------------------------------

Script 6: "ARC_RCS Calculation.R"

This code calculates Climate Sensitivity (CS) from standard deviation values generated in Script #5. Area of Occupancy (AOO) data from Script #2 and CS data are merged into a single RCS output for all species at the watershed grain. Finally, the Relative Climate Sensitivity index (RCS) is calculated for each species. 

===============================

# Figure Scripts - Order Does Not Matter

-------------------------------

Figure 1: "RCS Dot Plot Code.R"

Code to generate the dot plot figure for the Relative Climate Sensitivity index (RCS). Can be modified to plot multiple RCS scores for each species.

===============================

OLD READ.ME information. To be revised by SCS in Spring 2020, ignore the info below for now. 

Sam Silknetter (SCS) Updates 03/09/2019

All files from NFVAP downloaded and uploaded to SCS personal Git repository; no files from the original NFVAP repository were edited. I've added the last date the code was modified at the end of the file name ("Data"_MM-DD-YYYY)

As of 05-18-2019, all scripts are pulling from species lists with N=128. 

-------------------------------

Script-1: 'species_fishtraits_03-09-2019.R'

Minor code changes regarding the for loop after trouble-shooting with MCM. Added paste function with [sep = ""] to 'dat' object in Line 16.

write.csv() output locations edited so that new data files are stored in the working directory as opposed to 'Final location data' subfolder

CODE WORKS - any issues are almost certainly related to the file location names, spacing, quotes, etc..

-------------------------------

Script 2: 'fish trait availability_03-09-2019.R'

Updated Lines 1-2 to reflect changes in Script 1.

Lines 6-8 updated to include version of digital data for posterity. *****MUST MAKE SURE v3.0 WAS IN FACT USED FOR THIS ANALYSIS*****

Notes: Can Script 2 be merged with Script 1 above? Aside from potential working directory/file location issues (I use a single project file to avoid that headache), I'm not sure of any advantages to having more files.

-------------------------------

Script 3: 'nature serve area_03-09-2019.R'

This script may now be obsolete, as we're not interested in NatureServe AOO  (nor 5 or 20km buffer points) for the RCS paper. 

library(rgdal) added to Line 4. Function readOGR() won't work without it.

At Line 20, I added 'argument =' for function readOGR(dns=, layer=). Probably not necessary, but helpful for SCS to understand code. Also, issue with for loop resulting from working directory set to WD_NATURESERVE; calling file located elsewhere requires using full directory name. 

For loop takes a long time to run (~ 40 minutes on SCS laptop)

-------------------------------

Script 4: 'AOOvsRCS_05-18-2019.R'

This script outputs a 4-panel plot of RCS x AOO at each of our grain sizes (1 & 10km buffer points, HUC-8 & HUC-12 watersheds). Detailed annotations within the code identify areas where code will need cleaned up. For instance, .csv output is used to calculate SD in excel, then data is copied to "FinalFish.csv", which is the final species dataset for downstream analyses where N-128 species. 

-------------------------------

Script 5: 'Bubble Plot_05-18-2019.R'
Data File*: FinalFish.csv
  * FinalFish.csv is the final species summary table (N=128 spp.) with columns added      for CS (Columns F/G), AOO (H/I), RCS (J/K), and updated federal listing status       per species (Column U/V). 

This script generates a plot of all species' (N=128) RCS values with the associated SD. Ranked by RCS values in descending order. 

-------------------------------

Script 6: 'RCS x Status Boxplot_05-18-2019.R'
Data File*: FinalFish.csv

This script generates a boxplot of all species' (N=128) RCS values by status. See annotations for definitions of values

-------------------------------

Script 7: 'Trait-Based Linear Models_05-18-2019.R'

Exploratory analyses with traits. No need to go through until I have aims to guide this type of analysis. 
