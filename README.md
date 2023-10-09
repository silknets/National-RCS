# National-Fishes-Vulnerability-Assessment-Project
National Fishes Vulnerability Assessment Project

===============================

Sam Silknetter (SCS) Updates: 05 September 2023

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

All files saved at the GitHub repository https://github.com/silknets/National-RCS in the subfolder "Final Scripts - GitHub"
-------------------------------

Script 1: "ARC_Species Filtering Code.R"

Code to download and filter GBIF occurrence points for a list of candidate species. All fixed data required by the script is now housed through the GitHub repository at https://github.com/silknets/National-RCS. Output files are filtered occurrence points for each candidate species that passes through the filtering steps with a minimum number of occurrence points at the national scale (see code and/or manuscript for additional details). This reduced list of fishes is referred to as the 'focal species list', and is used throughout the remainder of the scripts.  

-------------------------------

Script 2: "ARC_Area of Occupancy.R"

Code to calculate area of occupancy (AOO) at the HUC12 watershed/sub-basin grain for all focal species. All fixed data required by the script is now housed through the GitHub repository at https://github.com/silknets/National-RCS. Outputs from Script 1 (Species Filtering) include all candidate species, but AOO code should only include species with > 50 filtered occurrence points plus 'exception species' to account for geographic representation in the West region. Output files include shapefiles of occupied watersheds, a CSV with AOO values for all species, and a TXT file of all occupied HUCs per year with duplicates removed. 

Regional Scripts: Script 2 above is for all species at the national scale. Additional script files were used for each of four regions (East, Gulf, Mississippi, West) and perform the same functions as Script 2 but for a subset of the species occurrences clipped to the regional study extent. Script Names are: "ARC_East_Area of Occupancy Code.R", "ARC_Gulf_Area of Occupancy Code.R", "ARC_Miss_Area of Occupancy Code.R", "ARC_West_Area of Occupancy Code.R"

-------------------------------

Script 3: "ARC_PRISM Data Download.R"

This code downloads PRISM data for the 5 climate variables of interest from 1895-2017, which reflect the maximum range of stable records. Download performed using library(prism), and using the most current version (0.2.0 at time of writing) is strongly recommended. Data may also be retrieved through the PRISM website - see https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf for additional details.  

-------------------------------

Script 4: "ARC_Moving Window Calculation.R"

This script relies on input data from a ScienceBase data release (Silknetter et al.; in prep), as this data product provides PRISM climate data extracted for all HUC12 sub-basins in the conterminous U.S. This script takes the PRISM x HUC data and generates a 30-year moving window for all five climate variables used for the NFVAP. Because 29 years of prior data are required to create the moving window, the output is a .csv file of moving window climate data from 1924:2017. 

-------------------------------

Script 5: "ARC_MW Climate Sensitivity.R"

This code relies on occupied HUCs per year from Script #2 and moving window data for all climate variables from Script #4. This code builds a dataframe of climate values associated with each occupied HUC (by year). Columns of the dataframe represent the climate conditions for each climate variable. The code then calculates standard deviations (SD) of each climate variable for all focal species. Outputs include a CSV of climate data for each species, as well as a master table of all climate SDs.

Regional Script: Script 5 above is for all species at the national scale. An additional script file was used for the four regions (East, Gulf, Mississippi, West), which performs the same functions as Script 5 but for a subset of the species occurrences clipped to the regional study extent. Script Name is: "ARC_Regional_MW Climate Sensitivity.R"

-------------------------------

Script 6: "ARC_RCS Calculation.R"

This code calculates Climate Sensitivity (CS) from standard deviation values generated in Script #5. Area of Occupancy (AOO) data from Script #2 and CS data are merged into a single RCS output at the national scale for all species at the watershed grain. Finally, the Relative Climate Sensitivity index (RCS) is calculated for each species. This process is then repeated for each of the four regions (East, Gulf, Mississippi, West), and a final RCS table is generated with these RCS scores bound together. Lastly, trait values are appended to this spreadsheet for all focal species to be used in downstream analyses. 

-------------------------------

Script 7: R Markdown - "Manuscript Figure and Table Scripts.Rmd"

Code to generate analysis-driven tables and figures. Six standalone scripts are embedded in a Markdown file, which are used for generating Tables 2-4 and Figures 3-6, as well as Table S6 and Figure S1; see annotations for additional details. 
