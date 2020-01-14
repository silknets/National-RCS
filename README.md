# National-Fishes-Vulnerability-Assessment-Project
National Fishes Vulnerability Assessment Project

===============================

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
