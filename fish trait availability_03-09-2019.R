#Input filtered trait data for the final species list (output from Line 77 of script 'species_fishtraits_03-09-2019.R')  
#Provides a summary of the % of traits available for each species, and the the % of species represented by each trait

fishtrait <- read.csv("specie_fishtraits_filtered.csv", header=TRUE)

#trait count starts at native to the end, 
row_missing <- as.data.frame(rowSums(!is.na(fishtrait)))
row_missing[2] <- fishtrait$SPECIES
row_missing <- row_missing[c(2,1)]
row_missing[3] <- (row_missing$`rowSums(!is.na(fishtrait))`/length(colnames(fishtrait))) * 100
row_missing[3] <- round(row_missing$V3, digits = 2)
names(row_missing) <- c("Species", "Total Traits Available", "Percentage Traits Available")
write.csv(row_missing, file = paste0("traits available row wise.csv"), row.names=FALSE) #output the file as csv


column_missing <-sapply(fishtrait, function(y) sum(length(which(is.na(y)))))
column_missing <- data.frame(column_missing)
column_missing[2] <- round(((column_missing$column_missing / length(colnames(fishtrait))) * 100), digits = 2)
column_missing[3] <- 100 - column_missing$V2
names(column_missing) <- c("NA Count", "% Missing", "% Available")
write.csv(column_missing, file = paste0("traits available column wise.csv"), row.names=TRUE) #output the file as csv

traitnames <- as.data.frame(colnames(fishtrait))
write.csv(traitnames, file = paste0("trait name.csv"), row.names=FALSE) #output the file as csv
