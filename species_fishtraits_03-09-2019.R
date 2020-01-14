rm(list=ls())


#takes fishtraits from the FishTraits database (directly obtained from Emmanuel Frimpong at Virginia Tech)
#and extracts data for the species represented in the final species list (n=128)

setwd("G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP")

fishtraits <- read.csv("FishTraitsSpeciesSelection.csv", header = TRUE) #this is the raw trait data

FILES <- list.files(path="G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/Final location data/", pattern = ".csv") #create file list for loop

species_fishtraits = NULL

for (i in 1:length(FILES)){
  dat <- read.csv(paste("G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/Final location data/", FILES[i], sep = ""), header=TRUE)
  species <- as.character(dat[2,10])
  species_split <- strsplit(species, " ")
  species_split <- as.data.frame(unlist(species_split))
  a_species = as.character(species_split[2,1])
  b_genus = as.character(species_split[1,1])
  extraction <- fishtraits[which(fishtraits$SPECIES == a_species & fishtraits$GENUS == b_genus),]
  species_fishtraits[[i]] <- extraction
  
}

species_fishtraits1 <- do.call(rbind.data.frame, species_fishtraits)


a <- "Phoxinus"
b <- "erythrogaster"

extraction <- fishtraits[which(fishtraits$SPECIES == b & fishtraits$GENUS == a),]

species_fishtraits1[120,] <- extraction

a <- "Lampetra"
b <- "tridentata"

extraction <- fishtraits[which(fishtraits$SPECIES == b & fishtraits$GENUS == a),]

species_fishtraits1[121,] <- extraction

a <- "Cycleptus"
b <- "elongatus"

extraction <- fishtraits[which(fishtraits$SPECIES == b & fishtraits$GENUS == a),]

species_fishtraits1[122,] <- extraction

a <- "Moxostoma"
b <- "duquesnei"

extraction <- fishtraits[which(fishtraits$SPECIES == b & fishtraits$GENUS == a),]

species_fishtraits1[123,] <- extraction



write.csv(species_fishtraits1, file = "G:/My Drive/Virginia Tech/Fish Traits/Git Data - NFVAP/NFVAP/species_fishtraits.csv", row.names = FALSE)  #save the dataframe


#subsetting the first traits

specie_fishtraits <- read.csv("species_fishtraits.csv", header = TRUE) #must have file in directory's main folder
attach(specie_fishtraits)
specie_fishtraits_filtered <- subset(specie_fishtraits, select=-c(FamilyNumber, JAN:DEC, REPSTATE, EURYHALINE:LATCENTROID, 
                                                         AREAKM2:LONRANGE, LIST1:EXTINCT))

write.csv(specie_fishtraits_filtered, file = "specie_fishtraits_filtered.csv", row.names = FALSE)  #save the dataframe


fishtraits_filtered <- subset(fishtraits, select=-c(FamilyNumber, JAN:DEC, REPSTATE, EURYHALINE:LATCENTROID, 
                                                           AREAKM2:LONRANGE, LIST1:EXTINCT))


write.csv(fishtraits_filtered, file = "fishtraits_filtered.csv", row.names = FALSE)  #save the dataframe
