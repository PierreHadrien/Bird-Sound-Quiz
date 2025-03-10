library(suwo)
library(openxlsx2)

###1.filter the place and taxa type
#upload infos on species present in a given country for a given species
metadata_test <- suwo::query_xenocanto("cnt:Belgium grp:birds", all_data = getOption("all_data",FALSE))

df<-metadata_test

# Column name to filter
column_name <- "location"

#liste des régions de la métropole fr si jamais on a besoin de ce filtre
# pattern_to_filter <- c(
#   "Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté", "Bretagne", "Centre-Val de Loire",
#   "Corse|Corsica", "Grand Est", "Hauts-de-France", "Île-de-France|Ile-de-France",
#   "Normandie|Normandy", "Nouvelle-Aquitaine", "Occitanie", "Pays de la Loire",
#   "Provence-Alpes-Côte d'Azur|Provence-Alpes-Cote d'Azur"
# )

#Filter with this line a precise region/locality
pattern_to_filter <- c("Hauts-de-France","Nord-Pas-de-Calais")

# Create a combined regex pattern
combined_pattern <- paste(pattern_to_filter, collapse = "|")

# Create a logical vector to indicate rows to keep (metropolitan regions)
keep_rows <- grepl(combined_pattern, df[[column_name]], ignore.case = TRUE)

# Subset the data frame
df_filtered <- df[keep_rows, ]

###2.keep one row for each species after filtering the place
df_unique <- df_filtered[!duplicated(df_filtered$species), ]

###3.Other necessary modifications
##Supression des lignes aberrantes avec vulpes vulpes et mystery mystery dans la col species
# Column name to filter
column_name <- "species"

# Patterns to exclude
exclude_patterns <- c("Mystery_mystery", "Vulpes_vulpes")

# Create a logical vector to indicate rows to keep
keep_rows <- !df_unique[[column_name]] %in% exclude_patterns

# Subset the data frame
df_unique <- df_unique[keep_rows, ]

#Keep only the species col in the df
list_species <- data.frame(df_unique["species"])

#replace "_" by " " in names
library(stringr)
list_species$species <- str_replace_all(list_species$species, "_", " ")

###4. This part is made for addings of species common name but IT IS ADPTED TO FRENCH LANGUAGE !! be careful to replace
###   it with a file with your of your language
#open taxref and merge the french names
taxref<-openxlsx2::read_xlsx("Taxref_birds.xlsx")
list_species<- merge(list_species, taxref, by.x = "species",by.y = "LB_NOM", all.x = TRUE)
list_species<- list_species[!is.na(list_species$NOM_VERN), ]
list_species<- list_species[,c("species","NOM_VERN") ]
colnames(list_species)[colnames(list_species) == "NOM_VERN"] <- "species2"
rm(taxref)

###5.writing of the final file as xlsx
#Création du fichier xlsx avec liste d'sp
library(openxlsx2)
openxlsx2::write_xlsx(list_species,"list_sp_Hauts-de-France_xc.xlsx")
# openxlsx2::write_xlsx(list_species_fr,"list_sp_fr_metrop_xc.xlsx")
