# Installer et charger les bibliothèques nécessaires
if(!require("fs")){install.packages("fs")} ; library("fs")
if(!require("later")){install.packages("later")} ; library("later")
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("remotes")){install.packages("remotes")} ; library("remotes")
if(!require("rtaxref")){install_github("Rekyt/rtaxref")} ; library("rtaxref")

# Définir le répertoire de fichiers
WD <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Fonction de traitement principal
process_data <- function() {
  # Charger baseflor_bryo
  baseflor_bryo <- read.csv2("baseflor_bryoTAXREFv16.csv", h = TRUE)
  # Chargement du correctif
  Correctif_baseflor_bryo <- read_excel("Correctif_baseflor_bryo.xlsx")
  Correctif_baseflor_bryo$CD_NOM = as.character(Correctif_baseflor_bryo$CD_NOM)
  # Boucle de correction
  for (i in 1:nrow(Correctif_baseflor_bryo)) {
    if (nrow(baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i], ]) >= 1) {
      baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i], ]$floraison <- Correctif_baseflor_bryo$floraison[i]
      baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i], ]$CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL. <- Correctif_baseflor_bryo$CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.[i]
      baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i], ]$INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE <- Correctif_baseflor_bryo$INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE[i]
    }
    if(nrow(baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i], ]) == 0){
     #Création du tableau vide
      baseflor_bryo = bind_rows(baseflor_bryo,Correctif_baseflor_bryo[i,])
    }
  }
  # Enregistrer le fichier final
  write.csv2(baseflor_bryo, "baseflor_bryoTAXREFv16.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

# Fonction pour surveiller les modifications du fichier Correctif_baseflor_bryo.xlsx
watch_file <- function(file, callback, interval = 10) {
  filepath = paste0("D:/Github/BDD_FLORE_CONSTRUCT/TAXONOMIE/TAXREF-MATCH-BASEFLOR/",file)
  last_mtime <- file_info(filepath)$modification_time
  repeat {
    Sys.sleep(interval)
    new_mtime <- file_info(filepath)$modification_time
    if (new_mtime != last_mtime) {
      callback()
      last_mtime <- new_mtime
    }
  }
}

# Exécuter le processus de surveillance
watch_file("Correctif_baseflor_bryo.xlsx", process_data)
