# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")



#Chargement des bases de données
Method_enjeu_PACAv2_0 <- read_excel("../../../ENJEU_FLORE/ENJEU_PACA_AUGUSTIN_SOULARD/Method_enjeu_PACAv2.0.xlsx", 
                                    sheet = "Tableau_general")
baseflor_bryoTAXREFv16 <- read.csv("../../TAXONOMIE/TAXREF-MATCH-BASEFLOR/baseflor_bryoTAXREFv16.csv", sep=";")

#filtre des bases
baseflor_bryoTAXREFv16 = baseflor_bryoTAXREFv16 %>% select(CD_NOM,CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.,floraison)
Method_enjeu_PACAv2_0 = Method_enjeu_PACAv2_0 %>% select(CD_NOM,NOM_VALIDE,NOM_VERN,INTERET_PACA,PROTECTION_PACA)

#Jointure des bases de données
Method_enjeu_PACAv2_0$CD_NOM = as.character(Method_enjeu_PACAv2_0$CD_NOM)
ENJEU_BASEFLOR = left_join(Method_enjeu_PACAv2_0 ,baseflor_bryoTAXREFv16,by="CD_NOM")

#gestion des NA et des vides dans la floraison
ENJEU_BASEFLOR$changefloraison <- ifelse(is.na(ENJEU_BASEFLOR$floraison)| ENJEU_BASEFLOR$floraison == "", "ajout floraison (1-12)", "")
ENJEU_BASEFLOR[is.na(ENJEU_BASEFLOR$floraison)| ENJEU_BASEFLOR$floraison == "",]$floraison = "1-12"
ENJEU_BASEFLOR = unique(ENJEU_BASEFLOR)

#gestion des valeurs uniques
for(i in 1 :nrow(ENJEU_BASEFLOR)){
  cat(i,"\n")
  if(nchar(ENJEU_BASEFLOR$floraison[i])==1){
    ENJEU_BASEFLOR$floraison[i] = paste0(ENJEU_BASEFLOR$floraison[i],"-",ENJEU_BASEFLOR$floraison[i])
  }
}


convertir_periode <- function(periode) {
  mois <- rep(FALSE, 12)
  periodes <- strsplit(periode, "-")[[1]]
  if (length(periodes) == 2) {
    debut <- as.integer(periodes[1])
    fin <- as.integer(periodes[2])
    if (!is.na(debut) & !is.na(fin)) {
      if (debut <= fin) {
        mois[debut:fin] <- TRUE
      } else {
        mois[1:fin] <- TRUE
        mois[debut:12] <- TRUE
      }
    }
  }
  return(mois)
}

# Appliquer la fonction convertir_periode à chaque ligne du dataframe
mois_floraison <- t(sapply(ENJEU_BASEFLOR$floraison, convertir_periode))

# Créer un dataframe résultat avec les mois
ENJEU_BASEFLOR_FLORAISON <- cbind(ENJEU_BASEFLOR, mois_floraison)

# Renommer les colonnes mois
colnames(ENJEU_BASEFLOR_FLORAISON)[9:20] <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")

# Remplacer TRUE par "X"
ENJEU_BASEFLOR_FLORAISON[ENJEU_BASEFLOR_FLORAISON == FALSE] <- ""
ENJEU_BASEFLOR_FLORAISON[ENJEU_BASEFLOR_FLORAISON == TRUE] <- "X"

# filtrer les espèces à enjeux
ENJEU_MODERE_BASEFLOR_FLORAISON = ENJEU_BASEFLOR_FLORAISON %>% filter(PROTECTION_PACA != "-" | 
                                      INTERET_PACA == "MODERE" |
                                      INTERET_PACA == "FORT"|
                                    INTERET_PACA == "TRES FORT" |
                                      INTERET_PACA == "MAJEUR")

#Enregistrer le résultats
write.csv(ENJEU_MODERE_BASEFLOR_FLORAISON,file = "ENJEU_MODERE_BASEFLOR_FLORAISON.csv",row.names = F,fileEncoding = "UTF-8",na="")
