#remotes::install_github("Rekyt/rtaxref")
# WEBSITE: https://rekyt.github.io/rtaxref/


# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)


# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("rtaxref")){remotes::install_github("Rekyt/rtaxref")} ; library("rtaxref")

#Chargement de baseflor_bryo
baseflor_bryo <- read.csv("../CATMINAT/baseflor_bryo.csv",h=T)

# Ajustement des rangs taxonomiques

baseflor_bryo$CD_NOM = "0"
baseflor_bryo$NOM_VALIDE = "NULL"

#Simplification des nom scientifique en retirant les auteurs
baseflor_bryo$NOM_SIMPLE = str_split(baseflor_bryo$NOM_SCIENTIFIQUE, "[:blank:][:upper:]",n=2, simplify = TRUE)[,1]
baseflor_bryo$NOM_SIMPLE = str_split(baseflor_bryo$NOM_SIMPLE, "\\(", n = 2, simplify = TRUE)[, 1]
baseflor_bryo$NOM_SIMPLE = str_split(baseflor_bryo$NOM_SIMPLE, "[:blank:]$", n = 2, simplify = TRUE)[, 1]

#Boucle de matching
for (i in 1:nrow(baseflor_bryo)){
  cat(i,"\n")
  reference = FALSE # Remise de  reference en faux
    tryCatch({
      t = rt_taxa_search(sciname = baseflor_bryo$NOM_SIMPLE[i],version = "16.0")
      if(any(t$id != t$referenceId)){
        reference = TRUE # Garder en mémoire que la taxon de base n'est pas celui de référence
      }
      if(ncol(t)>1){
        #Supression des synonymes
        if(nrow(t[t$id==t$referenceId,])>=1){t = t[t$id==t$referenceId,]}
        #Supression des sous-espèces si nécessaire
        if(nrow(t)>=1 & nrow(t[t$rankId=="ES",])>=1){t = t[t$rankId=="ES",]}
        #Supression des hybrides
        if(str_detect(baseflor_bryo$NOM_SIMPLE[i],"[:blank:]x[:blank:]")==FALSE){
          t = t[!str_detect(t$scientificName,"[:blank:]x[:blank:]"),]
        }
        if(reference == TRUE){
          t = rt_taxa_search(id = t$referenceId[1],version = "16.0")
        }
        
        #Attribution des valeurs de CD_NOM et NOM_VALIDE
        baseflor_bryo$CD_NOM[i] = t$referenceId[1]
        baseflor_bryo$NOM_VALIDE[i] = t$scientificName[1]}else{
          baseflor_bryo$CD_NOM[i] = "NOMATCH"
          baseflor_bryo$NOM_VALIDE[i] = "NOMATCH"
        }

  }, error = function(e) {
    baseflor_bryo$CD_NOM[i] = "NOMATCH"
    baseflor_bryo$NOM_VALIDE[i] = "NOMATCH"
  })

}


# Verification des différences
difference = baseflor_bryo[baseflor_bryo$NOM_VALIDE!=baseflor_bryo$NOM_SIMPLE,]
view(difference)

######## Prise en compte du correctif de baseflore_bryo
# Chargement du correctif
Correctif_baseflor_bryo <- read_excel("Correctif_baseflor_bryo.xlsx")

#Boucle de correction
for(i in 1:nrow(Correctif_baseflor_bryo)){
  if(nrow(baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i],])>=1){
    #remplacement des colonnes pour els epsèces concernées
    baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i],]$floraison = Correctif_baseflor_bryo$floraison[i]
    baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i],]$CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL. =
      Correctif_baseflor_bryo$CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.[i]
    baseflor_bryo[baseflor_bryo$CD_NOM == Correctif_baseflor_bryo$CD_NOM[i],]$INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE = 
      Correctif_baseflor_bryo$INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE[i]
##############COMPLETERRRRRRR
  }
  
}



# Enregistrer le fichier final
write.csv2(baseflor_bryo,"baseflor_bryoTAXREFv16.csv",row.names = F,fileEncoding = "UTF-8",na="")

