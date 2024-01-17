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
  t = rt_taxa_search(sciname = baseflor_bryo$NOM_SIMPLE[i],version = "16.0")
  if(ncol(t)>1){
    if(nrow(t[t$id==t$referenceId,])>1){t = t[t$id==t$referenceId,]}
    if(nrow(t)>1 & nrow(t[t$rankId=="ES",])>1){t = t[t$rankId=="ES",]}
    baseflor_bryo$CD_NOM[i] = t$referenceId[1]
    baseflor_bryo$NOM_VALIDE[i] = t$scientificName[1]}else{
      baseflor_bryo$CD_NOM[i] = "NOMATCH"
      baseflor_bryo$NOM_VALIDE[i] = "NOMATCH"
    }
}
write.csv2(baseflor_bryo,"baseflor_bryoTAXREFv17.csv",row.names = F,fileEncoding = "UTF-8",na="")



# Installer et charger le package stringr
install.packages("stringr")
library(stringr)

# Exemple de dataframe
df <- data.frame(Nom = c("Jean Dupont", "Marie-Louise Johnson", "Robert (Bob) Smith"))

# Séparer avant la deuxième majuscule
df = str_split(df$Nom, "[:blank:][:upper:]",n=2, simplify = TRUE)[,1]

# Séparer avant la parenthèse
df = str_split(df$Nom, "\\(", n = 2, simplify = TRUE)[, 1]

# Afficher le résultat
print(df)


