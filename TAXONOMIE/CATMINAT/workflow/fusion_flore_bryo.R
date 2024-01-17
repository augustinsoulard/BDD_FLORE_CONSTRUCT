# remotes::install_github("Rekyt/rtaxref")
# WEBSITE: https://rekyt.github.io/rtaxref/


# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)


# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")

#Chargement de baseflor
baseflor <- read_excel("../baseflor.xlsx")

#Chargement de basebryo
basebryo <- read_excel("../basebryo.xlsx")

#Ajustement des noms de colonnes de basebryo
colnames(basebryo)[3] = "code_CATMINAT"
colnames(basebryo)[4] = "NOM_SCIENTIFIQUE"
colnames(basebryo)[5] = "INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE"
colnames(basebryo)[6] = "CARACTERISATION_ECOLOGIQUE_(HABITAT_OPTIMAL)"
colnames(basebryo)[7] = "INDICATION_DIFFERENTIELLE_1"
colnames(basebryo)[8] = "INDICATION_DIFFERENTIELLE_2"
colnames(basebryo)[10] = "Lichtzahl"
colnames(basebryo)[11] = "Temperaturzahl"
colnames(basebryo)[12] = "Kontinentalitat"
colnames(basebryo)[13] = "Feuchtzahl"
colnames(basebryo)[14] = "Reaktionszahl"
colnames(basebryo)[15] = "TYPE_BIOLOGIQUE"
basebryo["...16"] = NULL

#Ajustement des noms de colonnes de baseflor
colnames(baseflor)[38] = "Kontinentalitat"

#Fusion des 2 tables
common_columns <- intersect(names(baseflor), names(basebryo))
baseflor_bryo = merge(baseflor,basebryo,by=common_columns,all=T)

#Export en csv
write.csv(baseflor_bryo,file = "../baseflor_bryo.csv",row.names = F,fileEncoding = "UTF-8",na="")

