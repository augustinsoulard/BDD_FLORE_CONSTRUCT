# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")

# Import des especes ZNIEFF
Flore_habitats_ZNIEFF_PACA = read_excel("Flore_habitats_ZNIEFF_PACA.xlsx", 
                                         col_types = c("text", "text", "text", 
                                           "text", "text", "text", "text"))

#Import de TAXREFv16
TAXREFv16_FLORE_FR_SYN.csv <- read.csv2("../../../TAXONOMIE/TAXREF/TAXREFv16_FLORE_FR_SYN.csv")
TAXREFv16_FLORE_FR_SYN.csv$CD_NOM = as.character(TAXREFv16_FLORE_FR_SYN.csv$CD_NOM)

#Jointure
F_ZNIEFF_PACA_JOIN_BRUT = left_join(Flore_habitats_ZNIEFF_PACA,TAXREFv16_FLORE_FR_SYN.csv,by=c("CD_NOM_TAXREFv5"="CD_NOM"))


# Selection des colonnes
F_ZNIEFF_PACA_JOIN = F_ZNIEFF_PACA_JOIN_BRUT %>% select(CD_NOM_TAXREFv5,NOM_CITE_TAXREFv5,CD_REF, NOM_VALIDE,FAMILLE.y)

#Export de l'excel
write.xlsx(F_ZNIEFF_PACA_JOIN,"Flore_ZNIEFF_PACA_TAXREFv16.xlsx")
