# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("xlsx")){install.packages("xlsx")} ; library("xlsx")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")

#Chargement du fichier EEE

Liste_EVEE_PACA = read.table("../Liste_EVEE_PACA_paca_29022024.txt",h=T)

####### MISE A JOUR TAXREF

# Import de TAXREFv17
TAXREFv17_FLORE_FR_SYN.csv <- read.csv("../../../TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR_SYN.csv")

#Jointure
TAB_GEN_JOIN_BRUT = left_join(Liste_EVEE_PACA,TAXREFv17_FLORE_FR_SYN.csv,by=c("cd_ref"="CD_NOM"))

# VERIFICATION !!!!!!MANUELLE!!!!!!!! des taxons dont la jointure a echouee
NO_MATCH = TAB_GEN_JOIN_BRUT[is.na(TAB_GEN_JOIN_BRUT$REGNE),]

#Tables finales
TAB_EVEE_PACA = TAB_GEN_JOIN_BRUT %>% select(CD_NOM = CD_REF,famille,nom_vern_invmmed,nom_taxon_invmed,NOM_VALIDE,date_intro,origine,milieux,categorie_paca)

#Enregistrement des tables en format excel
write.xlsx(TAB_EVEE_PACA,"../TAB_EVEE_PACA.xlsx",row.names = F)
write.xlsx(NO_MATCH,"../TAB_EVEE_PACA_NOMACTH.xlsx",row.names = F)