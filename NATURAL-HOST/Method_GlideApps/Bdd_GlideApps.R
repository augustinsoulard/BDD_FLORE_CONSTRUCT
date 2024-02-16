# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("reshape2")){install.packages("reshape2")} ; library("reshape2")


#Chargement des fichiers

BDD_FICHE_FLORE <- read_excel("../../BDD_FICHE_FLORE/BDD_FICHE_FLORE.xlsx",
                              sheet = "IMG")
TAB_GEN_FULL_JOIN <- read.csv("../../../ENJEU_FLORE/ENJEU_PACA_AUGUSTIN_SOULARD/workflow/TAB_GEN_FULL_JOIN.csv")

baseflor_bryoTAXREFv16 <- read.csv("../../TAXONOMIE/TAXREF-MATCH-BASEFLOR/baseflor_bryoTAXREFv16.csv", sep=";")

#Transformation des donnees brutes
#BDD_FICHE_FLORE
BDD_FICHE_FLORE = BDD_FICHE_FLORE %>% filter(!is.na(PATH_IMG))

BDD_FICHE_FLORE$TEXTE_LEGEND_IMG = paste0(BDD_FICHE_FLORE$AUTEUR," - ",
                                          BDD_FICHE_FLORE$LICENCE)


#baseflor_bryoTAXREFv16

correspondances <- c("1" = "janvier", "2" = "février", "3" = "mars", 
                                           "4" = "avril", "5" = "mai", "6" = "juin", 
                                           "7" = "juillet", "8" = "août", "9" = "septembre", 
                                           "10" = "octobre", "11" = "novembre", "12" = "décembre")
baseflor_bryoTAXREFv16$floraison <- str_replace_all(baseflor_bryoTAXREFv16$floraison, correspondances)

# Gestion des duplicats
BDD_FICHE_FLORE_reformed <- BDD_FICHE_FLORE %>%
  group_by(CD_NOM) %>%
  mutate(
    # Pour une deuxième image
    PATH_IMG_2 = ifelse(n() == 2, lead(PATH_IMG), NA),
    TEXTE_LEGEND_IMG_2 = ifelse(n() == 2, lead(TEXTE_LEGEND_IMG), NA),
  
    # Pour une troisième image
    PATH_IMG_3 = ifelse(n() == 3, lead(PATH_IMG,2), NA),
    TEXTE_LEGEND_IMG_3 = ifelse(n() == 3, lead(TEXTE_LEGEND_IMG,2), NA)
  )

BDD_FICHE_FLORE_unique = BDD_FICHE_FLORE_reformed[!duplicated(BDD_FICHE_FLORE_reformed$CD_NOM),]

#Jointure des autres données
#Préparation colonne de jointure
BDD_FICHE_FLORE_unique$CD_NOM = as.character(BDD_FICHE_FLORE_unique$CD_NOM)
TAB_GEN_FULL_JOIN$CD_NOM = as.character(TAB_GEN_FULL_JOIN$CD_NOM)
baseflor_bryoTAXREFv16$CD_NOM = as.character(baseflor_bryoTAXREFv16$CD_NOM)
#Jointure
BDD_FICHE_FLORE_join_T = left_join(BDD_FICHE_FLORE_unique,TAB_GEN_FULL_JOIN,by="CD_NOM")
BDD_FICHE_FLORE_join_TB = left_join(BDD_FICHE_FLORE_join_T,baseflor_bryoTAXREFv16,by="CD_NOM")

#Remise en valeur unique
BDD_FICHE_FLORE_join_TB = BDD_FICHE_FLORE_join_TB[!duplicated(BDD_FICHE_FLORE_join_TB$CD_NOM),]

#Selection des colonnes
BDD_FICHE_FLORE_join_TB = BDD_FICHE_FLORE_join_TB %>% select(
  CD_NOM,
  ESPECE,
  NOM_VERN = NOM_VERN.x,
  FAMILLE,
  PATH_IMG,
  TEXTE_LEGEND_IMG,
  PATH_IMG_2,
  TEXTE_LEGEND_IMG_2,
  PATH_IMG_3,
  TEXTE_LEGEND_IMG_3,
  INDIGENAT,
  EVEE,
  DH,
  LRN,
  LRR,
  ZNIEFF,
  ENJEU_CBN,
  Indicatrice.ZH,
  CARACTERISATION_ECOLOGIQUE = CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.,
  INDICATION_PHYTOSOCIOLOGIQUE = INDICATION_PHYTOSOCIOLOGIQUE_CARACTERISTIQUE,
  CHOROLOGIE,
  floraison,
  inflorescence,
  sexualité,
  ordre_maturation,	
  pollinisation,
  fruit,	
  dissémination,	
  couleur_fleur,	
  macule,
  Lumière,	Température,	
  Continentalité,	
  Humidité_atmosphérique,
  Humidité_édaphique,	
  Réaction_du_sol_.pH.,
  Niveau_trophique,	
  Salinité,
  Texture,
  Matière_organique
)

#Enregistrement du tableau a integrer à GlideApp
write.csv(BDD_FICHE_FLORE_join_TB,file = "BDD_Flore_patri_PACA.csv",row.names = F,fileEncoding = "UTF-8",na="")


