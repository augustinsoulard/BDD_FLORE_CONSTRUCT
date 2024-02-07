# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Charger les bibliotheques necessaires
if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")
if(!require("rtaxref")){remotes::install_github("Rekyt/rtaxref")} ; library("rtaxref")

#Chargement des données
EEE_Auvergne = read_excel("../EEE_Auvergne.xlsx")
EEE_Rhone_Alpes = read_excel("../EEE_Rhone-Alpes.xlsx")

####################################################################
####################################################################
#########################################################AUVERGNE
####################################################################
####################################################################
# Ajustement des rangs taxonomiques

EEE_Auvergne$CD_NOM = "0"
EEE_Auvergne$NOM_VALIDE = "NULL"

#Simplification des nom scientifique en retirant les auteurs
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SCIEN, "[:blank:][:upper:]",n=2, simplify = TRUE)[,1]
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SIMPLE, "\\(", n = 2, simplify = TRUE)[, 1]
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SIMPLE, "[:blank:]$", n = 2, simplify = TRUE)[, 1]

#Boucle de matching
for (i in 1:nrow(EEE_Auvergne)){
  cat(i," : ",EEE_Auvergne$NOM_SIMPLE[i],"\n")
  tryCatch({
    t = rt_taxa_search(sciname = EEE_Auvergne$NOM_SIMPLE[i],version = "16.0")
    if(ncol(t)>1){
      #Supression des synonymes
      if(nrow(t[t$id==t$referenceId,])>=1){t = t[t$id==t$referenceId,]}
      #Supression des sous-espèces si nécessaire
      if(nrow(t)>=1 & nrow(t[t$rankId=="ES",])>=1){t = t[t$rankId=="ES",]}
      #Supression des hybrides
      if(str_detect(EEE_Auvergne$NOM_SIMPLE[i],"[:blank:]x[:blank:]")==FALSE){
        t = t[!str_detect(t$scientificName,"[:blank:]x[:blank:]"),]
      }
      #Attribution des valeurs de CD_NOM et NOM_VALIDE
      EEE_Auvergne$CD_NOM[i] = t$referenceId[1]
      EEE_Auvergne$NOM_VALIDE[i] = t$scientificName[1]}else{
        EEE_Auvergne$CD_NOM[i] = "NOMATCH"
        EEE_Auvergne$NOM_VALIDE[i] = "NOMATCH"
      }
    
  }, error = function(e) {
    EEE_Auvergne$CD_NOM[i] = "NOMATCH"
    EEE_Auvergne$NOM_VALIDE[i] = "NOMATCH"
  })
  
}

# Verification des différences
difference = EEE_Auvergne[EEE_Auvergne$NOM_VALIDE!=EEE_Auvergne$NOM_SIMPLE,]

# Ajout des taxons inferieurs
TAXREFv17_FLORE_FR = read.csv("../../../TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR.csv")
TAXREFv17_FLORE_FR$CD_SUP = as.character(TAXREFv17_FLORE_FR$CD_SUP)
TAXREFv17_FLORE_FR$CD_NOM = as.character(TAXREFv17_FLORE_FR$CD_NOM)

CD_SUP_MATCH = list()
CD_SUP_MATCH[[1]] = TAXREFv17_FLORE_FR %>% filter(CD_SUP %in% EEE_Auvergne$CD_NOM)
CD_SUP_MATCH[[1]] = left_join(CD_SUP_MATCH[[1]],EEE_Auvergne,by=c("CD_SUP"="CD_NOM"))
for(i in 2:100){
  CD_SUP_MATCH[[i]] = TAXREFv17_FLORE_FR[TAXREFv17_FLORE_FR$CD_SUP %in% CD_SUP_MATCH[[i-1]]$CD_NOM,]
  CD_SUP_MATCH[[i]] = left_join(CD_SUP_MATCH[[i]],EEE_Auvergne,by=c("CD_SUP"="CD_NOM"))
  CD_SUP_MATCH[[i]]$LAVERGNE = left_join(CD_SUP_MATCH[[i]],CD_SUP_MATCH[[i-1]],by=c("CD_SUP"="CD_NOM"))$LAVERGNE.y
  
  if(nrow(CD_SUP_MATCH[[i]])==0){
    break;
  }
}
EEE_CD_SUP = do.call(rbind,CD_SUP_MATCH)

#Creation du tableau final avec cotation de Lavergne
EEE_CD_SUP = EEE_CD_SUP %>% select(CD_NOM = CD_REF,NOM_VALIDE = NOM_VALIDE.x,NOM_VERN = NOM_VALIDE.x,LAVERGNE)
EEE_Auvergne_SELECT = EEE_Auvergne %>% select(CD_NOM,NOM_VALIDE,NOM_VERN,LAVERGNE)
EEE_Auvergne_SELECT = rbind(EEE_Auvergne_SELECT,EEE_CD_SUP)

#Sauvegarde du document final Auvergne
write.csv(EEE_Auvergne_SELECT,"EEE_Auvergne.csv",row.names = F,fileEncoding = "UTF-8",na="")

####################################################################
####################################################################
#########################################################RHONE-ALPES
####################################################################
####################################################################

# Ajustement des rangs taxonomiques

EEE_Rhone_Alpes$CD_NOM = "0"
EEE_Rhone_Alpes$NOM_VALIDE = "NULL"

#Simplification des nom scientifique en retirant les auteurs
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SCIEN, "[:blank:][:upper:]",n=2, simplify = TRUE)[,1]
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SIMPLE, "\\(", n = 2, simplify = TRUE)[, 1]
EEE_Auvergne$NOM_SIMPLE = str_split(EEE_Auvergne$NOM_SIMPLE, "[:blank:]$", n = 2, simplify = TRUE)[, 1]

#Boucle de matching
for (i in 1:nrow(EEE_Auvergne)){
  cat(i," : ",EEE_Auvergne$NOM_SIMPLE[i],"\n")
  tryCatch({
    t = rt_taxa_search(sciname = EEE_Auvergne$NOM_SIMPLE[i],version = "16.0")
    if(ncol(t)>1){
      #Supression des synonymes
      if(nrow(t[t$id==t$referenceId,])>=1){t = t[t$id==t$referenceId,]}
      #Supression des sous-espèces si nécessaire
      if(nrow(t)>=1 & nrow(t[t$rankId=="ES",])>=1){t = t[t$rankId=="ES",]}
      #Supression des hybrides
      if(str_detect(EEE_Auvergne$NOM_SIMPLE[i],"[:blank:]x[:blank:]")==FALSE){
        t = t[!str_detect(t$scientificName,"[:blank:]x[:blank:]"),]
      }
      #Attribution des valeurs de CD_NOM et NOM_VALIDE
      EEE_Auvergne$CD_NOM[i] = t$referenceId[1]
      EEE_Auvergne$NOM_VALIDE[i] = t$scientificName[1]}else{
        EEE_Auvergne$CD_NOM[i] = "NOMATCH"
        EEE_Auvergne$NOM_VALIDE[i] = "NOMATCH"
      }
    
  }, error = function(e) {
    EEE_Auvergne$CD_NOM[i] = "NOMATCH"
    EEE_Auvergne$NOM_VALIDE[i] = "NOMATCH"
  })
  
}

# Verification des différences
difference = EEE_Auvergne[EEE_Auvergne$NOM_VALIDE!=EEE_Auvergne$NOM_SIMPLE,]

# Ajout des taxons inferieurs
TAXREFv17_FLORE_FR = read.csv("../../../TAXONOMIE/TAXREF/TAXREFv17_FLORE_FR.csv")
TAXREFv17_FLORE_FR$CD_SUP = as.character(TAXREFv17_FLORE_FR$CD_SUP)
TAXREFv17_FLORE_FR$CD_NOM = as.character(TAXREFv17_FLORE_FR$CD_NOM)

CD_SUP_MATCH = list()
CD_SUP_MATCH[[1]] = TAXREFv17_FLORE_FR %>% filter(CD_SUP %in% EEE_Auvergne$CD_NOM)
CD_SUP_MATCH[[1]] = left_join(CD_SUP_MATCH[[1]],EEE_Auvergne,by=c("CD_SUP"="CD_NOM"))
for(i in 2:100){
  CD_SUP_MATCH[[i]] = TAXREFv17_FLORE_FR[TAXREFv17_FLORE_FR$CD_SUP %in% CD_SUP_MATCH[[i-1]]$CD_NOM,]
  CD_SUP_MATCH[[i]] = left_join(CD_SUP_MATCH[[i]],EEE_Auvergne,by=c("CD_SUP"="CD_NOM"))
  CD_SUP_MATCH[[i]]$LAVERGNE = left_join(CD_SUP_MATCH[[i]],CD_SUP_MATCH[[i-1]],by=c("CD_SUP"="CD_NOM"))$LAVERGNE.y
  
  if(nrow(CD_SUP_MATCH[[i]])==0){
    break;
  }
}
EEE_CD_SUP = do.call(rbind,CD_SUP_MATCH)

#Creation du tableau final avec cotation de Lavergne
EEE_CD_SUP = EEE_CD_SUP %>% select(CD_NOM = CD_REF,NOM_VALIDE = NOM_VALIDE.x,NOM_VERN = NOM_VALIDE.x,LAVERGNE)
EEE_Auvergne_SELECT = EEE_Auvergne %>% select(CD_NOM,NOM_VALIDE,NOM_VERN,LAVERGNE)
EEE_Auvergne_SELECT = rbind(EEE_Auvergne_SELECT,EEE_CD_SUP)

#Sauvegarde du document final Auvergne
write.csv(EEE_Auvergne_SELECT,"EEE_Auvergne.csv",row.names = F,fileEncoding = "UTF-8",na="")
