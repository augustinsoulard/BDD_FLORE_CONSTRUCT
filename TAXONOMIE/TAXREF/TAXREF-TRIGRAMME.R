# Definition du repertoire de fichiers
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)


# Chargement des packages
if (!require("do")) {install.packages("do")}+library("do")#For use left function
if (!require("tidyverse")) {install.packages("tidyverse")}+library("tidyverse")


#Chargement de TAXREF
TAXREF <- read.delim("TAXREFv18.txt", encoding="UTF-8")
# TAXREF <- read.delim("C:/Users/MTDA-029/Downloads/TAXREFv18.txt", encoding="UTF-8")



TAXREF_FLORE_FR = TAXREF[TAXREF$REGNE == "Plantae" & TAXREF$FR!="" & TAXREF$CD_REF == TAXREF$CD_NOM,]
TAXREF_FLORE_FR_SYN = TAXREF[TAXREFv17$REGNE == "Plantae" & TAXREF$FR!="",]

TAXREF = TAXREF_FLORE_FR
# TAXREF = TAXREF[duplicated(TAXREF$CD_REF)==FALSE,] #Delete duplicated taxa
# row.names(TAXREF)= c(1:nrow(TAXREF))


TAXREF$ESPBRUT = paste0(left(TAXREF$NOM_VALIDE,3),
                        substr(TAXREF$NOM_VALIDE, str_locate(TAXREF$NOM_VALIDE," ")+1,
                               str_locate(TAXREF$NOM_VALIDE," ")+3))

TAXREF$subsp = str_detect(TAXREF$NOM_VALIDE,"subsp. ")
TAXREF$var = str_detect(TAXREF$NOM_VALIDE,"var. ")
TAXREF$f = str_detect(TAXREF$NOM_VALIDE," f. ")



for(i in 1:nrow(TAXREF)){
  if(TAXREF$subsp[i]){
    TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                           substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i],"subsp. ")+7,
                                  str_locate(TAXREF$NOM_VALIDE[i],"subsp. ")+9))
  }
  if(TAXREF$var[i]){
    TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                           substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i],"var. ")+5,
                                  str_locate(TAXREF$NOM_VALIDE[i],"var. ")+7))
  }
  if(TAXREF$f[i]){
    TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                           substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i]," f. ")+4,
                                  str_locate(TAXREF$NOM_VALIDE[i]," f. ")+6))
  }
  if((TAXREF$subsp[i]|TAXREF$var[i]|TAXREF$f[i])==FALSE){
    TAXREF$ESP[i] = TAXREF$ESPBRUT[i]
  }
  cat(i,"\n")
}

#Gestion des hybrides
TAXREF$x = str_detect(TAXREF$NOM_VALIDE," x ")
TAXREFX = TAXREF[TAXREF$x==TRUE,]
xsplit = str_split(TAXREFX$NOM_VALIDE," ")


#Application de la fonction ) la liste
fun = function(x){
  if(x[3]=="x"){
    paste0(left(x[1],3),left(x[2],3),"x",left(x[4],3),left(x[5],3))
  } else {
    paste0(left(x[1],3),"x",left(x[3],3))
  }
  
  
  
}


TAXREFX$ESP = unlist(lapply(xsplit,FUN=fun))
TAXREFX$ESP2 = TAXREFX$ESP

# Gestion des duplicats x
TAXREFDX = TAXREFX[duplicated(TAXREFX$ESP)==TRUE,]
for(i in 1:nrow(TAXREFDX)){
  num = c(1:nrow(TAXREFDX[TAXREFDX$ESP == TAXREFDX$ESP[i],]))
  for(j in 1:length(num)){
    if((TAXREFDX$subsp[i]|TAXREFDX$var[i]|TAXREFDX$f[i])==FALSE){
      TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP2[j] = paste0(TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP[j],num[j]+1)
    } else {
      cat("test")
      TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP2[j] = paste0(TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP[j],"sub")
    }
    
  }
  cat(i,"\n")
}

#REMISE dans le tableau principale
TAXREFX[duplicated(TAXREFX$ESP)==TRUE,] =TAXREFDX
TAXREF[TAXREF$x==TRUE,] = TAXREFX

# Dernier passage des doubons

TAXREFD = TAXREF[duplicated(TAXREF$ESP),]
TAXREFD$ESP2 = as.character(TAXREFD$ESP)

for(i in 1:nrow(TAXREFD)){
  num = c(1:nrow(TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]))
  for(j in 1:length(num)){
    TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]$ESP2[j] = paste0(TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]$ESP[j],num[j]+1)
  }
  cat(i,"\n")
}
TAXREF$ESP2 = TAXREF$ESP
TAXREF[duplicated(TAXREF$ESP),]=TAXREFD


### GENRE EN UN MOT :
JOIN_GENRE_SAISIE = TAXREF


for(i in 1:nrow(JOIN_GENRE_SAISIE)){
  if(JOIN_GENRE_SAISIE$RANG[i]=="GN"||JOIN_GENRE_SAISIE$RANG[i]=="FM"||JOIN_GENRE_SAISIE$RANG[i]=="TR"||JOIN_GENRE_SAISIE$RANG[i]=="SPTR"||JOIN_GENRE_SAISIE$RANG[i]=="SSTR"||JOIN_GENRE_SAISIE$RANG[i]=="SBFM"||JOIN_GENRE_SAISIE$RANG[i]=="SSGR"){
    JOIN_GENRE_SAISIE$ESP2[i]=JOIN_GENRE_SAISIE$LB_NOM[i]}
  cat(i,"\n")
}
# Retirer les colonnes de travail
TAXREF_FLORE_FR = JOIN_GENRE_SAISIE
colnames(TAXREF_FLORE_FR)[48] = "TRIGRAMME"
TAXREF_FLORE_FR$x = NULL
TAXREF_FLORE_FR$ESP = NULL
TAXREF_FLORE_FR$var = NULL
TAXREF_FLORE_FR$f = NULL
TAXREF_FLORE_FR$subsp = NULL
TAXREF_FLORE_FR$ESPBRUT = NULL

# On récupère les trigrammes dans la bonne colonne
TAXREF_FLORE_FR$TRIGRAMME = TAXREF_FLORE_FR$ESP2
TAXREF_FLORE_FR$ESP2 = NULL

# Enregistrer les nouveaux TAXREF en Csv
write.csv(TAXREF_FLORE_FR,file = "TAXREFv18_FLORE_FR.csv",fileEncoding = "UTF-8")
write.csv(TAXREF_FLORE_FR_SYN,file = "TAXREFv18_FLORE_FR_SYN.csv",fileEncoding = "UTF-8")

#Preparation de TAXAQgis
TAXAQgis = TAXREF_FLORE_FR %>%
  filter(!FR %in% c("A","W","X","Y","Z","Q")) %>% 
  arrange(LB_NOM)

TAXAQgis <- TAXAQgis[, c("CD_NOM", "LB_NOM","TRIGRAMME")]

#Enregistrement de TAXAQgis en CSV
write.csv(TAXAQgis,file = "TAXAQgis.csv",row.names = F,
          fileEncoding = "UTF-8")
