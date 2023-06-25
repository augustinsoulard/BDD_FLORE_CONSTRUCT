if (!require("do")) {install.packages("do")}+library("do")#For use left function
if (!require("tidyverse")) {install.packages("tidyverse")}+library("tidyverse")

setwd("C:/Users/Augustin Soulard/Documents/Programmation/Github/BDD_FLORE_CONSTRUCT/OUTRE-MER/GUYANE/TAXAQgis")
TAXREFv16 <- read.delim("TAXREFv16.txt", encoding="UTF-8")

# Creation du TAXREFv16_FLORE_GF
TAXREFv16_FLORE_GF = TAXREFv16[TAXREFv16$REGNE == "Plantae" & TAXREFv16$GF!="" & TAXREFv16$CD_REF == TAXREFv16$CD_NOM,]

write.csv2(TAXREFv16_FLORE_GF,"TAXREFv16_FLORE_GF.csv",row.names = F,fileEncoding = "UTF-8")

TAXAQgis_GF = TAXREFv16_FLORE_GF %>% select(CD_REF,LB_NOM,NOM_VERN,FAMILLE,GF)


write.csv2(TAXAQgis_GF,"TAXAQgis_GF.csv",row.names = F,fileEncoding = "UTF-8")









TAXREF = TAXREFv16_FLORE_FR
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



#Enregistrer le tableau au format csv
write.csv(TAXREF,file = "TaxrefTrigramme.csv",fileEncoding = "UTF-8")

TAXREFsimp = data.frame(TRIGRAMME = TAXREF$ESP2, NOM_VALIDE = TAXREF$NOM_VALIDE,CD_NOM = TAXREF$CD_REF,STATUT_FR = TAXREF$FR)
write.csv(TAXREFsimp,file = "TaxrefTrigrammesimp.csv",fileEncoding = "UTF-8",row.names = FALSE)



### GENRE EN UN MOT :
JOIN_GENRE_SAISIE = TAXREF


for(i in 1:nrow(JOIN_GENRE_SAISIE)){
  if(JOIN_GENRE_SAISIE$RANG[i]=="GN"||JOIN_GENRE_SAISIE$RANG[i]=="FM"||JOIN_GENRE_SAISIE$RANG[i]=="TR"||JOIN_GENRE_SAISIE$RANG[i]=="SPTR"||JOIN_GENRE_SAISIE$RANG[i]=="SSTR"||JOIN_GENRE_SAISIE$RANG[i]=="SBFM"||JOIN_GENRE_SAISIE$RANG[i]=="SSGR"){
    JOIN_GENRE_SAISIE$ESP2[i]=JOIN_GENRE_SAISIE$LB_NOM[i]}
  cat(i,"\n")
}
# Retirer les colonnes de travail
TAXREFv16_FLORE_FR = JOIN_GENRE_SAISIE
colnames(TAXREFv16_FLORE_FR)[48] = "TRIGRAMME"
TAXREFv16_FLORE_FR$x = NULL
TAXREFv16_FLORE_FR$ESP = NULL
TAXREFv16_FLORE_FR$var = NULL
TAXREFv16_FLORE_FR$f = NULL
TAXREFv16_FLORE_FR$subsp = NULL
TAXREFv16_FLORE_FR$ESPBRUT = NULL

# Enregistrer le nouveau TAXREF en Csv
write.csv(TAXREFv16_FLORE_FR,file = "TAXREFv16_FLORE_FR.csv",fileEncoding = "UTF-8")

#Préparation de TAXAQgis
TAXAQgis = TAXREFv16_FLORE_FR
TAXAQgis = TAXAQgis[,-c(1:4,6:10,12:15,17:20,22,23,25:41)]
TAXAQgis <- TAXAQgis[c("CD_NOM","TRIGRAMME","LB_NOM","NOM_VERN","FAMILLE","FR")]
write.csv(TAXAQgis,file = "TAXAQgis.csv",row.names=F)
