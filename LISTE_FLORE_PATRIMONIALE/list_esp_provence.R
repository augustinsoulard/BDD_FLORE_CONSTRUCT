# Traitement de TAXREF15
TAXREF15 = read.table("C:/Users/Augustin Soulard/Documents/Nature/TAXREF/TAXREFv15.txt",h=T,sep="\t")

TAXREF15plantae = TAXREF15[TAXREF15$REGNE == "Plantae",]
TAXREF15PF = TAXREF15plantae[TAXREF15plantae$FR != "",]# TAXREF des plantes de France

write.table(TAXREF15PF,"C:/Users/Augustin Soulard/Documents/Nature/TAXREF/TAXREFv15_Plantae_FR.txt",row.names=F)
TAXREF15PF = read.table("C:/Users/Augustin Soulard/Documents/Nature/TAXREF/TAXREFv15_Plantae_FR.txt",h=T)
Encoding(TAXREF15PF$NOM_VERN) = "UTF-8"
Encoding(TAXREF15PF$NOM_VALIDE) = "UTF-8"

# Library
library(tidyverse)
# Charger les fichiers de statut
menace = read.csv2("G:/Mon Drive/Nature/Botanique/PACA/INPN_Flore_menacee_PACA_2022724.csv",h=T,fileEncoding = "UTF-8")
protection = read.csv2("G:/Mon Drive/Nature/Botanique/PACA/FLORE_protegee_PACA.csv",h=T,fileEncoding = "UTF-8")

Statut1 = full_join(menace,
          protection,
          by = "CD_NOM")

Statut2 = left_join(Statut1,TAXREF15PF,by = "CD_NOM")

# AJout des statut indigénat
Statut = read.csv2("C:/Users/Augustin Soulard/Documents/Nature/TAXREF/statuts_note.csv",h=T)


Statut3 = left_join(Statut2,Statut,by = c("FR"="STATUT"))

Statut3$PROTECTION = Statut3$ARTICLE
Statut3$PROTECTION[is.na(Statut3$PROTECTION)==FALSE] = "Protégé"

List_esp_men_pr = data.frame(CD_NOM=Statut3$CD_NOM,
                             NOM_VALIDE=Statut3$NOM_VALIDE,
                             NOM_VERN = Statut3$NOM_VERN,
                             HABITAT = Statut3$HABITAT,
                             INDIGENAT = Statut3$DESCRIPTION,
                             PROTECTION = Statut3$PROTECTION,
                             MENACE = Statut3$Catégorie.Régionale)
write.csv(List_esp_men_pr,"G:/Mon Drive/Nature/Botanique/PACA/ESPECE_MENACE_PRTEC_PACA.csv",na = "",row.names = F,fileEncoding = "UTF-8")

# Création du R markdown :
library(rvest)


img = data.frame(matrix(ncol = 2, nrow = length(List_esp_men_pr$NOM_VALIDE)))
for(i in 86:length(List_esp_men_pr$NOM_VALIDE)){
  esp_split = str_split(List_esp_men_pr$NOM_VALIDE[i]," ")
  esp = paste0(esp_split[[1]][1],"+",esp_split[[1]][2])

link = paste0("https://www.google.com/search?q=",esp,"&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjqs8z8wJH5AhWv34UKHeGGD60Q_AUoAXoECAIQAw&biw=1366&bih=657&dpr=1")
page = read_html(link)

name = page %>% html_nodes("div img") %>% html_attr('src')

img$X1[i] = name[2]
img$X2[i] = name[3]
cat(i,"\n")
}

# DOWNLOADING
for(i in 1:length(List_esp_men_pr$NOM_VALIDE)){
download.file(img$X1[i],paste0("C:/Users/Augustin Soulard/Documents/Nature/Botanique/HTML_IMG_PACA/img1/",i,".jpg"),mode = 'wb')
download.file(img$X2[i],paste0("C:/Users/Augustin Soulard/Documents/Nature/Botanique/HTML_IMG_PACA/img2/",i,".jpg"),mode = 'wb')
  
}


### Création du fichier Rmarkdown
con <- file("List_esp_men_pr.Rmd", open = "wt", encoding = "UTF-8")
sink(con,split=T)
cat("---
title: \"Espèce menacée et protégée de PACA\"
date: \"`r Sys.Date()`\"
output:
  html_document: 
    toc: TRUE
    toc_float: TRUE
    theme: darkly
    highlight: zenburn
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
List_esp_men_pr = read.csv('G:/Mon Drive/Nature/Botanique/PACA/ESPECE_MENACE_PRTEC_PACA.csv',fileEncoding = 'UTF-8',h=T)

``` \n \n")

for(i in 1:length(List_esp_men_pr$NOM_VALIDE)){
  cat("- ***\`r List_esp_men_pr$NOM_VALIDE[",i,"]\`***  ","\n")
  cat("- **\`r List_esp_men_pr$NOM_VERN[",i,"]\`**  ","\n")
  cat("- CD NOM :  **\`r List_esp_men_pr$CD_NOM[",i,"]\`**  ","\n")
  cat("- Indigénat :  \`r List_esp_men_pr$INDIGENAT[",i,"]\`  ","\n")
  cat("- Liste rouge :  \`r List_esp_men_pr$MENACE[",i,"]\`  ","\n")
  cat("- Protection :  \`r List_esp_men_pr$PROTECTION[",i,"]\`  ","\n")
  cat("![](C:/Users/Augustin Soulard/Documents/Nature/Botanique/HTML_IMG_PACA/img1/\`r ",i,"\`.jpg)  ","![](C:/Users/Augustin Soulard/Documents/Nature/Botanique/HTML_IMG_PACA/img2/\`r ",i,"\`.jpg)  ","\n  ")
  cat("\n  \n  \n<br><br><br>")
}

sink()
close(con)

#Test knit
rmarkdown::render("List_esp_men_pr.Rmd",output_format = "html_document",output_file = "List_esp_menacees.html")
