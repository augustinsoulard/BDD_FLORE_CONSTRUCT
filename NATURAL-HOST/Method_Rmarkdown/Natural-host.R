# Choisir le dossier de travail
WD = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(WD)

# Chargement des librairies

if(!require("readxl")){install.packages("readxl")} ; library("readxl")
if(!require("assertthat")){install.packages("assertthat")} ; library("assertthat")
if(!require("rmarkdown")){install.packages("rmarkdown",repos="http://cran.irsn.fr")
  require("rmarkdown")}
if(!require("knitr")){install.packages("knitr")} ; library("knitr")  

ASO <- read_excel("Natural-host.xlsx")
#ASO = read.xlsx("Natural-host.xlsx",1)

ASO = data.frame(lapply(ASO,factor))
DPlink = function (x){
  for(j in 1:nrow(x)){
    cat("[",as.character(x$text[j]),"]","(",as.character(x$lien[j]),")<br>","\n \n",sep = "")
  }
}

Xlevels = function(x){
  x = levels(factor(x))
  return(x)
}

con <- file("Natural-host.Rmd", open = "wt", encoding = "UTF-8")
sink(con,split=T)
cat("---
title: \"Natural-host v0.3\"
date: \"`r Sys.Date()`\"
output:
  html_document: 
    toc: TRUE
    toc_float: TRUE
    theme: flatly
    highlight: zenburn
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
``` \n \n")
# boucle titre 1
for(i in 1:length(levels(ASO$Titre_1))){
  T1 = ASO[ASO$Titre_1 == levels(ASO$Titre_1)[i],]
  cat("#",as.character(T1[1,1]),"{.tabset .tabset-fade .tabset-pills} \n \n")
  display = T1[is.na(T1$Titre_2),]
  if(nrow(display)==0 || is.na(display[1,5])){}else{DPlink(display)}
  
  #boucle titre 2
  for(e in 1:length(Xlevels(T1$Titre_2))){
    T2 = T1[T1$Titre_2 == as.character(Xlevels(T1$Titre_2)[e]),]
    T2 = T2[!is.na(T2$Titre_1),]
    if(is.na(T2[1,2])==FALSE){cat("<br>\n\n##",as.character(T2[1,2])," {.tabset .tabset-fade}\n \n")}
    display = T2[is.na(T2$Titre_3),]
    if(nrow(display)==0 || is.na(display[1,5])){}else{DPlink(display)}
    
    #boucle titre 3
    for(f in 1:length(Xlevels(T2$Titre_3))){
      T3 = T2[T2$Titre_3 == as.character(Xlevels(T2$Titre_3)[f]),]
      T3 = T3[!is.na(T3$Titre_2),]
      if(is.na(T3[1,3])==FALSE){cat("<br>\n\n###",as.character(T3[1,3]),"\n \n")}
      display = T3[is.na(T3$Titre_4),]
      if(nrow(display)==0 || is.na(display[1,5])){}else{DPlink(display)}
       #boucle titre 4
      if(length(Xlevels(T3$Titre_4))>0){
        for(g in 1:length(Xlevels(T3$Titre_4))){
        T4 = T3[T3$Titre_4 == as.character(Xlevels(T3$Titre_4)[g]),]
        T4 = T4[!is.na(T4$Titre_3),]
        if(is.na(T4[1,4])==FALSE){cat("<br>\n\n####",as.character(T4[1,4]),": \n \n")}
        display = T4
        if(nrow(display)==0 || is.na(display[1,5])){}else{DPlink(display)}
        }
      }
      cat("\n \n ------------------------------- \n \n")
      cat("\n \n ------------------------------- \n \n")
    }
  }

}

sink()
close(con)

#Test knit
rmarkdown::render("Natural-host.Rmd",output_format = "html_document",output_file = "Natural-host.html")


# debug
# T1 = ASO[ASO$Titre_1 == levels(ASO$Titre_1)[5],]
# T2 = T1[T1$Titre_2 == as.character(Xlevels(T1$Titre_2)[1]),]
# T3 = T2[T2$Titre_3 == as.character(Xlevels(T2$Titre_3)[2]),]
# T4 = T3[T2$Titre_4 == as.character(Xlevels(T3$Titre_4)[1]),]

