# Charger le package rinat
if(!require("rinat")){install.packages("rinat")} ; library("rinat")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")


# Récupérer 10000 observations de l'utilisateur augustinsoulard
observations <- get_inat_obs_user(username="augustinsoulard",maxresults = 10000)
observations = read.csv("I:/Mon Drive/2_PROJETS/30_MAMP_CartoHabitats_Nord_Etang_Berre 2025/CARTO/inat/inat_tot_mamp_nrd_etg_berre_2025.csv")
write.csv2(observations,"D:/INaturalist/INaturalist_Augustin_Soulard.csv",row.names = F,fileEncoding = 'UTF-8')


# Si on ne repart aps de 0 utiliser le read.csv
# observations = read.csv2("D:/INaturalist/INaturalist_Augustin_Soulard.csv",h=T)


### Télécharger les photos
for (i in 209:nrow(observations)){
  cat(i,'/',nrow(observations),"\n")
  photos = get_inat_obs_id(observations$id[i])[["observation_photos"]][["photo"]]
  for(j in 1:nrow(photos)){
    url <- str_replace(photos$large_url[j], "large.jpg", "original.jpg")
    download.file(url, destfile = paste0("D:/INaturalist/IMG2/",observations$scientific_name[i],"_",observations$id[i],"-",photos$id[j], ".jpg"), mode = "wb")
  }
}

# 6665 /7586
# Dernier import 03/02/2025
# Api requete pour les données créées en 2025 : 
# if(!require("httr")){install.packages("httr")} ; library("httr")
# if(!require("jsonlite")){install.packages("jsonlite")} ; library("jsonlite")
# url <- "https://api.inaturalist.org/v1/observations?user_login=augustinsoulard&created_year=2025&order=desc&order_by=created_at"
# response <- GET(url)
# # Extraire le contenu JSON
# data <- content(response, as = "text", encoding = "UTF-8")
# 
# # Convertir en liste R
# json_data <- fromJSON(data, flatten = TRUE)
# 
# # Vérifier la structure des données
# str(json_data)
# 
# observations <- as.data.frame(json_data$results)
# head(observations)  # Aperçu des premières lignes