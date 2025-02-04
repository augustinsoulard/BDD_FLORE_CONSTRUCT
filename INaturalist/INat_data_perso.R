# Charger le package rinat
if(!require("rinat")){install.packages("rinat")} ; library("rinat")
if(!require("tidyverse")){install.packages("tidyverse")} ; library("tidyverse")


# Récupérer 10000 observations de l'utilisateur augustinsoulard
observations <- get_inat_obs_user(username="augustinsoulard",maxresults = 10000) 
write.csv2(observations,"D:/INaturalist/INaturalist_Augustin_Soulard.csv",row.names = F,fileEncoding = 'UTF-8')
# Télécharger les photos
for (i in 1:nrow(observations)){
  cat(i,'/',nrow(observations),"\n")
  photos = get_inat_obs_id(observations$id[i])[["observation_photos"]][["photo"]]
  for(j in 1:nrow(photos)){
    url <- str_replace(photos$large_url[j], "large.jpg", "original.jpg")
    download.file(url, destfile = paste0("D:/INaturalist/IMG/",observations$scientific_name[i],"_",observations$id[i],"-",photos$id[j], ".jpg"), mode = "wb")
  }
}

# 660 /7586
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