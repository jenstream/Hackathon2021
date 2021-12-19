install.packages("tidygeocoder")
install.packages("sf"); install.packages("mapview")
library(tidyr)
library(tidygeocoder)
library(tidyverse)




#Datensatz erstellen
test_daten<-data.frame(Ort = c("Adresse"), Adresse =c("Oberstraße 59, Witten"))

#Adresse geocoden
test_daten_b<-test_daten %>%
  add_column(type = "Adresse") %>%
  geocode(Adresse, method = 'osm', lat = latitude , long = longitude)
test_daten_b
test_daten_b$latitude
test_daten_b$longitude


