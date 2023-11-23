## packages
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)

################################################################################
################      TOUS les Vergers  de  2022          ######################
################################################################################

# le shapefile des parcelles de 2022
setwd("P:/SIG/TERRAIN_2022/Parcelles_2022")
parcelles_2022<-st_read("Parcelles_suivies_2022b.shp") 


## Les infos des parcelles 2022
setwd("P:/SIG/TERRAIN_2022/Parcelles_2022")
info_vergers_2022 <- read.csv("INFO_PARCELLES_2022.csv", header=T, sep=";") 

parcelles_2022_info <- merge(parcelles_2022, info_vergers_2022, by.x="id_plot", by.y="id_parcelle", all.x=T)
### on cree un shapefile du centroide des parcelles
center_parcelles_2022_info<-st_centroid(parcelles_2022_info)

################################################################################
##########     Vergers   possibles de  2022 (AB sans filets)    ################
################################################################################

## On recupere les parcelles recuperees par JCB
setwd("P:/SIG/TERRAIN_2022/SELECTION PARCELLES")
selection_vergers_2022 <- read.xlsx("ContactsEtSelectionParcelles2022.xlsx", "selection AB sans filet JCB2022") %>%
  filter(coord_ok=="oui")

### la couche des nouveaux vergers de 2022
vergers_2022<-st_as_sf(selection_vergers_2022, coords = c("coord_y","coord_x"), crs=4326)
vergers_2022_lambert2<-st_transform(vergers_2022, crs=2154)

## on cree un id_plot + infos parcelles
vergers_2022_lambert2$id_plot<-paste("2022",rownames(vergers_2022_lambert2), sep="_")
vergers_2022_lambert2$AB<-"OUI"
vergers_2022_lambert2$filet <- "NON"
vergers_2022_lambert2$paire <- "NON"

################################################################################
#############        Vergers AB sans filets de  2021            ################
################################################################################

### On charge la couche des parcelles de 2021
setwd("P:/SIG/DATA_CBC/Vergers_Cibles")
parcelles_2021<-st_read("Parcelles_suivies_2021.shp")

### on cree un shapefile du centroide des parcelles
center_parcelles_2021<-st_centroid(parcelles_2021)

### chargement des infos parcelles
setwd("P:/EPI/donnees chaudes/ZA Basse Durance/DATA_TERRAIN/2021")
data_parcelle<-read.table("INFO_PARCELLES_2021.csv", header=T, sep=";")
parcelles_2021 <- merge(center_parcelles_2021, data_parcelle, by.x="id_plot", by.y="id_parcelle", all.x=T)

## On selectionne les AB sans filets et encore en place de 2021
vergers_2021<-parcelles_2021 %>%  filter(AB=="OUI" & filet=="NON" & valide_2022=="ok")

vergers_2021$paire <- "OUI"
#vergers_2021$paire[vergers_2021$id_plot==237 | vergers_2021$id_plot==235]<-"OUI_FMK"


################################################################################
##########                Vergers      de  2021                 ################
################################################################################

### On charge la couche des parcelles de 2021
setwd("P:/SIG/DATA_CBC/Vergers_Cibles")
parcelles_2021<-st_read("Parcelles_suivies_2021.shp")

### on cree un shapefile du centroide des parcelles
center_parcelles_2021<-st_centroid(parcelles_2021)

### chargement des infos parcelles
setwd("P:/EPI/donnees chaudes/ZA Basse Durance/DATA_TERRAIN/2021")
data_parcelle<-read.table("INFO_PARCELLES_2021.csv", header=T, sep=";")
parcelles_2021 <- merge(center_parcelles_2021, data_parcelle, by.x="id_plot", by.y="id_parcelle", all.x=T)

## On selectionne les EXCLU encore en place de 2021 
vergers_Exclu_2021<-parcelles_2021 %>%  filter(exclu=="oui" & valide_2022=="ok" | valide_2022=="binome_arrachee")

## On selectionne les Sebiopag encore en place de 2021 
vergers_SEBIO_2021<-parcelles_2021 %>%  filter(sebiopag=="oui")


################################################################################
##########             Candidats valides EXCLU  de  2022        ################
################################################################################

### On charge la couche des parcelles de 2021
setwd("P:/SIG/TERRAIN_2022/SELECTION PARCELLES")
New_Exclu_parcelles_2022<-st_read("New_parcelles_Exclu_2022.shp")

### on cree un shapefile du centroide des parcelles
center_New_Exclu_parcelles_2022<-st_centroid(New_Exclu_parcelles_2022)


################################################################################
##########             Candidats  EXCLU  de  2022               ################
################################################################################

### On charge la couche des parcelles de 2021
setwd("P:/SIG/TERRAIN_2022/SELECTION PARCELLES")
Potential_Exclu_parcelles_2022<-st_read("Potential_parcelles_Exclu_2022.shp")

### on cree un shapefile du centroide des parcelles
center_Potential_Exclu_parcelles_2022<-st_centroid(Potential_Exclu_parcelles_2022)
st_crs(center_Potential_Exclu_parcelles_2022) <- 4326
center_Potential_Exclu_parcelles_2022<-st_transform(center_Potential_Exclu_parcelles_2022, 2154)

################################################################################
########       Regroupement 2021-2022 candidats AB no filet   ##################
################################################################################
vergers_2021$id_plot <- as.character(vergers_2021$id_plot)
candidats_ABnoFilet <- bind_rows(vergers_2022_lambert2, vergers_2021,) %>% select(c("id_plot","AB", "filet", "geometry", "paire"))

################################################################################
########       Regroupement 2021-2022     candidats EXCLU     ##################
################################################################################
vergers_Exclu_2021$id_plot <- as.character(vergers_Exclu_2021$id_plot)
candidats_EXCLU <- bind_rows(vergers_2022_lambert2, vergers_Exclu_2021,center_New_Exclu_parcelles_2022,center_Potential_Exclu_parcelles_2022) %>% 
  select(c("id_plot","AB", "filet", "geometry", "exclu", "valide_2022"))

candidats_EXCLU$exclu[is.na(candidats_EXCLU$exclu)]<-"Oiseaux"
candidats_EXCLU$exclu[is.na(candidats_EXCLU$filet)]<-"oui"
candidats_EXCLU$valide_2022[is.na(candidats_EXCLU$valide_2022)]<-"ok"

