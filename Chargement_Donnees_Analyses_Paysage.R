################################################################################
################################################################################
#                                                                              #
#     Fabrication et chargement des données pour les analyses paysagères       #
#                                                                              #
################################################################################
################################################################################

########## Charger les librairies
.libPaths("C:/BERTRAND/OUTILS/R/win-library/4.2", include.site = TRUE)
library(st) # utilisé ?
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

################################################################################
################           les Vergers  cibles            ######################
################################################################################

# le shapefile des parcelles de 2021
setwd("P:/SIG/DATA_CBC/Vergers_Cibles")
parcelles_2021<-st_read("Parcelles_suivies_2021.shp") 
parcelles_2022<-st_read("Parcelles_suivies_2022.shp") 
parcelles_2023<-st_read("Parcelles_suivies_2023.shp") 

# combine tout ça pour avoir 1 shapefile (sans parcelles dupliquées)
single_sf <- dplyr::bind_rows(list(parcelles_2021,parcelles_2022,parcelles_2023))
parcelles_21_22_23 <-single_sf %>% distinct(id_plot, .keep_all = TRUE)

# combien de parcelles
nrow(parcelles_21_22_23)

### cree un shapefile du centroide des parcelles
center_parcelles<-st_centroid(parcelles_21_22_23)


################################################################################
#############             La couche des filets              ####################
################################################################################

### charge la couche numérisation filets (necessaire tant que verif pas terminee)
setwd("P:/SIG/Chantier_Numerisation_Filets/DATA/")
verif_vergers <- st_read("230215_numerisation_filets.shp")
verif_vergers[which(verif_vergers$filet == "non"),"type_filet"]<-"absent"

### Combien de d'obs par filet 
nb_ech_par<-verif_vergers %>%
  group_by(type_filet) %>%
  summarise(nbre=n());nb_ech_par

### Combien de d'obs par OCS 
nb_ech_par<-verif_vergers %>%
  group_by(Code_n3) %>%
  summarise(nbre=n());print(nb_ech_par, n=30)

################################################################################
#############      La couche d'occupation du sol 2022        ###################
################################################################################

##################   LA COUCHE DE TOUTES LES PARCELLES (inclus agrandissement)
setwd("P:/SIG/Chantier_Numerisation_Filets/Alexis 2023")
OCS_BVD<-st_read("OCS Complet 2023.shp")

### Combien de d'obs 
nb_ech_par<-OCS_BVD %>%
  group_by(Code_n1) %>%
  summarise(nbre=n());print(nb_ech_par, n=10)


################################################################################
##################          La couche de Haies         #########################
################################################################################

setwd("P:/SIG/Chantier numerisation ZA BVD 2020/HAIES/DATA")
combined_haies <- st_read("fusionStephJCB_Nov23.shp")
plot(combined_haies)

## longueur totale de haies numerisees (3,965 km)
sum(st_length(combined_haies$geometry))

################################################################################
#############         La couche de vergers pepin AB         ####################
################################################################################

## la couche agribio de 2020 + st_make_valid (sinon ça ne marche pas)
setwd("P:/SIG/DATA_PUBIC/Agribio")
Agribio_2019_2020<-st_make_valid(st_read("rpg-bio-2019-2020-r93_empriseBVD.shp"))

## fabrique couche de vergers pepins à partir des verif vergers
vergersPepin_BVD<-verif_vergers%>%
  filter(grepl("^13", as.character(Code_n3)))%>%
  distinct(FID, .keep_all = TRUE)

### Combien de d'obs par OCS 
nb_ech_par<-vergersPepin_BVD %>%
  group_by(Code_n3) %>%
  summarise(nbre=n());print(nb_ech_par, n=30)

## 1) On fait intersection entre la couche Agribio et couche vergers pepins
inter_AB_OCS_BVD<-st_intersection(vergersPepin_BVD, Agribio_2019_2020)
## 2) calcul de l'aire de l'intersection pour chaque parcelle
inter_AB_OCS_BVD$aire_interAB<-st_area(inter_AB_OCS_BVD$geometry)

#### Calcul surface par ID (ID = chaque parcelle de notre parcellaire)
## on enleve la geometrie avant
st_geometry(inter_AB_OCS_BVD) <- NULL
## aire de chaque code parcelle
area_inter_AB_OCS_BVD<-inter_AB_OCS_BVD %>% 
  group_by(FID) %>%
  summarise(poly_n = n(), area_AB=sum(aire_interAB))
# 3) Jointure avec la couche  OCS pour recup geometrie
parcelleAB<-st_as_sf(merge(area_inter_AB_OCS_BVD, verif_vergers, by="FID", all.x=T))
# calcul de l'aire de chaque parcelle
parcelleAB$aire_plot<-st_area(parcelleAB$geometry)
## ratio aire AB / aire plot => pour eliminer parcelles qui "mordent" sur zone AB
parcelleAB$ratioABtot<-(parcelleAB$area_AB/parcelleAB$aire_plot)*100

#####  Couche de parcelles AB
vergerPepin_AB<-parcelleAB %>%
  filter(as.numeric(ratioABtot)>50) %>%  ## definir seuil a partir duquel on considere AB
  mutate(AB_cartobio=1)

## Ca donne quoi?
hist(vergerPepin_AB$ratioABtot, breaks = 20)







