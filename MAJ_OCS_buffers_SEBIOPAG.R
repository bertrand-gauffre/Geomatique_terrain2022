
########## Charger les librairies
.libPaths("C:/BERTRAND/OUTILS/R/win-library/4.2", include.site = TRUE)
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)




#### On charge la couche d'OCS à jour
setwd("P:/SIG/DATA_CBC/OCS_BVD")
OCS_BVD<-st_read("BVD_OCS_21_V5_220228.shp")


#### On charge une couche sebiopag
setwd("P:/SIG/Lucas/Numerisation SEBIOPAG/Vergers2014")
exempleSEBIO<-st_read("parcelle4.shp")

### on cree un shapefile du centroide des parcelles sebiopag
center_exSebio<-st_centroid(exempleSEBIO)

## Joint entre OCS BVD et couche sebiopag
## on obtient une couche avec une ligne par polygone sans point et X lignes par polygone avec point (X etant le nbre de points par polygone)
## attention bien verif que chaque poly de Sebio a un objectID unique
OCS_BVD_Sebio_join<-OCS_BVD%>%
  st_join(center_exSebio)%>%
  filter(!is.na(OBJECTID))





