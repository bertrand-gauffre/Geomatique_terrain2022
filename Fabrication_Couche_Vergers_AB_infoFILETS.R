



##################   LA COUCHE DE TOUTES LES PARCELLES 
### On charge la couche d'OCS à jour
setwd("P:/SIG/DATA_CBC/OCS_BVD") 
OCS_BVD<-st_read("BVD_OCS_21_V5_220228.shp")

################################################################################
#############      La couche de numerisation des filets     ####################
################################################################################

### On charge la couche numérisation filets (necessaire tant que verif pas terminee)
setwd("P:/SIG/Chantier_Numerisation_Filets/DATA/")
verif_vergers <- st_read("230215_numerisation_filets.shp")
verif_vergers[which(verif_vergers$filet == "non"),"type_filet"]<-"absent"

### Combien de d'obs 
nb_ech_par<-verif_vergers %>%
  group_by(type_filet) %>%
  summarise(nbre=n());nb_ech_par

################################################################################
#############         La couche de vergers pepin AB         ####################
################################################################################

##################   AVOIR LA COUCHE DE VERGERS PEPINS & AB
## la couche agribio de 2020 + st_make_valid (sinon ça ne marche pas)
setwd("P:/SIG/DATA_PUBIC/Agribio")
Agribio_2019_2020<-st_make_valid(st_read("rpg-bio-2019-2020-r93_empriseBVD.shp"))
## 1) On fait intersection entre la couche Agribio et numerisation des filets
inter_AB_vergers_BVD<-st_intersection(verif_vergers, Agribio_2019_2020)
## 2) calcul de l'aire de l'intersection pour chaque parcelle
inter_AB_vergers_BVD$aire_interAB<-st_area(inter_AB_vergers_BVD$geometry)
#### Calcul surface par ID (ID = chaque parcelle)
## on enleve la geometrie avant
st_geometry(inter_AB_vergers_BVD) <- NULL
## aire de chaque code parcelle
area_inter_AB_vergers_BVD<-inter_AB_vergers_BVD %>% 
  group_by(FID) %>%
  summarise(poly_n = n(), area_AB=sum(aire_interAB))
# 3) Jointure avec la couche  vergers
parcelleAB<-st_as_sf(merge(area_inter_AB_vergers_BVD, verif_vergers, by="FID", all.x=T))
# calcul de l'aire de chaque parcelle
parcelleAB$aire_plot<-st_area(parcelleAB$geometry)
## rati aire AB / aire plot => pour eliminer parcelles qui "mordent" sur zone AB
parcelleAB$ratioABtot<-(parcelleAB$area_AB/parcelleAB$aire_plot)*100
#####  Couche de parcelles AB
parcelleAB_filtered<-parcelleAB %>%
  filter(as.numeric(ratioABtot)>50) %>%
  mutate(AB_cartobio=1)

setwd("C:/BERTRAND/SITE_ATELIER_BVD/SIG/DATA") 
st_write(parcelleAB_filtered, dsn = "vergersAB_2023.shp", layer = "vergersAB_2023.shp", driver = "ESRI Shapefile", delete_layer=T)



## On merge avec num/verif des vergers pour recup tous les vergers pepins
#enleve geom de verif verger
parcelleAB_filtered_noGeo<-parcelleAB_filtered
st_geometry(parcelleAB_filtered_noGeo) <- NULL

########  LA COUCHE DE VERGERS PEPINS & AB
vergerPepin_AB<-vergersPepin_BVD %>%
  merge(parcelleAB_filtered_noGeo,by="FID", all.x=T)%>%
  select(FID, Code_n3=Code_n3.x, ratioABtot, AB_cartobio, geometry)
vergerPepin_AB[is.na(vergerPepin_AB)]<-0

