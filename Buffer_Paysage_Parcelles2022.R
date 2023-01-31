

########## Charger les librairies
.libPaths("C:/BERTRAND/OUTILS/R/win-library/4.2", include.site = TRUE)
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)

################################################################################
################      les Vergers  suivis en  2021        ######################
################################################################################

# le shapefile des parcelles de 2021
setwd("P:/SIG/DATA_CBC/Vergers_Cibles")
parcelles_2021<-st_read("Parcelles_suivies_2021.shp") 

### on cree un shapefile du centroide des parcelles
center_parcelles_2021<-st_centroid(parcelles_2021)


################################################################################
################      les Vergers  suivis en  2022        ######################
################################################################################

# le shapefile des parcelles de 2022
setwd("P:/SIG/DATA_CBC/Vergers_Cibles")
parcelles_2022<-st_read("Parcelles_suivies_2022.shp") 

## Les infos des parcelles 2022
setwd("P:/EPI/donnees chaudes/ZA Basse Durance/DONNEES_TERRAIN_2022")
info_vergers_2022 <- read.csv("INFO_PARCELLES_2022.csv", header=T, sep=";") 

parcelles_2022_info <- merge(parcelles_2022, info_vergers_2022, by.x="id_plot", by.y="id_parcelle", all.x=T)

### on cree un shapefile du centroide des parcelles
center_parcelles_2022_info<-st_centroid(parcelles_2022_info)


################################################################################
#############      La couche d'occupation du sol 2022        ###################
################################################################################

##################   LA COUCHE DE TOUTES LES PARCELLES 
### On charge la couche d'OCS à jour
setwd("P:/SIG/DATA_CBC/OCS_BVD") 
OCS_BVD<-st_read("BVD_OCS_21_V5_220228.shp")


##################   LA COUCHE DES VERGERS PEPINS (code 13X)

## On extrait les vergers de fruits a pepin de la couche OCS 
vergersPepin_BVD<-OCS_BVD%>%
  filter(grepl("^13", as.character(Code_n3)))%>%
  distinct(FID, .keep_all = TRUE)


################################################################################
#############      La couche de numerisation des filets     ####################
################################################################################

### On charge la couche numérisation filets (necessaire tant que verif pas terminee)
setwd("P:/SIG/Chantier_Numerisation_Filets")
verif_vergers <- st_read("Numerisation_Filets_Decembre21.shp")


################################################################################
#############         La couche de vergers pepin AB         ####################
################################################################################

##################   AVOIR LA COUCHE DE VERGERS PEPINS & AB
## la couche agribio de 2020 + st_make_valid (sinon ça ne marche pas)
setwd("P:/SIG/DATA_PUBIC/Agribio")
Agribio_2019_2020<-st_make_valid(st_read("rpg-bio-2019-2020-r93_empriseBVD.shp"))
## 1) On fait intersection entre la couche Agribio et OCS_BVD
inter_AB_OCS_BVD<-st_intersection(OCS_BVD, Agribio_2019_2020)
## 2) calcul de l'aire de l'intersection pour chaque parcelle
inter_AB_OCS_BVD$aire_interAB<-st_area(inter_AB_OCS_BVD$geometry)
#### Calcul surface par ID (ID = chaque parcelle)
## on enleve la geometrie avant
st_geometry(inter_AB_OCS_BVD) <- NULL
## aire de chaque code parcelle
area_inter_AB_OCS_BVD<-inter_AB_OCS_BVD %>% 
  group_by(FID) %>%
  summarise(poly_n = n(), area_AB=sum(aire_interAB))
# 3) Jointure avec la couche  OCS
parcelleAB<-st_as_sf(merge(area_inter_AB_OCS_BVD, OCS_BVD, by="FID", all.x=T))
# calcul de l'aire de chaque parcelle
parcelleAB$aire_plot<-st_area(parcelleAB$geometry)
## rati aire AB / aire plot => pour eliminer parcelles qui "mordent" sur zone AB
parcelleAB$ratioABtot<-(parcelleAB$area_AB/parcelleAB$aire_plot)*100
#####  Couche de parcelles AB
parcelleAB_filtered<-parcelleAB %>%
  filter(as.numeric(ratioABtot)>50) %>%
  mutate(AB_cartobio=1)
## On merge avec num/verif des vergers pour recup tous les vergers pepins
#enleve geom de verif verger
parcelleAB_filtered_noGeo<-parcelleAB_filtered
st_geometry(parcelleAB_filtered_noGeo) <- NULL

########  LA COUCHE DE VERGERS PEPINS & AB
vergerPepin_AB<-vergersPepin_BVD %>%
  merge(parcelleAB_filtered_noGeo,by="FID", all.x=T)%>%
  select(FID, Code_n3=Code_n3.x, ratioABtot, AB_cartobio, geometry)
vergerPepin_AB[is.na(vergerPepin_AB)]<-0


################################################################################
##################          La couche de Haies         #########################
################################################################################

setwd("P:/SIG/Chantier numerisation ZA BVD 2020/HAIES/DATA")

Haies_JCB <- st_read("HaiesNumeriseesJCharles2022.shp")
#Haies_JCB <- Haies_JCB %>%
#  mutate(numerisateur = "JCB", id=row_number())%>%
#  dplyr::select(id, geometry,numerisateur)

Haies_STEF <- st_read("numerisation_haies_stephanie2022.shp")
#Haies_STEF <- Haies_STEF %>%
#  mutate(numerisateur = "STEF", id=row_number())%>%
# dplyr::select(id, geometry,numerisateur)

#combined_haies <- st_union(Haies_JCB, Haies_STEF)

setwd("P:/SIG/Chantier numerisation ZA BVD 2020/HAIES/DATA")
combined_haies <- st_read("fusionStephJCB_janv23.shp")
combined_haies <- combined_haies %>%
  mutate( id=row_number())%>%
  dplyr::select(id, A_verifier, layer, geometry)



################################################################################
################################################################################
#                                                                              #
#  A partir d'ici il faut modifier IMPUT FILE de parcelle et taille de buffer  #
#                                                                              #
################################################################################
################################################################################

### on cree un buffer autour du centroide des parcelles
taille_du_buffer<-500
### ici changer le shapefile de l'annee
buffer_parcellesCible<-st_buffer(center_parcelles_2021, taille_du_buffer, 50) 


##########################################
#################      % OCS within buffers        
##########################################

### Intersection avec OCS !!!
inter_OCS_buffer_parcellesCible<-st_intersection(OCS_BVD, buffer_parcellesCible) 
## new column avec area
inter_OCS_buffer_parcellesCible$aire<-st_area(inter_OCS_buffer_parcellesCible$geometry)

#### Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_OCS_buffer_parcellesCible) <- NULL
## aire de chaque code parcelle
areas_buffer_parcellesCible<-inter_OCS_buffer_parcellesCible %>% 
  group_by(id_plot, Code_n1) %>%
  summarise(poly_n = n(), area_OCS=sum(aire))

## aire totale de chaque buffer
area_tot_buffer_parcellesCible<-by(areas_buffer_parcellesCible$area_OCS, areas_buffer_parcellesCible$id_plot, sum)
area_tot_buffer_parcellesCible<-cbind(area_tot_buffer_parcellesCible)
AREA_tot_buffer_parcellesCible<-as.data.frame(area_tot_buffer_parcellesCible)
AREA_tot_buffer_parcellesCible$id<-row.names(AREA_tot_buffer_parcellesCible) 

#tableau final aires
compo_buffer_parcellesCible <- areas_buffer_parcellesCible %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="Code_n1", values_from="area_OCS")%>%
  mutate(id_plot=as.character(id_plot))

compo_buffer_parcellesCible <- inner_join(compo_buffer_parcellesCible, AREA_tot_buffer_parcellesCible, by= c("id_plot"="id"))
colnames(compo_buffer_parcellesCible) <- c("id_plot", "vergers", "urbain", "cult_annuelles", "milieu_nat", "prairies", "vignes", "autres", "area_tot_buffer")

#### pourcentage de surface occupe par chaque code parcelle
compo_buffer_parcellesCible$pourcent_verger <- as.numeric(compo_buffer_parcellesCible$vergers/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_urbain <- as.numeric(compo_buffer_parcellesCible$urbain/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_cult_annuelle <- as.numeric(compo_buffer_parcellesCible$cult_annuelles/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_milieu_nat <- as.numeric(compo_buffer_parcellesCible$milieu_nat/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_prairies <- as.numeric(compo_buffer_parcellesCible$prairies/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_vigne <- as.numeric(compo_buffer_parcellesCible$vignes/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_autre <- as.numeric(compo_buffer_parcellesCible$autres/compo_buffer_parcellesCible$area_tot_buffer)*100

## Faire une figure
p <- compo_buffer_parcellesCible %>%
  ggplot( aes(x=pourcent_verger)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); p

############################################
#################     % filets within Buffer   
############################################

### On charge la couche numérisation filets
verif_vergers

## Joint entre OCS BVD et corr vergers, selection des Vergers AVEC filet
OCS_filets_BVD_x_filets_correction<-OCS_BVD%>%
  st_join(verif_vergers)%>%
  filter(grepl("^1", as.character(Code_n3.y)))%>%
  filter(filet=="oui"| filet=="mixte")
## On fait intersection entre buffers et couche des vergers filets
inter_VergerFilet_buffer_parcellesCible<-st_intersection(OCS_filets_BVD_x_filets_correction, buffer_parcellesCible) 
## new column avec area
inter_VergerFilet_buffer_parcellesCible$airebuffer<-st_area(inter_VergerFilet_buffer_parcellesCible$geometry)
## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_VergerFilet_buffer_parcellesCible) <- NULL

## aire de chaque code parcelle
aires_buffer_parcellesCible<-inter_VergerFilet_buffer_parcellesCible %>% 
  group_by(id_plot) %>%
  summarise(poly_n = n(), area_filets=sum(airebuffer))%>%
  mutate(percent_verger_filet=as.numeric(area_filets/(3.141016*taille_du_buffer*taille_du_buffer))*100) ## 


####### Rassembler les données filets avec donnees OCS 
zizou2<-as.data.frame(merge(aires_buffer_parcellesCible, compo_buffer_parcellesCible, by="id_plot", all.y=T))
#### remplacer les NA par zero (recup les parcelles sans filet dans l buffer)
zizou2[is.na(zizou2)]<-0

#### Calcul de la proportion des vergers qui ont des filets
zizou2$percent_of_orchards_with_nets<-as.numeric(zizou2$area_filets/zizou2$vergers)*100

## Faire une figure filets / pas filet
fig_filets <- zizou2 %>%
  ggplot( aes(x=percent_verger_filet)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%");fig_filets

#### FIGURE AVEC DOUBLE GRADIENT
ggplot(zizou2, aes(x=pourcent_verger, y=percent_verger_filet)) + 
  geom_point(size=1) 

###########################################
###################           Buffer % bio 
###########################################

## la couche des vergers (code 13X) qui sont AB 
vergerPepin_AB

## On fait intersection entre buffers et couche des vergers filets
inter_buffer_parcellesCible_bio<-st_intersection(vergerPepin_AB, buffer_parcellesCible) 
## new column avec area
inter_buffer_parcellesCible_bio$airebuffer<-st_area(inter_buffer_parcellesCible_bio$geometry)
## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_buffer_parcellesCible_bio) <- NULL

## aire de chaque code parcelle
aires_buffer_parcellesCible_bio<-inter_buffer_parcellesCible_bio %>% 
  group_by(id_plot) %>%
  summarise(poly_n = n(), area_AB=sum(airebuffer))%>%
  mutate(percent_verger_bio=as.numeric(area_AB/(3.141016*taille_du_buffer*taille_du_buffer))*100)

## On merge avec le tableau precedent
vergers_parcellesCible_final0 <- merge(zizou2, aires_buffer_parcellesCible_bio, by="id_plot", all.x=T)
#### remplacer les NA par zero (recup les parcelles sans AB dans l buffer)
vergers_parcellesCible_final0[is.na(vergers_parcellesCible_final0)]<-0


### Le dataset complet
vergers_parcellesCible_final<-vergers_parcellesCible_final0 %>% 
  select(c(1,13,14,15,16,17,18,19,23,4,20))

## Faire une figure bio/pas bio
pbio <- vergers_parcellesCible_final %>%
  ggplot( aes(x=percent_verger_bio)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); pbio


##########################################
#################      Lineaire de haies within buffer        
##########################################

inter_haies_buffer_parcellesCible<-st_intersection(combined_haies, buffer_parcellesCible) 
## new column avec area
inter_haies_buffer_parcellesCible$longueur<-st_length(inter_haies_buffer_parcellesCible$geometry)

#### Calcul lineaire par ID (ID = chaque buffer) + transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_haies_buffer_parcellesCible) <- NULL
## aire de chaque code parcelle
inter_haies_buffer_parcellesCible<-inter_haies_buffer_parcellesCible %>% 
  group_by(id_plot) %>%
  summarise(nb_haies = n(), length_haies=sum(longueur))


################################################################################
############## FINALISATION DU FICHIER

## On merge avec le tableau precedent
Compo_paysage_vergers_cible_buffer <- merge(vergers_parcellesCible_final, inter_haies_buffer_parcellesCible, by="id_plot", all.x=T)
Compo_paysage_vergers_cible_buffer<-Compo_paysage_vergers_cible_buffer %>% 
select(c(1,2,3,4,5,6,7,8,9,10,11,13))


#########################   Buffer 2022   ###################

## Buffer 2022 // 1000 m
colnames(Compo_paysage_vergers_cible_buffer) <- paste(colnames(Compo_paysage_vergers_cible_buffer),"buffer1000",sep="_")
buffer1000_parcelles2022<-Compo_paysage_vergers_cible_buffer

## Buffer 2022 // 500 m
colnames(Compo_paysage_vergers_cible_buffer) <- paste(colnames(Compo_paysage_vergers_cible_buffer),"buffer500",sep="_")
buffer500_parcelles2022<-Compo_paysage_vergers_cible_buffer

paysage_2022 <- merge(buffer500_parcelles2022, buffer1000_parcelles2022, by.x="id_plot_buffer500", by.y="id_plot_buffer1000", all.x=F, all.y=F)
names(paysage_2022)[1]<-paste("id_plot")
  

#########################   Buffer 2021   ###################

## Buffer 2021 // 1000 m
colnames(Compo_paysage_vergers_cible_buffer) <- paste(colnames(Compo_paysage_vergers_cible_buffer),"buffer1000",sep="_")
buffer1000_parcelles2021<-Compo_paysage_vergers_cible_buffer

## Buffer 2021 // 500 m
colnames(Compo_paysage_vergers_cible_buffer) <- paste(colnames(Compo_paysage_vergers_cible_buffer),"buffer500",sep="_")
buffer500_parcelles2021<-Compo_paysage_vergers_cible_buffer

paysage_2021 <- merge(buffer500_parcelles2021, buffer1000_parcelles2021, by.x="id_plot_buffer500", by.y="id_plot_buffer1000", all.x=F, all.y=F)
names(paysage_2021)[1]<-paste("id_plot")


#################  On combine les 2 années
theDataSet<-rbind(paysage_2021,paysage_2022)
list_plot<-as.data.frame(unique(theDataSet$id_plot))
names(list_plot)[1]<-paste("id_plot")

paysageB500B1000_allVergers<-distinct(theDataSet)
setwd("C:/BERTRAND/PROJECT_Filets/ANALYSES/DONNEES PAYSAGE")
write.csv(paysageB500B1000_allVergers, "DonneesPaysage_B500_B1000_Vergers21et22_version2023.csv", row.names = FALSE)


### petit truc pour les parcelles de jean charles

paysageB500B1000_allVergers

## Les infos des parcelles 2022
setwd("P:/EPI/donnees chaudes/ZA Basse Durance/DONNEES_TERRAIN_2022")
info_vergers_2022 <- read.csv("INFO_PARCELLES_2022.csv", header=T, sep=";") 

paysageB500B1000_allVergers_INFO <- merge(paysageB500B1000_allVergers, info_vergers_2022, by.x="id_plot", by.y="id_parcelle", all.x=T)

paysageB500B1000_allVergers_INFO_JCB<- paysageB500B1000_allVergers_INFO%>%
  filter(exclu_oiseaux=="oui")

setwd("C:/BERTRAND/PROJECT_Filets/ANALYSES/DONNEES PAYSAGE")
write.csv(paysageB500B1000_allVergers_INFO_JCB, "DonneesPaysage_B500_B1000_InfoParcelle_VergersExcluBirds.csv", row.names = FALSE)








### Pour les haies et ESN avant la couche de lineaire de haies

###########################################
###################           Buffer % ESN 
###########################################

### Charger la couche BDTOPO vegetation 2020
setwd("P:/SIG/Bertrand/BDTOPO2020")
VEGET_BDTOPO<-st_read("BDTOPO_VEGET_2020_BVD.shp")

### Intersection avec OCS !!!
inter_VEGET_buffer_parcellesCible<-st_intersection(VEGET_BDTOPO, buffer_parcellesCible) 
## new column avec area
inter_VEGET_buffer_parcellesCible$aire<-st_area(inter_VEGET_buffer_parcellesCible$geometry)

#### Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_VEGET_buffer_parcellesCible) <- NULL
## aire de chaque code parcelle
areas_VEGET_buffer_parcellesCible<-inter_VEGET_buffer_parcellesCible %>% 
  group_by(id_plot, NATURE) %>%
  summarise(poly_n = n(), area_OCS=sum(aire))

#tableau final aires
compo_VEGET_buffer_parcellesCible <- areas_VEGET_buffer_parcellesCible %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="NATURE", values_from="area_OCS")%>%
  mutate(id_plot=as.character(id_plot))%>%
  mutate(area_tot_buffer=(3.141016*taille_du_buffer*taille_du_buffer))

#### ATTZENTION VERIFIER QUE cA CORRESPOND AVANT
colnames(compo_VEGET_buffer_parcellesCible) <- c("id_plot", "bois", "foret_fermee_feuillus", "haie", "lande_ligneuse", "verger", "foret_fermee_mixte", "foret_ouverte", "vigne", "foret_fermee_coniferes", "area_tot_buffer")

#### pourcentage de surface occupe par chaque code parcelle
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_feuillus <- as.numeric(compo_VEGET_buffer_parcellesCible$foret_fermee_feuillus/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100
compo_VEGET_buffer_parcellesCible$percent_haie <- as.numeric(compo_VEGET_buffer_parcellesCible$haie/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100
compo_VEGET_buffer_parcellesCible$percent_foret_ouverte <- as.numeric(compo_VEGET_buffer_parcellesCible$foret_ouverte/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100
compo_VEGET_buffer_parcellesCible$percent_lande_ligneuse <- as.numeric(compo_VEGET_buffer_parcellesCible$lande_ligneuse/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_mixte <- as.numeric(compo_VEGET_buffer_parcellesCible$foret_fermee_mixte/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_coniferes <- as.numeric(compo_VEGET_buffer_parcellesCible$foret_fermee_coniferes/compo_VEGET_buffer_parcellesCible$area_tot_buffer)*100

## remplacer NA par zero
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_feuillus[is.na(compo_VEGET_buffer_parcellesCible$percent_foret_fermee_feuillus)] <- 0
compo_VEGET_buffer_parcellesCible$percent_haie[is.na(compo_VEGET_buffer_parcellesCible$percent_haie)] <- 0
compo_VEGET_buffer_parcellesCible$percent_foret_ouverte[is.na(compo_VEGET_buffer_parcellesCible$percent_foret_ouverte)] <- 0
compo_VEGET_buffer_parcellesCible$percent_lande_ligneuse[is.na(compo_VEGET_buffer_parcellesCible$percent_lande_ligneuse)] <- 0
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_mixte[is.na(compo_VEGET_buffer_parcellesCible$percent_foret_fermee_mixte)] <- 0
compo_VEGET_buffer_parcellesCible$percent_foret_fermee_coniferes[is.na(compo_VEGET_buffer_parcellesCible$percent_foret_fermee_coniferes)] <- 0

## calcul tot ESN
compo_VEGET_ESN_buffer_parcellesCible<-compo_VEGET_buffer_parcellesCible%>%
  mutate(percent_ESN=percent_foret_fermee_feuillus+percent_haie+percent_foret_ouverte+percent_lande_ligneuse+percent_foret_fermee_mixte+percent_foret_fermee_coniferes)

## Faire une figure
p <- compo_VEGET_ESN_buffer_parcellesCible %>%
  ggplot( aes(x=percent_ESN)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); p

