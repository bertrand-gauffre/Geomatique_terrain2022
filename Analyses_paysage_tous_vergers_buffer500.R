## packages
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)


################################################################################
##########     TOUS LES VERGERS AB sans filets de la zone       ################
################################################################################

##################   AVOIR LA COUCHE DE VERGERS PEPINS A JOUR 
### On charge la couche d'OCS à jour
setwd("P:/SIG/DATA_CBC/OCS_BVD") 
OCS_BVD<-st_read("BVD_OCS_21_V5_220228.shp")

### On charge la couche numérisation filets (necessaire tant que verif pas terminee)
setwd("P:/SIG/Chantier_Numerisation_Filets")
verif_vergers <- st_read("Numerisation_Filets_Decembre21.shp")

## Joint entre OCS BVD et verif vergers + selection des Vergers fruit a pepin 
vergersPepin_BVD_corrected<-OCS_BVD%>%
  st_join(verif_vergers)%>%
  filter(grepl("^13", as.character(Code_n3.y)))%>%
  distinct(FID, .keep_all = TRUE)

##################   AVOIR LA COUCHE DE VERGERS PEPINS & AB
## la couche agribio de 2020 + st_make_valid (sinon ça ne marche pas)
setwd("P:/SIG/DATA_PUBIC/Agribio")
Agribio_2019_2020<-st_make_valid(st_read("rpg-bio-2019-2020-r93_empriseBVD.shp"))
## 1) On fait intersection entre la couche Agribio et vergers pepins
inter_AB_vergersPepin<-st_intersection(vergersPepin_BVD_corrected, Agribio_2019_2020)
## 2) calcul de l'aire de l'intersection pour chaque parcelle
inter_AB_vergersPepin$aire_interAB<-st_area(inter_AB_vergersPepin$geometry)
#### Calcul surface par ID (ID = chaque parcelle)
## on enleve la geometrie avant
st_geometry(inter_AB_vergersPepin) <- NULL
## aire de chaque code parcelle
area_AB_in_ABvergerpepin<-inter_AB_vergersPepin %>% 
  group_by(FID) %>%
  summarise(poly_n = n(), area_AB=sum(aire_interAB))
# 3) Jointure avec la couche  vergers
vergerPepinAB0<-st_as_sf(merge(area_AB_in_ABvergerpepin,vergersPepin_BVD_corrected, by="FID", all.x=T))
# calcul de l'aire de chaque parcelle
vergerPepinAB0$aire_plot<-st_area(vergerPepinAB0$geometry)
## rati aire AB / aire plot => pour eliminer parcelles qui "mordent" sur zone AB
vergerPepinAB0$ratioABtot<-(vergerPepinAB0$area_AB/vergerPepinAB0$aire_plot)*100

##################   AVOIR LA COUCHE DE VERGERS PEPINS  & AB 
vergerPepinAB<-vergerPepinAB0 %>%
  filter(as.numeric(ratioABtot)>50) %>%
  mutate(AB_cartobio=1)
  
## On merge avec num/verif des vergers pour recup tous els vergers pepins
#enleve geom de verif verger
vergerPepinAB_noGeo<-vergerPepinAB
st_geometry(vergerPepinAB_noGeo) <- NULL

vergerPepin<-vergersPepin_BVD_corrected %>%
  merge(vergerPepinAB_noGeo,by="FID", all.x=T)%>%
  select(FID_plot=FID, Code_n3_plot=Code_n3.x.x, filet=filet.x, ratioABtot, AB_cartobio, geometry)
vergerPepin[is.na(vergerPepin)]<-0

##################   AVOIR LA COUCHE DE VERGERS PEPINS  & AB et sans filet
## On selectionne parmi les vergers pepins AB ceux qui sont sans filet (1060 parcelles)
vergerPepinABnofilet<-vergerPepinAB%>%
  filter(filet=="non")%>%
  filter(as.numeric(ratioABtot)>50)%>%
  select(FID_plot=FID, Code_n3_plot=Code_n3.y, filet, ratioABtot, AB_cartobio, geometry) 


## print la couche
setwd("P:/SIG/Bertrand/Verification couches fev 22")
#st_write(vergerPepinABnofilet, dsn = "1061parcellesABnofilet.shp", layer = "1061parcellesABnofilet.shp", driver = "ESRI Shapefile", delete_layer=T)


################################################################################
##########    Analyses paysageres sur vergers AB ss filets      ################
################################################################################

cible<-vergerPepin

### on cree un shapefile du centroide des parcelles
center_parcellesCible<-st_centroid(cible)
### on cree un buffer autour du centroide des parcelles
buffer_parcellesCible<-st_buffer(center_parcellesCible, 500, 50) 


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
  group_by(FID_plot, Code_n1) %>%
  summarise(poly_n = n(), area_OCS=sum(aire))

## aire totale de chaque buffer
area_tot_buffer_parcellesCible<-by(areas_buffer_parcellesCible$area_OCS, areas_buffer_parcellesCible$FID_plot, sum)
area_tot_buffer_parcellesCible<-cbind(area_tot_buffer_parcellesCible)
AREA_tot_buffer_parcellesCible<-as.data.frame(area_tot_buffer_parcellesCible)
AREA_tot_buffer_parcellesCible$id<-row.names(AREA_tot_buffer_parcellesCible) 

#tableau final aires
compo_buffer_parcellesCible <- areas_buffer_parcellesCible %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="Code_n1", values_from="area_OCS")%>%
  mutate(FID_plot=as.character(FID_plot))

compo_buffer_parcellesCible <- inner_join(compo_buffer_parcellesCible, AREA_tot_buffer_parcellesCible, by= c("FID_plot"="id"))
colnames(compo_buffer_parcellesCible) <- c("FID_plot", "vergers", "urbain", "cult_annuelles", "milieu_nat", "prairies", "vignes", "autres", "area_tot_buffer")

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
  group_by(FID_plot) %>%
  summarise(poly_n = n(), area_filets=sum(airebuffer))%>%
  mutate(percent_verger_filet=as.numeric(area_filets/(3.141016*500*500))*100) ## buffer 500


####### Rassembler les données filets avec donnees OCS 
zizou2<-as.data.frame(merge(aires_buffer_parcellesCible, compo_buffer_parcellesCible, by="FID_plot", all.y=T))
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
vergerPepinAB

## On fait intersection entre buffers et couche des vergers filets
inter_buffer_parcellesCible_bio<-st_intersection(vergerPepinAB, buffer_parcellesCible) 
## new column avec area
inter_buffer_parcellesCible_bio$airebuffer<-st_area(inter_buffer_parcellesCible_bio$geometry)
## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_buffer_parcellesCible_bio) <- NULL

## aire de chaque code parcelle
aires_buffer_parcellesCible_bio<-inter_buffer_parcellesCible_bio %>% 
  group_by(FID_plot) %>%
  summarise(poly_n = n(), area_AB=sum(airebuffer))%>%
  mutate(percent_verger_bio=as.numeric(area_AB/(3.141016*500*500))*100)

## On merge avec le tableau precedent
vergers_parcellesCible_final0 <- merge(zizou2, aires_buffer_parcellesCible_bio, by="FID_plot", all.x=T)
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
  group_by(FID_plot, NATURE) %>%
  summarise(poly_n = n(), area_OCS=sum(aire))

#tableau final aires
compo_VEGET_buffer_parcellesCible <- areas_VEGET_buffer_parcellesCible %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="NATURE", values_from="area_OCS")%>%
  mutate(FID_plot=as.character(FID_plot))%>%
  mutate(area_tot_buffer=(3.141016*500*500))

#### ATTZENTION VERIFIER QUE cA CORRESPOND AVANT
colnames(compo_VEGET_buffer_parcellesCible) <- c("FID_plot", "bois", "foret_fermee_feuillus", "haie", "verger", "foret_ouverte", "lande_ligneuse", "vigne", "foret_fermee_mixte", "foret_fermee_coniferes", "area_tot_buffer")

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


## On merge avec le tableau precedent
vergers_parcellesCible_final2 <- merge(vergers_parcellesCible_final, compo_VEGET_ESN_buffer_parcellesCible, by="FID_plot", all.x=T)
vergers_parcellesCible_final2<-vergers_parcellesCible_final2 %>% 
  select(c(1,2,3,4,5,6,7,8,9,10,11,22,23,24,25,26,27,28))



##########################################################
#################    merge compo buffer + pourcent filet    
##########################################################

## On recupere l'info spatiale contenue dans la couche de correction des vergers
vergers_parcellesCible_paysage_sp<-st_as_sf(merge(vergers_parcellesCible_final2, vergerPepin, by.x="FID_plot", by.y="FID_plot", all.x=T))



# Ajouter d'une couleur les parcelles de notre réseau via merge sur FID
## pour avoir le shapefiles des centroides des candidats
## faire tourner le script Selecion_parcelles_2022_info_paysage

################################################################################
########## LES PARCELLES DE 2022
################################################################################


center_parcelles_2022_info
center_parcelles_2022_info$parcelles_suivies<-1

## on fait un spatial join entre les vergers 
vergers_EXCLU_paysage<-vergers_parcellesCible_paysage_sp%>%
  st_join(center_parcelles_2022_info)%>%
  select(id_plot, parcelles_suivies, c(1:18), ratioABtot, Code_n3_plot,filet=filet.x, filet_ok=filet.y, id_plot, parcelles_suivies,
         exclu_paire, exclu_oiseaux,  exclu_paire, exclu_campa, exclu_capteurs, sebiopag, momac, framework, AB)%>%
 replace(is.na(.), 0)



## print la couche
setwd("P:/SIG/TERRAIN_2022/Parcelles_2022")
vergers_EXCLU_paysage_centroid<-st_centroid(vergers_EXCLU_paysage)
vergers_EXCLU_paysage_centroid_suivi<-vergers_EXCLU_paysage_centroid %>% filter(parcelles_suivies==1)
write.table(vergers_EXCLU_paysage_centroid_suivi, "parcelles_suivies_2022_paysage.csv", sep=";")




#### FIGURE AVEC DOUBLE GRADIENT  % vergers avec filets vs Haies
plot<-ggplot(vergers_EXCLU_paysage, aes(x=pourcent_verger, y=percent_haie, 
                                        size=as.factor(momac), shape=filet_ok, alpha=as.factor(momac), color=as.factor(momac))) + 
  geom_point() +
  xlab("% vergers")+
  ylab("% haies ")
plot+theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))

## juste les exclu
vergers_exclu<-vergers_candidats %>% filter(exclu=="oui" & valide_2022=="ok")
cor.test(vergers_exclu$percent_haie,vergers_exclu$percent_of_orchards_with_nets, method = "pearson")

## Faire une figure
p <- vergers_exclu %>%
  ggplot( aes(x=percent_verger_filet)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); p+theme(axis.text=element_text(size=14),
                                    axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))







################################################################################
#################         CANDIDATS AB SANS FILET      #########################
################################################################################
candidats_EXCLU
candidats_EXCLU$candidat<-1

## on fait un spatial join entre les vergers et on ne garde que les AB sans filet
vergers_ABnoFilet<-vergers_parcellesCible_paysage_sp%>%
  st_join(candidats_EXCLU)%>%
  select(c(1:18), ratioABtot, Code_n3_plot,filet=filet.x, filet_ok=filet.y, parcelle_suivie=id_plot, candidat, exclu, valide_2022)%>%
  replace(is.na(.), 0)%>%
  mutate(exclu_all=paste(exclu, valide_2022, sep="_"))%>%
  filter(exclu_all!="oui_binome_arrachee")%>%
  filter(filet!="oui")

vergers_ABnoFilet<-st_centroid(vergers_ABnoFilet)

##### Subset des vergers EXCLU et candidats
vergers_candidats_ABnoFilet<-vergers_ABnoFilet %>% filter(candidat==1, exclu_all!="oui_binome_arrachee")



#### FIGURE AVEC DOUBLE GRADIENT % vergers vs % vergers avec filets
plot<-ggplot(vergers_ABnoFilet, aes(x=percent_of_orchards_with_nets, y=pourcent_verger, size=as.factor(exclu_all), shape=filet_ok, alpha=as.factor(exclu_all), color=as.factor(exclu_all))) + 
  geom_point() +
  xlab("% vergers qui ont des filets")+
  ylab("% verger ")
plot+theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))

cor.test(vergers_ABnoFilet$pourcent_verger,vergers_ABnoFilet$percent_of_orchards_with_nets, method = "pearson")
cor.test(vergers_candidats_ABnoFilet$pourcent_verger,vergers_candidats_ABnoFilet$percent_of_orchards_with_nets, method = "pearson")


#### FIGURE AVEC DOUBLE GRADIENT  % vergers avec filets vs Haies
plot<-ggplot(vergers_ABnoFilet, aes(x=percent_of_orchards_with_nets, y=pourcent_verger, size=as.factor(exclu_all), shape=filet_ok, alpha=as.factor(exclu_all), color=as.factor(exclu_all))) + 
  geom_point() +
  xlab("% vergers qui ont des filets")+
  ylab("% haies ")
plot+theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))

cor.test(vergers_ABnoFilet$percent_haie,vergers_ABnoFilet$percent_of_orchards_with_nets, method = "pearson")
cor.test(vergers_candidats_ABnoFilet$percent_haie,vergers_candidats_ABnoFilet$percent_of_orchards_with_nets, method = "pearson")

## juste les exclu
vergers_exclu<-vergers_candidats %>% filter(exclu=="oui" & valide_2022=="ok")
cor.test(vergers_exclu$percent_haie,vergers_exclu$percent_of_orchards_with_nets, method = "pearson")



################################################################################
#########################     CANDIDATS EXCLU          #########################
################################################################################

candidats_EXCLU
candidats_EXCLU$candidat<-1

## on fait un spatial join entre les vergers 
vergers_EXCLU_paysage<-vergers_parcellesCible_paysage_sp%>%
  st_join(candidats_EXCLU)%>%
  select(c(1:18), ratioABtot, Code_n3_plot,filet=filet.x, filet_ok=filet.y, parcelle_suivie=id_plot, candidat, exclu, valide_2022)%>%
  replace(is.na(.), 0)%>%
  mutate(exclu_all=paste(exclu, valide_2022, sep="_"))

## print la couche
setwd("P:/SIG/Bertrand/Verification couches fev 22")
vergers_EXCLU_paysage<-st_centroid(vergers_EXCLU_paysage)
vergers_EXCLU_paysage_AB<-vergers_EXCLU_paysage %>% filter(as.numeric(ratioABtot)>50)
#write.table(vergers_EXCLU_paysage, "2005vergersAB_candidats_infoLandscape.csv", sep=";")

##### Subset des vergers EXCLU et candidats
vergers_candidats<-vergers_EXCLU_paysage %>% filter(candidat==1 & filet_ok=="NON")
setwd("P:/SIG/Bertrand/Verification couches fev 22")
vergers_candidats_centroide<-st_centroid(vergers_candidats)
#write.table(vergers_candidats_centroide, "41vergers_candidatsEXCLU_infoLandscape_220316.csv", sep=";")


#### FIGURE AVEC DOUBLE GRADIENT % vergers vs % vergers avec filets
plot<-ggplot(vergers_EXCLU_paysage, aes(x=percent_of_orchards_with_nets, y=pourcent_verger, size=as.factor(exclu_all), shape=filet_ok, alpha=as.factor(exclu_all), color=as.factor(exclu_all))) + 
  geom_point() +
  xlab("% vergers qui ont des filets")+
  ylab("% verger ")
plot+theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))

cor.test(vergers_EXCLU_paysage$pourcent_verger,vergers_EXCLU_paysage$percent_of_orchards_with_nets, method = "pearson")
cor.test(vergers_candidats$pourcent_verger,vergers_candidats$percent_of_orchards_with_nets, method = "pearson")
## juste les exclu
vergers_exclu<-vergers_candidats %>% filter(exclu=="oui" & valide_2022=="ok")
cor.test(vergers_exclu$pourcent_verger,vergers_exclu$percent_of_orchards_with_nets, method = "pearson")


#### FIGURE AVEC DOUBLE GRADIENT  % vergers avec filets vs Haies
plot<-ggplot(vergers_EXCLU_paysage, aes(x=pourcent_verger, y=percent_haie, size=as.factor(exclu_all), shape=filet_ok, alpha=as.factor(exclu_all), color=as.factor(exclu_all))) + 
  geom_point() +
  xlab("% vergers")+
  ylab("% haies ")
plot+theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))

## juste les exclu
vergers_exclu<-vergers_candidats %>% filter(exclu=="oui" & valide_2022=="ok")
cor.test(vergers_exclu$percent_haie,vergers_exclu$percent_of_orchards_with_nets, method = "pearson")

## Faire une figure
p <- vergers_exclu %>%
  ggplot( aes(x=percent_verger_filet)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); p+theme(axis.text=element_text(size=14),
                                    axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=12))




