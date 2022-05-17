## packages
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)



################################################################################
################################################################################
#################          ANALYSES PAYSAGE         ############################
################################################################################
################################################################################

## Charger selection parcelle voulue

################################################################################
#################         Buffer % verger           ############################
################################################################################

#### On charge la couche d'OCS à jour
setwd("P:/SIG/DATA_CBC/OCS_BVD")
OCS_BVD<-st_read("BVD_OCS_21_V5_220228.shp")

### on cree un buffer autour du centroide des parcelles
buffer1000_candidats<-st_buffer(candidats, 500, 50) 

### Intersection avec OCS !!!
inter_OCS_buffer1000_candidats<-st_intersection(OCS_BVD, buffer1000_candidats) 

## new column avec area
inter_OCS_buffer1000_candidats$aire<-st_area(inter_OCS_buffer1000_candidats$geometry)

## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_OCS_buffer1000_candidats) <- NULL

## aire de chaque code parcelle
areas_buffer_candidats<-inter_OCS_buffer1000_candidats %>% 
  group_by(id_plot, Code_n1) %>%
  summarise(poly_n = n(), area_OCS=sum(aire))

## aire totale de chaque buffer
area_tot_buffer<-by(areas_buffer_candidats$area_OCS, areas_buffer_candidats$id_plot, sum)
Area_tot_buffer<-cbind(area_tot_buffer)
AREA_tot_buffer<-as.data.frame(Area_tot_buffer)
AREA_tot_buffer$id<-row.names(AREA_tot_buffer) 

#tableau final aires
compo_buffer_candidats <- areas_buffer_candidats %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="Code_n1", values_from="area_OCS")%>%
  mutate(id_plot=as.character(id_plot))

compo_buffer_candidats <- inner_join(compo_buffer_candidats, AREA_tot_buffer, by= c("id_plot"="id"))
colnames(compo_buffer_candidats) <- c("id_plot", "vergers", "urbain", "cult_annuelles", "milieu_nat", "prairies", "vignes", "autres", "area_tot_buffer")

#### pourcentage de surface occupe par chaque code parcelle
compo_buffer_candidats$pourcent_verger <- as.numeric(compo_buffer_candidats$vergers/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_urbain <- as.numeric(compo_buffer_candidats$urbain/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_cult_annuelle <- as.numeric(compo_buffer_candidats$cult_annuelles/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_milieu_nat <- as.numeric(compo_buffer_candidats$milieu_nat/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_prairies <- as.numeric(compo_buffer_candidats$prairies/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_vigne <- as.numeric(compo_buffer_candidats$vignes/compo_buffer_candidats$area_tot_buffer)*100
compo_buffer_candidats$pourcent_autre <- as.numeric(compo_buffer_candidats$autres/compo_buffer_candidats$area_tot_buffer)*100

## Faire une figure
p <- compo_buffer_candidats %>%
  ggplot( aes(x=pourcent_verger)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%")
p


################################################################################
#################         Buffer % filets           ############################
################################################################################

### On charge la couche numérisation filets
setwd("P:/SIG/Chantier_Numerisation_Filets")
numerisation <- st_read("Numerisation_Filets_Decembre21.shp")

## Joint entre OCS BVD et corr vergers, selection des Vergers AVEC filet
OCS_filets_BVD_x_filets_correction<-OCS_BVD%>%
  st_join(numerisation)%>%
  filter(grepl("^1", as.character(Code_n3.y)))%>%
  filter(filet=="oui"| filet=="mixte")

## On fait intersection entre buffers et couche des vergers filets
inter_VergerFilet_buffer1000_candidats<-st_intersection(OCS_filets_BVD_x_filets_correction, buffer1000_candidats) 

## new column avec area
inter_VergerFilet_buffer1000_candidats$airebuffer<-st_area(inter_VergerFilet_buffer1000_candidats$geometry)

## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_VergerFilet_buffer1000_candidats) <- NULL

## aire de chaque code parcelle
aires_buffer_candidat<-inter_VergerFilet_buffer1000_candidats %>% 
  group_by(id_plot) %>%
  summarise(poly_n = n(), area_OCS=sum(airebuffer))%>%
  mutate(percent_verger_filet=as.numeric(area_OCS/(3.141016*500*500))*100)


####### Rassembler les données filets avec donnees OCS 
zizou<-as.data.frame(merge(aires_buffer_candidat, compo_buffer_candidats, by="id_plot", all.y=T))
#### remplacer les NA par zero
zizou[is.na(zizou)]<-0

## Faire une figure filets / pas filet
pfilet <- zizou %>%
  ggplot( aes(x=percent_verger_filet)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%")
pfilet


#### FIGURE AVEC DOUBLE GRADIENT
ggplot(zizou, aes(x=pourcent_verger, y=percent_verger_filet)) + 
  geom_point(size=4) 


################################################################################
###################           Buffer % bio             #########################
################################################################################

setwd("P:/SIG/Bertrand/Verification couches fev 22")
vergerABpepin<-st_read("vergersPepinAB.shp")%>%
  filter(BIO==1 & (filet=="non"| filet=="mixte"))

## On fait intersection entre buffers et couche des vergers filets
inter_OCS_bio<-st_intersection(vergerABpepin, buffer1000_candidats) 

## new column avec area
inter_OCS_bio$airebuffer<-st_area(inter_OCS_bio$geometry)

## Calcul surface par ID (ID = chaque buffer)+ transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_OCS_bio) <- NULL

## aire de chaque code parcelle
aires_buffer_candidat_bio<-inter_OCS_bio %>% 
  group_by(id_plot) %>%
  summarise(poly_n = n(), area_OCS=sum(airebuffer))%>%
  mutate(percent_verger_bio=as.numeric(area_OCS/(3.141016*500*500))*100)

vergers_candidats2 <- merge (zizou, aires_buffer_candidat_bio, by="id_plot", all.x=T) 

## Faire une figure bio/pas bio
pbio <- vergers_candidats2 %>%
  ggplot( aes(x=percent_verger_bio)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%")
pbio

################################################################################
#################    merge compo buffer + pourcent filet    ####################
################################################################################


#### FIGURE AVEC DOUBLE GRADIENT
ggplot(vergers_candidats2, aes(x=percent_verger_bio, y=percent_verger_filet)) + 
  geom_point(size=4) 

### creer la couche !

## un merge pour avoir la geom
Vergers_AB_noFilet_2022<-candidats %>%
  merge(vergers_candidats2, all.y=T)

## Print
setwd("P:/SIG/Bertrand/Verification couches fev 22")
write.table(Vergers_AB_noFilet_2022, "Vergers_AB_noFilet_2022_2.csv", sep="\t")

# print la couche
setwd("P:/SIG/Bertrand/Verification couches fev 22")
st_write(Vergers_AB_noFilet_2022, dsn = "Vergers_AB_noFilet_2022.shp", layer = "Vergers_AB_noFilet_2022.shp", driver = "ESRI Shapefile", delete_layer=T)
st_write(Vergers_AB_noFilet_2022,"PG:dbname=postgis", "meuse", layer_options = "OVERWRITE=true")
