
################################################################################
################################################################################
#                                                                              #
#  Calcul des variables paysageres dans les buffers autour des vergers cibles  #
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
################            Avant de commencer            ######################
################################################################################

### definir le rayon de buffer souhaite
#########################################
taille_du_buffer<-1000
#########################################

## Possibilite de selectionner un sous ensemble des parcelles
## faire un subset de center_parcelles à partir de la liste des
## id_plot des parcelles qui vous interessent

### desiner buffer sur le shapefile des centroides de parcelle 
buffer_parcellesCible<-st_buffer(center_parcelles, taille_du_buffer, 50) 

plot(buffer_parcellesCible)

################################################################################
#############           %  OCS dans les buffers              ###################
################################################################################

### Intersection buffers avec la couche OCS 
inter_OCS_buffer_parcellesCible<-st_intersection(OCS_BVD, buffer_parcellesCible) 
plot(inter_OCS_buffer_parcellesCible[,5])

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

hist(AREA_tot_buffer_parcellesCible$area_tot_buffer_parcellesCible)

#tableau final aires : pivot
compo_buffer_parcellesCible <- areas_buffer_parcellesCible %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="Code_n1", values_from="area_OCS")%>%
  mutate(id_plot=as.character(id_plot))

# on merge compo par OCS et aire totale
compo_buffer_parcellesCible <- inner_join(compo_buffer_parcellesCible, AREA_tot_buffer_parcellesCible, by= c("id_plot"="id"))
compo_buffer_parcellesCible <-compo_buffer_parcellesCible[,c("id_plot", "1", "2", "3", "4", "5", "6", "0", "area_tot_buffer_parcellesCible")]
colnames(compo_buffer_parcellesCible) <- c("id_plot", "vergers", "urbain", "cult_annuelles", "milieu_nat", "prairies", "vignes", "autres", "area_tot_buffer")

#### pourcentage de surface occupe par chaque code parcelle
compo_buffer_parcellesCible$pourcent_vergerALM <- as.numeric(compo_buffer_parcellesCible$vergers/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_urbain <- as.numeric(compo_buffer_parcellesCible$urbain/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_cult_annuelle <- as.numeric(compo_buffer_parcellesCible$cult_annuelles/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_milieu_nat <- as.numeric(compo_buffer_parcellesCible$milieu_nat/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_prairies <- as.numeric(compo_buffer_parcellesCible$prairies/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_vigne <- as.numeric(compo_buffer_parcellesCible$vignes/compo_buffer_parcellesCible$area_tot_buffer)*100
compo_buffer_parcellesCible$pourcent_autre <- as.numeric(compo_buffer_parcellesCible$autres/compo_buffer_parcellesCible$area_tot_buffer)*100

## Finalisation du dataset (ne pas prendre les vergers !)
compo_OCS_buffer_parcelles_Cible_final<-compo_buffer_parcellesCible %>% 
  select(c("id_plot", "area_tot_buffer", "pourcent_urbain", "pourcent_cult_annuelle", "pourcent_milieu_nat", "pourcent_prairies", "pourcent_vigne", "pourcent_autre")) %>% 
  replace(is.na(.),0)

##################
####   FIGURES    
##################

## histogramme
p <- compo_buffer_parcellesCible %>%
  ggplot( aes(x=pourcent_verger)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); p


################################################################################
#############       %  vergers et filets dans buffers        ###################
#############                  CODE A NETTOYER               ###################
################################################################################

### On charge la couche numérisation filets
verif_vergers
### on cree un shapefile du centroide des vergers verifies
center_vergers<-st_centroid(verif_vergers)

## Jointure spatiale entre OCS BVD et corr vergers + selection des vergers 
OCS_filets_BVD_x_filets_correction<-OCS_BVD%>%
  st_join(center_vergers)%>%
  filter(grepl("^1", as.character(Code_n3.y)))

### Intersection avec buffer !!!
inter_OCS_buffer_vergers<-st_intersection(OCS_filets_BVD_x_filets_correction, buffer_parcellesCible) 
plot(inter_OCS_buffer_vergers[,11])

## new column avec area
inter_OCS_buffer_vergers$aire<-st_area(inter_OCS_buffer_vergers$geometry)

## on enleve la geometrie avant
st_geometry(inter_OCS_buffer_vergers) <- NULL
## aire de chaque code parcelle
areas_buffer_filets<-inter_OCS_buffer_vergers %>% 
  group_by(id_plot, type_filet) %>%
  summarise(poly_n = n(), area_filets=sum(aire))

## aire totale de verger dans chaque buffer (tout type de verger)
areas_buffer_verger<-by(areas_buffer_filets$area_filets, areas_buffer_filets$id_plot, sum)
areas_buffer_verger<-cbind(areas_buffer_verger)
areas_buffer_verger<-as.data.frame(areas_buffer_verger)
areas_buffer_verger$id<-row.names(areas_buffer_verger) 

hist(areas_buffer_verger$areas_buffer_verger)

#tableau final aires : un pivot
compo_filet_buffer_parcellesCible <- areas_buffer_filets %>% 
  select(!(poly_n))%>%
  pivot_wider(names_from="type_filet", values_from="area_filets")
# on rename les col
colnames(compo_filet_buffer_parcellesCible) <- c("id_plot", "absent", "monoparcelle", "monorang", "non_deploye", "inconnu")
## regler pb des NA !!! => Alexis

#### Pour Calcul surface tot verger
## mise en forme du tableau 
compo_filet_buffer_parcellesCible<-compo_filet_buffer_parcellesCible%>%
  mutate(absent= as.numeric(absent), monoparcelle= as.numeric(monoparcelle),monorang= as.numeric(monorang),non_deploye= as.numeric(non_deploye), inconnu= as.numeric(inconnu))%>%
  replace(is.na(.),0)%>% 
  mutate(id_plot=as.character(id_plot))

## Jointure avec les données d'aire totale de verger 
compo_filet_buffer_parcellesCible <- inner_join(compo_filet_buffer_parcellesCible, areas_buffer_verger, by= c("id_plot"="id"))

#### pourcentage de surface occupe par chaque type de filet par rapport aux vergers
compo_filet_buffer_parcellesCible$pourcent_vergers_filet_absent <- as.numeric((compo_filet_buffer_parcellesCible$absent/compo_filet_buffer_parcellesCible$areas_buffer_verger)*100)
compo_filet_buffer_parcellesCible$pourcent_vergers_filet_monoparcelle <- as.numeric((compo_filet_buffer_parcellesCible$monoparcelle/compo_filet_buffer_parcellesCible$areas_buffer_verger)*100)
compo_filet_buffer_parcellesCible$pourcent_vergers_filet_monorang <- as.numeric((compo_filet_buffer_parcellesCible$monorang/compo_filet_buffer_parcellesCible$areas_buffer_verger)*100)
compo_filet_buffer_parcellesCible$pourcent_vergers_filet_non_deploye <- as.numeric((compo_filet_buffer_parcellesCible$non_deploye/compo_filet_buffer_parcellesCible$areas_buffer_verger)*100)

#### pourcentage de surface du buffer occupe par chaque type de filet sur l'aire totale du buffer 
compo_filet_buffer_parcellesCible$pourcent_filet_absent <- as.numeric((compo_filet_buffer_parcellesCible$absent/(3.141016*taille_du_buffer*taille_du_buffer))*100)
compo_filet_buffer_parcellesCible$pourcent_filet_monoparcelle <- as.numeric((compo_filet_buffer_parcellesCible$monoparcelle/(3.141016*taille_du_buffer*taille_du_buffer))*100)
compo_filet_buffer_parcellesCible$pourcent_filet_monorang <- as.numeric((compo_filet_buffer_parcellesCible$monorang/(3.141016*taille_du_buffer*taille_du_buffer))*100)
compo_filet_buffer_parcellesCible$pourcent_filet_non_deploye <- as.numeric((compo_filet_buffer_parcellesCible$non_deploye/(3.141016*taille_du_buffer*taille_du_buffer))*100)
compo_filet_buffer_parcellesCible$pourcent_filet_inconnu <- as.numeric((compo_filet_buffer_parcellesCible$inconnu/(3.141016*taille_du_buffer*taille_du_buffer))*100)

# calcul surface vergers
compo_filet_buffer_parcellesCible$pourcent_vergers <-compo_filet_buffer_parcellesCible$pourcent_filet_absent+compo_filet_buffer_parcellesCible$pourcent_filet_monoparcelle+
  compo_filet_buffer_parcellesCible$pourcent_filet_monorang+compo_filet_buffer_parcellesCible$pourcent_filet_non_deploye+compo_filet_buffer_parcellesCible$pourcent_filet_inconnu

## remplacer les NA par zero
compo_filet_buffer_parcellesCible<-compo_filet_buffer_parcellesCible%>% replace(is.na(.),0)


##################
####   FIGURES    
##################

# Faire une figure filets / pas filet
fig_filets <- compo_filet_buffer_parcellesCible %>%
  ggplot( aes(x=pourcent_vergers_filet_monorang)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%");fig_filets

# Double gradient
ggplot(compo_filet_buffer_parcellesCible, aes(x=pourcent_vergers, y=pourcent_filet_monorang)) + 
  geom_point(size=1) +
  xlim(0,60) + ylim(0,50)

################################################################################
#############           %  AB dans les buffers               ###################
################################################################################

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
vergers_parcellesCible_final0 <- merge(compo_filet_buffer_parcellesCible, aires_buffer_parcellesCible_bio, by="id_plot", all.x=T)
#### remplacer les NA par zero (recup les parcelles sans AB dans l buffer)
vergers_parcellesCible_final0[is.na(vergers_parcellesCible_final0)]<-0

### Le dataset complet
vergers_parcellesCible_final<-vergers_parcellesCible_final0 %>% 
  select(c("id_plot", "pourcent_vergers_filet_absent", "pourcent_vergers_filet_monoparcelle", "pourcent_vergers_filet_monorang", "pourcent_vergers_filet_non_deploye",
           "pourcent_filet_absent", "pourcent_filet_monoparcelle", "pourcent_filet_monorang", "pourcent_filet_non_deploye", "pourcent_filet_inconnu", "pourcent_vergers",
           "percent_verger_bio"))


##################
####   FIGURES    
##################

## Faire une figure bio/pas bio
pbio <- vergers_parcellesCible_final %>%
  ggplot( aes(x=percent_verger_bio)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 5%"); pbio


################################################################################
#############         Lineaire Haies dans les buffers        ###################
################################################################################

# intersection Haies avec buffers
inter_haies_buffer_parcellesCible<-st_intersection(combined_haies, buffer_parcellesCible) 
plot(inter_haies_buffer_parcellesCible[,2])

## new column avec longueur
inter_haies_buffer_parcellesCible$longueur<-st_length(inter_haies_buffer_parcellesCible$geometry)

#### Calcul lineaire par ID (ID = chaque buffer) + transfo en "dataframe"
## on enleve la geometrie avant
st_geometry(inter_haies_buffer_parcellesCible) <- NULL
## pivot pour avoir longueur haies de chaque buffer
inter_haies_buffer_parcellesCible<-inter_haies_buffer_parcellesCible %>% 
  group_by(id_plot) %>%
  summarise(nb_haies = n(), length_haies=sum(longueur))

## On merge avec le tableau precedent
vergers_parcellesCible_final_haies <- merge(vergers_parcellesCible_final, inter_haies_buffer_parcellesCible, by="id_plot", all.x=T)

##################
####   FIGURES    
##################

# histogramme
hist(inter_haies_buffer_parcellesCible$length_haies)



################################################################################
################################################################################
#############           Compilation fichier global           ###################
################################################################################
################################################################################

## combinerfichier des % OCS avec celui des vergers/filets + haies
Compo_paysage_buffer_vergers_cible <- merge(compo_OCS_buffer_parcelles_Cible_final, vergers_parcellesCible_final_haies, by="id_plot", all.x=T)

## on finalise le fichier
colnames(Compo_paysage_buffer_vergers_cible) <- paste(colnames(Compo_paysage_buffer_vergers_cible),paste("buffer",taille_du_buffer, sep="_"),sep="_")
names(Compo_paysage_buffer_vergers_cible)[1] <- "id_plot"


### Pour avoir toutes les infos, possible de combiner ce ficher avec "info parcelle" d'une année donnée
## Pour une meme parcelle les files info parcelles contiennent generalement la meme info (sauf exception voir avec Xavier)

##################
####   EXEMPLE    
##################

## info parcelles 22
setwd("P:/CBC/COL_SITE_ATELIER_BVD/DONNEES_TERRAIN_2022")
data_parcelle_2022<-read.csv("INFO_PARCELLES_2022.csv", header=T, sep=";", na.strings=c(""))

Compo_paysage_buffer_vergers_cible_IP_2022 <- merge(Compo_paysage_buffer_vergers_cible, data_parcelle_2022, by.x="id_plot", by.y="id_parcelle", all.x=T, all.y=T)






