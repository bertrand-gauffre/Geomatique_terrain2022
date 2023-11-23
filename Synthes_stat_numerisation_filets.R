

########## Charger les librairies
.libPaths("C:/BERTRAND/OUTILS/R/win-library/4.2", include.site = TRUE)
library(st)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)


################################################################################
#############      La couche de numerisation des filets     ####################
################################################################################

### On charge la couche numérisation filets (necessaire tant que verif pas terminee)
setwd("P:/SIG/Chantier_Numerisation_Filets/DATA/")
verif_vergers <- st_read("230215_numerisation_filets.shp")
verif_vergers[which(verif_vergers$filet == "non"),"type_filet"]<-"absent"
verif_vergers$aire_plot<-st_area(verif_vergers$geometry)



### Statistiques filets
nb_ech_par<-verif_vergers %>%
  group_by(type_filet, Code_n3) %>%
  summarise(nbre=n(), aire_tot=sum(aire_plot))

st_geometry(nb_ech_par) <- NULL

nb_orchard_by_type<-nb_ech_par%>%
  select(Code_n3, type_filet, nbre) %>%
  pivot_wider(names_from = type_filet, values_from = nbre)


surface_orchard_by_type<-nb_ech_par%>%
  select(Code_n3, type_filet, aire_tot) %>%
  pivot_wider(names_from = type_filet, values_from = aire_tot)



