# Set working directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(corrplot)
library(cowplot)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(raster)

cinema <- read.table(file="../data/cinema.csv", header=TRUE, sep=",")

# Récupération des données carto graphique des régions de France
FranceFormes <- getData(name="GADM", country="FRA", level=1)
# plot(FranceFormes, main="Carte des régions de France")

# Vérifier que les 2 listes de régions concordent
all.equal(str_sort(
  FranceFormes$NAME_1),
  str_sort(cinema$NomRegion %>% unique()
  )
)

# Calcul nombre d'établissement par Région
nbEtablissementParRegion <- cinema %>%
  dplyr::select(NomRegion, NbEtablissement) %>%
  group_by(NomRegion) %>% summarise(NbEtablissement = sum (NbEtablissement))

# Affichage
idxRegion <- match(FranceFormes$NAME_1, nbEtablissementParRegion$NomRegion)
FranceFormes$NbEtablissement <- nbEtablissementParRegion[idxRegion, "NbEtablissement"]
couleurs <- colorRampPalette(c('white', 'yellow'))
spplot(FranceFormes, "NbEtablissement", col.regions=couleurs(30),  main=list(label="Nombre de cinéma",cex=.8))

# Calcul nombre de cinéma par habitant par région
nbEtablissementParHabitantParRegion <- cinema %>%
  dplyr::select(NomRegion, NbEtablissement, Population) %>%
  group_by(NomRegion) %>%
  summarise(NbEtablissement = sum (NbEtablissement), Population = sum (Population)) %>% 
  mutate(NbEtablissementParHabitant= NbEtablissement/ (Population*1000000))

# Affichage
idxRegion <- match(FranceFormes$NAME_1, nbEtablissementParHabitantParRegion$NomRegion)
FranceFormes$NbEtablissementParHabitant <- nbEtablissementParHabitantParRegion[idx, "NbEtablissementParHabitant"]
couleurs <- colorRampPalette(c('white', 'blue'))
spplot(FranceFormes, "NbEtablissementParHabitant", col.regions=couleurs(30),  main=list(label="Nombre de cinéma par habitant",cex=.8))
