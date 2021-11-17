# Set working directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(corrplot)
library(cowplot)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(raster)

cinema <- read.table(file="../data/cinema.csv", header=TRUE, sep=",", encoding="UTF-8")

# Récupération des données carto graphique des régions de France
FranceFormes <- getData(name="GADM", country="FRA", level=1)
# plot(FranceFormes, main="Carte des régions de France")

# Vérifier que les 2 listes de régions concordent
all.equal(
  str_sort(
    FranceFormes$NAME_1
  ),
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


# Calcul nombre d'établissement par Région (sans IDF)
nbEtablissementParRegionSansIDF <- cinema %>%
  dplyr::select(NomRegion, NbEtablissement) %>%
  group_by(NomRegion) %>%
  summarise(NbEtablissement = sum (NbEtablissement)) %>% 
  subset(NomRegion != "Île-de-France")

# Affichage
idxRegion <- match(FranceFormes$NAME_1, nbEtablissementParRegionSansIDF$NomRegion)
FranceFormes$NbEtablissement <- nbEtablissementParRegionSansIDF[idxRegion, "NbEtablissement"]
couleurs <- colorRampPalette(c('white', 'yellow'))
spplot(FranceFormes, "NbEtablissement", col.regions=couleurs(30),  main=list(label="Nombre de cinéma par région (sans l'Île-de-France)",cex=.8))
  

# Calcul nombre de cinéma par habitant par région
nbEtablissementParHabitantParRegion <- cinema %>%
  dplyr::select(NomRegion, NbEtablissement, Population) %>%
  group_by(NomRegion) %>%
  summarise(NbEtablissement = sum(NbEtablissement), Population = sum(Population)) %>% 
  mutate(NbEtablissementParHabitant = NbEtablissement / Population)

nbEtablissementParHabitantParRegion$Population <- NULL
nbEtablissementParHabitantParRegion$NbEtablissement <- NULL

# Affichage
idxRegion <- match(FranceFormes$NAME_1, nbEtablissementParHabitantParRegion$NomRegion)
FranceFormes$NbEtablissementParHabitant <- nbEtablissementParHabitantParRegion[idxRegion, "NbEtablissementParHabitant"]
couleurs <- colorRampPalette(c('white', 'blue'))
spplot(FranceFormes, "NbEtablissementParHabitant", col.regions=couleurs(30),  main=list(label="Nombre de cinéma pour 1 million d'habitant",cex=.8))

# Total des recettes en France
sum(cinema$Recettes)

# créer cinéma par région
cinema_par_region <- cinema %>% 
  dplyr::select(NomRegion, Population, NbEntree, Recettes, NbSeance, NbCommuneEquipee, NbEtablissement, NbSalle, NbFauteuil, NBArtEtEssai, NbMultiplex) %>% 
  group_by(NomRegion) %>% 
  summarise(Population = sum(Population),
            NbEntree = sum(NbEntree),
            Recettes = sum(Recettes),
            NbSeance = sum(NbSeance),
            NbCommuneEquipee = sum(NbCommuneEquipee),
            NbEtablissement = sum(NbEtablissement),
            NbSalle = sum(NbSalle),
            NbFauteuil = sum(NbFauteuil),
            NBArtEtEssai = sum(NBArtEtEssai),
            NbMultiplex = sum(NbMultiplex),
            )

# Nombre d'entrées par habitant
nbEntreeParHabitantParRegion <- cinema_par_region %>% 
  dplyr::select(NomRegion, Population, NbEntree) %>% 
  mutate(NbEntreeParPersonnes = NbEntree / Population)

nbEntreeParHabitantParRegion$Population <- NULL
nbEntreeParHabitantParRegion$NbEntree <- NULL

# Affichage
idxRegion <- match(FranceFormes$NAME_1, nbEntreeParHabitantParRegion$NomRegion)
FranceFormes$NbEntreeParPersonnes <- nbEntreeParHabitantParRegion[idxRegion, "NbEntreeParPersonnes"]
couleurs <- colorRampPalette(c('white', 'brown'))
spplot(FranceFormes, "NbEntreeParPersonnes", col.regions=couleurs(30),  main=list(label="Nombre d'entrées par habitant",cex=.8))


nbEntreeParHabitantParRegion %>% 
  arrange(-NbEntreeParPersonnes)

cinema_par_region%>% 
  dplyr::select(NomRegion, Recettes) %>% 
  arrange(-Recettes)

cinema_par_region%>% 
  dplyr::select(NomRegion, NbEtablissement) %>% 
  arrange(-NbEtablissement)


repartition_recettes <- cinema_par_region %>% 
  dplyr::select(NomRegion, Recettes) %>% 
  rename(Region = NomRegion) %>% 
  mutate(PartRecettes = Recettes / sum(Recettes)) %>% 
  mutate(Region = paste(Region, "(", round(PartRecettes*100, 1), "%)")) %>% 
  arrange(-PartRecettes)

cinema_par_region_sans_IDF_Corse <- cinema_par_region %>% 
  subset(NomRegion != "Île-de-France") %>% 
  subset(NomRegion != "Corse")

cinema_par_region_sans_IDF <- cinema_par_region %>% 
  subset(NomRegion != "Île-de-France")

# Pop moyenne province
pop_moyenne_province <- mean(cinema_par_region_sans_IDF_Corse$Population)*1000000

pop_corse = 0.261*1000000

pop_moyenne_province / pop_corse
