library(sf)
library(dplyr)
library(units)


# Question 1
france_metro <- st_read("fonds/commune_francemetro_2021.gpkg")
# le fichier contient des pultipolygones, en lambert 93

# Question 2
summary(france_metro)
# On obtient les informations sur le type de variables

# Question 3
View(france_metro)

# Question 4
st_crs(france_metro)

# Question 5
communes_Bretagne <- subset(france_metro, reg == '53')
communes_Bretagne <- subset(communes_Bretagne, select=c(code, libelle, epc, dep, surf))

# Question 6
str(communes_Bretagne)

# Question 7
plot(communes_Bretagne)

# Question 8
plot(st_geometry(communes_Bretagne))

# Question 9
communes_Bretagne <- transform( communes_Bretagne, surf2 = st_area(geom)) # Unité en m²

# Question 10
communes_Bretagne <- communes_Bretagne %>% mutate(surf2 = set_units(surf2, km*km))

# Question 11
# Les colonnes surf et surf2 ne sont pas égales mais très proches.
# surf2 est calculé à partir des polygones mais ils sont simplifiés pour les stocker donc moins fiable

# Question 12
dep_Bretagne <- communes_Bretagne %>% 
  group_by(dep) %>% 
  summarise(surf = sum(surf))
dep_Bretagne
plot(dep_Bretagne)

# Question 13
dep_Bretagne2 <- communes_Bretagne %>%
  group_by(dep) %>%
  st_union()

Constituer cette fois un fond départemental en utilisant les fonctions summarise() et st_union(). A
la différence de la table précedemment créée, le fond ne contiendra que le code dept et la geometry
(aucune variable numérique ne sera utilisée). Faire ensuite un plot de votre table pour vérifier que les
geometry ont bien été regroupés par département.

help(st_union)
Creates multiple
geometries into a a single geometry, consisiting of
all geometry elements




  
  
  
  
  
  
  
  
  
  
  
  








