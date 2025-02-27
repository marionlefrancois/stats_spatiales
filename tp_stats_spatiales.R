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
  summarize(st_union(geom))

plot(dep_Bretagne2)

# Question 14
# a)
centroid_dept_bretagne <- st_centroid(dep_Bretagne)
str(centroid_dept_bretagne)
#centroid est de type point

# b)
plot(st_geometry(dep_Bretagne))
plot(st_geometry(centroid_dept_bretagne), add = TRUE)

# c)
dep <- as.character(c(35, 56, 29, 22))
nom_dep <- c("Ille-et-vilaine", "Morbihan", "Finistère", "Cotes d'Armor")
dep_lib <- data.frame(dep,nom_dep)


centroid_dept_bretagne <- centroid_dept_bretagne %>%
  left_join(dep_lib, 
            by = "dep")
str(centroid_dept_bretagne)  

# d)
centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords %>% str()

centroid_coords <- centroid_coords %>%
  bind_cols(
    centroid_dept_bretagne %>%
      st_drop_geometry()
  )

centroid_coords %>% str()

# e)
text(
  x=centroid_coords$X,
  y=centroid_coords$Y,
  labels = centroid_coords$nom_dep,
  pos=3,
  cex=0.8,
  col = "coral"
)

# Question 15
centroid_dans_communes <- st_intersects(communes_Bretagne, centroid_dept_bretagne)
centroid_dans_communes <- st_intersection(communes_Bretagne, centroid_dept_bretagne)
str(centroid_dans_communes)

# Question 16
centroid_dans_communes2 <- st_within(centroid_dept_bretagne,communes_Bretagne)

# Question 17
distance <- st_distance(centroid_dept_bretagne,
                        communes_Bretagne %>% 
                          filter(libelle %in% c("Saint-Brieuc", "Quimper", "Rennes", "Vannes")))

rownames(distance) <- centroid_dept_bretagne$dep
colnames(distance) <- c("Saint-Brieuc", "Quimper", "Rennes", "Vannes")  
distance  
  
# Question 18


Quelles sont les communes à moins de 20 km (à vol d’oiseau) de chaque centroïde
  
  
  
  








