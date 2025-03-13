install.packages("dplyr")
library(dplyr)
install.packages("sf")
library(sf)
install.packages("spdep")
library(spdep)
install.packages("RColorBrewer")
library(RColorBrewer)


# Exercice 1

# Question 1
iris<-st_read("stats_spatiales/iris_franceentiere_2021/iris_franceentiere_2021.shp")
data<-read.csv2("stats_spatiales/data/BASE_TD_FILO_DISP_IRIS_2018.csv",sep=";",dec=".")

marseille<-iris %>% 
  filter(substr(depcom,1,3)=="132") %>% 
  left_join(
    data %>% select(code=IRIS,DISP_MED18),
    by="code"
  )

# Question 2
st_transform(marseille, crs = 2154)

# Question 3
summary(marseille$DISP_MED18)

# Question 4
boxplot(marseille$DISP_MED18~marseille$depcom)

marseille <- na.omit(marseille)
plot(marseille["DISP_MED18"], breaks = "jenks", nbreaks = 10)

# Question 5 a)
help(sample)
marseille$DISP_MED18_ALEA <- sample(marseille$DISP_MED18)

# Question 5 b)
plot(marseille["DISP_MED18_ALEA"], breaks = "jenks", nbreaks = 10, add = TRUE)

# Question 6 a)
# Autocorrélation positive : les revenue médians similaires regroupés

# Question 6 b)
voisins_marseille <- spdep::poly2nb(marseille)

summary(voisins_marseille)

# Question 6 c)
head(voisins_marseille)
#le quatrième élément a 4 voisins

# Question 7 a)
matrice_poids <- spdep::nb2listw(voisins_marseille, zero.policy=TRUE)
summary(matrice_poids)

# Question 7 b)
str(matrice_poids, max.level = 1)

# Question 7 c)
all(sapply(matrice_poids$weights, sum) == 1)

# Question 8 a)
marseille$DISP_MED18_STD <- scale(marseille$DISP_MED18)
plot(marseille$DISP_MED18_STD)
summary(marseille$DISP_MED18_STD)
var(marseille$DISP_MED18_STD)

# Question 8 b)
moran.plot(as.numeric(marseille$DISP_MED18_STD), listw = matrice_poids)

# Question 8 c)
# L-L  et H-H : beaucoup de valeurs, corrélation positive
# L-H et H-L : peu de valeurs, peu de dissimilarités

# Question 8 d)
# Oui, autocorrélation positive

# Question 9 a)
moran.test(marseille$DISP_MED18_STD, matrice_poids, randomisation = TRUE)

# Question 9) b)
# p-value très petite donc H0 rejetée : les voisins covarient d'une façon particulière. Hypothèse confirmée





