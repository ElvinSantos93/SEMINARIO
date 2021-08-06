#==============================================================================================
############### Implementacion del Metodo de K-Medias y Agrupamiento Jerarquico ###############
#==============================================================================================

#===============================================================================================
# Instalando los paquetes requeridos para la implementacion de los metodos de aprendizaje no supervisados
install.packages("MVA")
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
install.packages("fpc")
install.packages("dendextend")
install.packages("circlize")
install.packages("gplots")
#===============================================================================================


#===============================================================================================
# Cargando los paquetes requeridos para la implementacion de los metodos de aprendizaje no supervisados
library(MVA)
library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
library(fpc)
library(dendextend)
library(circlize)
library(colorspace)
library(gplots)
#==================================================================================================


#==================================================================================================
##### Cargando la base de datos "USairpollution" #####
Datos_original <- USairpollution
view(Datos_original)
Datos_original

##### Deteccion de valores ausentes (NA) #####
which(is.na(Datos_original))
#==================================================================================================


#==================================================================================================
########## Normalizacion: Estandarizacion de las observaciones ##########
scale_datos <- scale(Datos_original, center = TRUE, scale = TRUE)
view(scale_datos) # Visualizando la base de datos normalizada, con media 0 y varianza 1
head(scale_datos)
summary(scale_datos) # Informacion general de la base de datos normalizada
#==================================================================================================


        ########################################################################
        #          IMPLEMENTANDO EL METODO DE AGRUPAMIENTO JERARQUICO          #
        ######################################################################## 

#==================================================================================================
########## Calculando la matriz de distancias ##########
matrix_distancia <- dist(scale_datos, method = "euclidean")
matrix_distancia
fviz_dist(matrix_distancia, gradient = list(low = "white", mid = "blue", high = "red"))
#==================================================================================================


#==================================================================================================
######## Agrupamiento Jerarquico, utilizando la funcion "hclust" con Vinculacion Completa ########
hclust1 <- hclust(matrix_distancia, method = "complete")

########## Grafica del dendograma obtenido ##########
plot(hclust1, cex = 0.9, hang = -1)
#==================================================================================================


#==================================================================================================
########## Agrupamiento Jerarquico con la funcion "agnes", con Vinculacion Completa ##########
hclust2 <- agnes(scale_datos, method = "complete")
hclust2$ac # Coeficiente de aglomeracion de la funcion "agnes"
#==================================================================================================


########## Comparacion de los metodos de Vinculacion de Agrupamiento Jerarquico ##########
###################################################################################################
#==================================================================================================
##### Vector de los Metodos de Comparacion #####
compar_method <- c("average", "single", "complete", "ward")
names(compar_method) <- c("average", "single", "complete", "ward")

##### Funcion para calcular los coeficiente de cada metodo #####
coeficientes <- function(x) {
  agnes(scale_datos, method = x)$ac
}
map_dbl(compar_method, coeficientes)
#==================================================================================================


#==================================================================================================
########## Grafica del dendograma obtenido por el metodo "ward" ya que fue el ########## 
######### metodo que dio el coeficiente de aglomeracion mas alto ##########
hclust3 <- agnes(scale_datos, method = "ward")
pltree(hclust3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

########## Grafico del dendograma de los cuatro grupos ##########
dend <- as.dendrogram(hclust3)
dend<-rotate(dend, 1:41)
dend <- color_branches(dend, h = 4)
plot(dend)
#==================================================================================================


#==================================================================================================
########## Grafico del dendograma de los cuatro grupos en forma circular ##########
par(mar = rep(0,4))
circlize_dendrogram(dend)
#==================================================================================================



        ########################################################################
        #               IMPLEMENTANDO EL METODO DE K-MEDIAS                    #
        ########################################################################  


########## Estimando el  número de clústers (grupos), utilizando varios metodos ##########
#================================================================================================
############### Metodo del Codo ###############
fviz_nbclust(scale_datos, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method") #+ geom_vline(xintercept = 4, linetype = 2)
#==================================================================================================

#==================================================================================================
############### Metodo de la Silueta Promedio ###############
fviz_nbclust(scale_datos, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")
#==================================================================================================

#==================================================================================================
############### Metodo de Estadistica de Brecha ###############
set.seed(123)
fviz_nbclust(scale_datos, kmeans, nstart = 25, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap Statistic Method")
#==================================================================================================

#==================================================================================================
##### Aplicando varios metodos para identificar cuantos son los clusteres adecuados para #####
##### implementar el metodo de k-medias #####

#"kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex",
# "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap",
#"frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" 

set.seed(123)
num.cluster <- NbClust(scale_datos, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "alllong")
fviz_nbclust(num.cluster)
#==================================================================================================


#==================================================================================================
########## Calculando los cuatro clústers (grupos) ##########
k <- kmeans(scale_datos, centers = 4, nstart = 25)
k # Indica en que cluster se encuentran cada una de las ciudades y la media de cada cluster
str(k) # Un poco mas de informacion 
k$cluster %>%
  table()
#==================================================================================================


#==================================================================================================
########## Visualizando de manera grafica los cuatro clústers (grupos) ##########
fviz_cluster(k, data = scale_datos) # Grafico clasico de los cuatro cluster 
fviz_cluster(k, data = scale_datos, ellipse.type = "euclid", repel = TRUE, star.plot = TRUE)# Grafico de elipse con la distancia euclideana
fviz_cluster(k, data = scale_datos, ellipse.type = "norm")
#==================================================================================================


########## Pasar los clústers (grupos) a Datos original, para trabajar con ellos ##########
###################################################################################################
#==================================================================================================
USairpollution %>%
  mutate (Cluster = k$cluster) %>%
  group_by (Cluster) %>%
  summarise_all ("mean") # Calcula las medias de los cluster

###################################################################################################
########## Agregando a la base de datos original la columna de los cluster ##########

Datos_original <- USairpollution # Base de datos original
Datos_original # Sobre escribiendo la base de datos original
Datos_original$cluster <- as.factor(k$cluster) # introduciendo el objeto k como factor en el data
Datos_original # Agrega la columna de los cluster de la variable k a la base de datos originales

###################################################################################################
########## Agregando a la base de datos normalizados la columna de los cluster ##########

Datos_original <- USairpollution # Cargando nuevamente la base de datos original
Datos_original <- scale(Datos_original)
Datos_original <- as.data.frame(Datos_original)
Datos_original$cluster <- as.factor(k$cluster)
Datos_original # Agrega los cluster de la variable k a la base de datos normalizada 

###################################################################################################

Datos_original$cluster<-factor(Datos_original$cluster) # variable que no va a cambiar
data_long <- gather(Datos_original, caracteristica, valor, SO2:predays, factor_key = TRUE)
data_long # cambiando la base de datos, donde en una columna llamada caracteristica van a  
          # estar las variables en otra los valores y por ultimo los clusters
#==================================================================================================


#==================================================================================================
########## Visualizacion grafica de los clústers (grupos)  ##########

ggplot(data_long, aes(as.factor(x = caracteristica), y = valor, group = cluster, colour = cluster)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1) +
  stat_summary(geom = "line") +
  geom_point(aes(shape = cluster))
#==================================================================================================
