########## ANALISIS DE COMPONENTES PRINCIPALES (PCA) ##########

#=============================================================================================================================================
install.packages("MVA")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("Hmisc")
install.packages("GGally")
install.packages("gmodels")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("ade4")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("textshape")
#=============================================================================================================================================

#=============================================================================================================================================
library(MVA)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(GGally)
library(gmodels)
library(corrplot)
library(PerformanceAnalytics)
library(ade4)
library(FactoMineR)
library(factoextra)
library(textshape)
#=============================================================================================================================================

#=============================================================================================================================================
#Visualizando la base de datos "USairpollution"#
Datos <- USairpollution
str(Datos)
View(Datos)

#Visualizacion de los datos de la matriz de correlacion#
cor(Datos)

#Deteccion de valores ausentes (NA)#
which(is.na(Datos)) #Donde se puede observar que no hay ningun valor ausente

#Estandarizacion de los Datos (o escalado de los datos)#
scale_datos <- scale(Datos, center = TRUE, scale = TRUE)

#Realizando el Analisis de Componentes Principales o PCA#
PCA <- prcomp(scale_datos)
names(PCA) #Las opciones de la funcion prcomp
PCA

#=============================================================================================================================================

#=============================================================================================================================================
#Visualizando la proporcion de varianza explicada por cada uno de los componentes principales#
#y a partir de ello, decidir con cuantos componentes nos vamos a quedar para el analisis#

summary(PCA)#Obtenemos las desviaciones estandar, la proporcion de varianza y la proporcion acumulada

#Calculando las varianza de cada uno de los componentes principales, donde primero calculamos la desviacion estandar#

desv_stand <- PCA[[1]] #Que nos de los primeros valores, que serian las desviaciones estandar
desv_stand #valor de la desviacion estandar

varianza <- desv_stand^2
varianza #Valor de la varianza

#Guardando los primeros tres componentes principales elegidos#
PC1 <- PCA[[2]][,1]
PC1 #Componente principal 1

PC2 <- PCA[[2]][,2]
PC2 #Componente principal 2

PC3 <- PCA[[2]][,3]
PC3 #Componente principal 3

#####Guardando los objetos y ordenandolos mediante columnas#####

#Gnerando la matriz factorial de los tres componentes principales#
comp_princ <- cbind(PC1, PC2, PC3)
comp_princ #Componentes principales en forma de matriz

#Generando las coordenadas de los componentes principales con todas las observaciones (ciudades)#
ciudades <- PCA$x[, 1:3]
ciudades #Muestra las coordenadas de las ciudades en el plano (es el producto de cada componente principal por cada una de las variables)

#####Informacion sobre las variables y las observaciones#####

#Extrae la informacion sobre las variables#
get_pca_var(PCA)

#Extrae informacion sobre las observaciones#
get_pca_ind(PCA)
#=============================================================================================================================================



#===============VISUALIZACION DE LOS RESULTADOS MEDIANTE DIFERENTES TIPOS DE GRAFICOS===============
#=============================================================================================================================================
#Grafico de barras de los eigenvalores ordenados de mayor a menor#
fviz_screeplot(PCA, addlabels = TRUE, ylim = c(0, 40)) #scree plot

#Grafico de la contribucion de las variables a la varianza explicada#
fviz_contrib(PCA, choice = "var")

#Grafico de la contribucion de todas las observaciones a la varianza explicada#
fviz_contrib(PCA, choice = "ind")

#Grafico de la contribucion de las primeras 20 observaciones a la varianza explicada, para una mejor visualizacion#
fviz_contrib(PCA, choice = "ind", axes = 1, top = 20)

#Grafico de correlacion de las variables#
fviz_pca_var(PCA,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE
            )
#=============================================================================================================================================

#==============================================================================================================================================
#####Graficando la correlacion entre los componentes principales y las variables originales#####
#####Y las graficas de las coordenadas de las observaciones en el plano#####

#Circulo de correlacion entre la componente principal 1 y la componente principal 2, para eso se omite la columna 3#
s.corcircle(comp_princ[,-3], sub = "PC1 y PC2", possub = "topright")
#Grafico de coordenadas de las observaciones en el plano#
s.label(ciudades[,-3], label = row.names(Datos), sub = "Coordenadas de las ciudades", possub = "topright")


#Circulo de correlacion entre la componente principal 1 y el componente principal 3, para eso se omite la columna 2#
s.corcircle(comp_princ[,-2], sub = "PC1 y PC3", possub = "topright")
#Grafico de coordenadas de las observaciones en el plano#
s.label(ciudades[,-2], label = row.names(Datos), sub = "Coordenadas de las ciudades", possub = "topright")


#Circulo de correlacion entre la componente principal 2 y el componente principal 3, para eso se omite la columna 1#
s.corcircle(comp_princ[,-1], sub = "PC2 y PC3", possub = "topright")
#Grafico de coordenadas de las observaciones en el plano#
s.label(ciudades[,-1], label = row.names(Datos), sub = "Coordenadas de las ciudades", possub = "topright")

#=============================================================================================================================================



#=============================================================================================================================================
########## OTRA FORMA DE VISUALIZAR LOS GRAFICOS DE LAS COMPONENTES PRINCIPALES ##########

#Representacion de las observaciones sobre las componentes principales
fviz_pca_ind(PCA,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE
             )

 
#Representacion conjunta sobre las dos primeras componentes y las observaciones#
fviz_pca_biplot(PCA, repel = FALSE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )

#Representacion conjunta sobre las dos primeras componentes y 20 observaciones#
fviz_pca_biplot(PCA, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969", # Individuals color
                select.ind = list(contrib = 20)
                )

#=============================================================================================================================================


