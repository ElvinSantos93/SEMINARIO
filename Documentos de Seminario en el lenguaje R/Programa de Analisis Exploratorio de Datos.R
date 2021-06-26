#############Informacion de la base de datos (USairpollution)##########
#La base de datos USairpollution se encuentra en el libro de Brian Everitt, "An Introduction to Applied Multivariante Analysis Whit R" paginas 10 y 11#
#Tambien se puede extraer en R, utilizando el paquete "MVA" para extraerla#



#Instalando los paquetes que se van a utilizar para el analisis exploratorio de datos#
install.packages("MVA")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("Hmisc")
install.packages("GGally")
install.packages("gmodels")
install.packages("corrplot")
install.packages("PerformanceAnalytics")

#Cargando los paquetes que se van a utilizar para el analisis exploratorio de datos#
library(MVA)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(GGally)
library(gmodels)
library(corrplot)
library(PerformanceAnalytics)

#Cargando la base de datos "USairpollution"#
Datos <- USairpollution

#Informacion sobre la estructura de la base de datos (# de filas y columnas y el tipo de dato)#
glimpse(Datos)

###Exploracion del contenido de la base de datos###
##########Resumen de la base de datos o dataset##########
summary(Datos)

##########Primeras 6 filas de la base de datos##########
head(Datos)

##########Ultimas 6 filas de la base de datos##########
tail(Datos)

##########frecuencia de valores de la variable SO2##########
table(Datos$SO2)

#Obtenemos el histograma para ver como estan distribuidos los datos de la variable SO2#
hist(Datos$SO2)

#Hacemos un grafico de caja y bigotes para ver los outliers#
boxplot(Datos, horizontal = TRUE)

######################################################################################
##########Haciendo una limpieza de la base de datos eliminando los outliers##########
###Obtenemos la lista de los valores que aparecen como outliers de cada variable###
boxplot.stats(Datos$SO2)
boxplot.stats(Datos$temp)
boxplot.stats(Datos$manu)
boxplot.stats(Datos$popul)
boxplot.stats(Datos$wind)
boxplot.stats(Datos$precip)
boxplot.stats(Datos$predays)

###Obtenemos la lista de valores que no son outliers de la variable SO2###
Datos_sin_out <- Datos$SO2[Datos$SO2 < 60]

###Volvemos a hacer el grafico de caja y bigotes de la variable SO2###
boxplot(Datos_sin_out, horizontal = TRUE)

###Obtenemos la lista de valores que no son outliers de la variable temp###
Datos_sin_out1 <- Datos$temp[Datos$temp < 75]

###Volvemos a hacer el grafico de caja y bigotes de la variable temp###
boxplot(Datos_sin_out1, horizontal = TRUE)

###Obtenemos la lista de valores que no son outliers de la variable manu###
Datos_sin_out2 <- Datos$manu[Datos$manu < 1000]

###Volvemos a hacer el grafico de caja y bigotes de la variable manu###
boxplot(Datos_sin_out2, horizontal = TRUE)

###Obtenemos la lista de valores que no son outliers de la variable popul###
Datos_sin_out3 <- Datos$popul[Datos$popul < 1500]

###Volvemos a hacer el grafico de caja y bigotes de la variable popul###
boxplot(Datos_sin_out3, horizontal = TRUE)

###Volvemos a obtener la lista de valores de los outliers de la variable popul###
boxplot.stats(Datos_sin_out3)

###Nos volvemos a quedar con los valores que no son autliers de la variable popul###
Datos_sin_out4 <- Datos$popul[Datos$popul < 1200]

###Volvemos a hacer el grafico de caja y bigotes de la variable popul###
boxplot(Datos_sin_out4, horizontal = TRUE)

###Obtenemos la lista de valores que no son outliers de la variable precip###
Datos_sin_out5 <- Datos$precip[Datos$precip < 7.00]

###Volvemos a hacer el grafico de caja y bigotes de la variable precip###
boxplot(Datos_sin_out5, horizontal = TRUE)

###Obtenemos la lista de valores que no son outliers de la variable predays###
Datos_sin_out6 <- Datos$predays[Datos$predays < 30]

###Volvemos a hacer el grafico de caja y bigotes de la variable predays###
boxplot(Datos_sin_out6, horizontal = TRUE)

######################################################################################

#Ejemplo del calculo de la media o promedio#
vec <- rnorm(10, 20, 10)
mean(vec)

vec.ruid <- c(1, 80, 25)
mean(vec.ruid)

#Ejemplo del calculo de la mediana, tomando el ejemplo anterior#
median(vec)

median(vec.ruid)

#Calculando el rango de la variable SO2#
max(Datos$SO2) - min(Datos$SO2)

#El minimo, los tres cuartiles y el maximo#
quantile(Datos$SO2, seq(0,1,0.25))

#Calculando los estadisticos descriptivos de cada variable#
lapply(Datos [,1:7], mean)
lapply(Datos [,1:7], median)
lapply(Datos [,1:7], range)
lapply(Datos [,1:7], quantile)

#Calculando El minimo, los tres cuartiles y el maximo de cada variable#
summary(Datos$SO2)
summary(Datos$temp)
summary(Datos$manu)
summary(Datos$popul)
summary(Datos$wind)
summary(Datos$precip)
summary(Datos$predays)

#Panorama completo de las variable#
describe(Datos)

#Calculando el coeficiente de correlacion de todo el conjunto de datos#
cor(Datos[, 1:7])

#Calculando el coeficiente de correlacion de la variable manu con las variable popul#
cor(Datos$manu, Datos$popul)

#Mirando si la relacion en la variable manu con la variable popul es estadisticamente significativa, utilizando el valor de p#
cor.test(Datos$manu, Datos$popul)

#Calculando la mariz de correlacion#
round(cor(Datos), 2)

#Calculando la mariz de correlacion para ver si es estadisticamente significativo utilizando el valor de p#
rcorr(as.matrix(Datos))

#Calculando la mariz de correlacion de forma grafica#
correlacion<-round(cor(Datos), 1)

corrplot(correlacion, method="number", type="upper")

#Grafico de dispersion de la matriz de correlacion#
chart.Correlation(Datos, histogram = F, pch = 19)

