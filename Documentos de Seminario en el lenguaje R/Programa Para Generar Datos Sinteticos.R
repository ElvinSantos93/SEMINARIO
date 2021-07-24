################################# GENERANDO DATOS SINTETICOS ######################################

#==================================================================================================
############### Instalando los paquetes requeridos para generar datos sinteticos ###############
install.packages("MVA")
install.packages("synthpop")
install.packages("tidyverse")
install.packages("univariateML")
install.packages("cowplot")
install.packages("Rcpp")
#==================================================================================================


#==================================================================================================
############### Cargando los paquetes requeridos para generar datos sinteticos ###############
library(MVA)
library(synthpop)
library(tidyverse)
library(univariateML)
library(cowplot)
library(Rcpp)
#==================================================================================================

#==================================================================================================
                      ########## Seleccion de los datos ##########

Datos_original <- USairpollution
vars <- c("SO2" , "temp" , "manu" , "popul" , "wind" , "precip" , "predays")
Datos_original <- Datos_original[, vars]
head(Datos_original) # Visualizando las primeras 6 ciudades
which(is.na(Datos_original)) # Se puede observar que no hay valores ausentes   
#==================================================================================================


#==================================================================================================
                      ########## Sintesis predeterminada ##########

my.seed <- 1500
sds.default <- syn(Datos_original, seed = my.seed)
sds.default
names(sds.default)
#==================================================================================================


#==================================================================================================
                    ########## Generando los datos sinteticos ##########

Datos_sinteticos <- syn ( Datos_original, m = 2, seed  =  my.seed ) # Crea los datos sintéticos
Datos_sinteticos
#==================================================================================================


#==================================================================================================
############ Comparación visual de conjuntos de datos originales y sintéticos ############

compare(Datos_sinteticos, Datos_original)
compare(Datos_sinteticos, Datos_original, nrow = 3, ncol = 4, cols = c("#62B6CB", "#1B4965"))$plot
#==================================================================================================


### Construyendo modelos de regresión lineal a partir de nuestro conjunto de datos sinteticos y
### comparar dichos modelos con los modelos del conjunto de datos original
##################################################################################################
#==================================================================================================
#======== Modelo de regresion de la variable de interes (SO2) con las variables temp y manu =======
############## Prueba del efecto principal del grupo para confirmar la equivalencia ##############
full_SO2 = lm( SO2  ~  1  + temp + manu, data  =  Datos_original ) 
null_SO2 = lm( SO2  ~  1  + temp + manu + temp:manu, data  =  Datos_original )#
summary(null_SO2)
anova(null_SO2, full_SO2)

########### Modelo a partir de datos observados y modelo a partir de datos sinteticos ###########
model_orig <- lm(SO2  ~  1  + temp + manu + temp:manu, data  =  Datos_original)# Modelo a partir de datos observados
model_orig_sum <- summary(model_orig)

model_syn <- lm.synds(SO2  ~  1  + temp + manu + temp:manu, data  =  Datos_sinteticos)# Modelo a partir de datos sinteticos
model_syn_sum <- summary(model_syn)

############ Comparacion de los dos modelos ############
compare(model_syn, Datos_original)# Comparacion de los modelos

############ Estimaciones de los coeficientes de una sola sintesis ############
lm.synds(formula = SO2  ~  1  + temp + manu + temp:manu, data  =  Datos_sinteticos)
#==================================================================================================



#==================================================================================================
#========== Modelo de regresion de la variable de interes con las variables popul y wind ==========
############## Prueba del efecto principal del grupo para confirmar la equivalencia ##############
full_SO2_n1 = lm( SO2  ~  1  + popul + wind, data  =  Datos_original )
null_SO2_n1 = lm( SO2  ~  1  + popul + wind + popul:wind, data  =  Datos_original )
summary(null_SO2_n1)
anova(null_SO2_n1, full_SO2_n1)

########### Modelo a partir de datos observados y modelo a partir de datos sinteticos ###########
model_orig_m1 <- lm(SO2  ~  1  + popul + wind + popul:wind, data  =  Datos_original)# Modelo a partir de datos observados
model_orig_sum_m1 <- summary(model_orig_m1)

model_syn_s1 <- lm.synds(SO2  ~  1  + popul + wind + popul:wind, data  =  Datos_sinteticos)# Modelo a partir de datos sinteticos
model_syn_sum_s1 <- summary(model_syn_s1)

############ Comparacion de los dos modelos ############
compare(model_syn_s1, Datos_original)# Comparacion de los modelos

############ Estimaciones de los coeficientes de una sola sintesis ############
lm.synds(formula = SO2  ~  1  + popul + wind + popul:wind, data  =  Datos_sinteticos)
#==================================================================================================


#==================================================================================================
#========== Modelo de regresion de la variable de interes con las variables precip y predays ==========
############## Prueba del efecto principal del grupo para confirmar la equivalencia ##############
full_SO2_n2 = lm( SO2  ~  1  + precip + predays, data  =  Datos_original )
null_SO2_n2 = lm( SO2  ~  1  + precip + predays + precip:predays, data  =  Datos_original )
summary(null_SO2_n2)
anova(null_SO2_n2, full_SO2_n2)

########### Modelo a partir de datos observados y modelo a partir de datos sinteticos ###########
model_orig_m2 <- lm(SO2  ~  1  + precip + predays + precip:predays, data  =  Datos_original)# Modelo a partir de datos observados
model_orig_sum_m2 <- summary(model_orig_m2)

model_syn_s2 <- lm.synds(SO2  ~  1  + precip + predays + precip:predays, data  =  Datos_sinteticos)# Modelo a partir de datos sinteticos
model_syn_sum_s2 <- summary(model_syn_s2)

############ Comparacion de los dos modelos ############
compare(model_syn_s2, Datos_original)# Comparacion de los modelos

############ Estimaciones de los coeficientes de una sola sintesis ############
lm.synds(formula = SO2  ~  1  + precip + predays + precip:predays, data  =  Datos_sinteticos)
#==================================================================================================


#==================================================================================================
#========== Modelo de regresion de la variable de interes con las demas variables ==========
############## Prueba del efecto principal del grupo para confirmar la equivalencia ##############
full_SO2_n3 = lm( SO2  ~  1  + temp + manu + popul + wind + precip + predays, data  =  Datos_original )
null_SO2_n3 = lm( SO2  ~  1  + temp + manu + popul + wind + precip + predays , data  =  Datos_original )
summary(null_SO2_n3)
anova(null_SO2_n3, full_SO2_n3)

########### Modelo a partir de datos observados y modelo a partir de datos sinteticos ###########
model_orig_m3 <- lm(SO2  ~  1  + temp + manu + popul + wind + precip + predays , data  =  Datos_original)# Modelo a partir de datos observados
model_orig_sum_m3 <- summary(model_orig_m3)

model_syn_s3 <- lm.synds(SO2  ~  1  + temp + manu + popul + wind + precip + predays , data  =  Datos_sinteticos)# Modelo a partir de datos sinteticos
model_syn_sum_s3 <- summary(model_syn_s3)

############ Comparacion de los dos modelos ############
compare(model_syn_s3, Datos_original)# Comparacion de los modelos

############ Estimaciones de los coeficientes de una sola sintesis ############
lm.synds(formula = SO2  ~  1  + temp + manu + popul + wind + precip + predays , data  =  Datos_sinteticos)
#==================================================================================================



                    ############################################################
                    #                AJUSTE DE DISTRIBUCIONES                  #
                    ############################################################                                                         
#==================================================================================================
######### COMPARACION DE DISTRIBUCIONES, UTILIZANDO LAS METRICAS de AJUSTE "AIC" y "BIC" ##########
###################################################################################################

#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "SO2" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$SO2),
  mlexp(Datos_original$SO2),
  mlinvgamma(Datos_original$SO2),
  mlgamma(Datos_original$SO2),
  mllnorm(Datos_original$SO2),
  mlrayleigh(Datos_original$SO2),
  mlinvgauss(Datos_original$SO2),
  mlweibull(Datos_original$SO2),
  mlinvweibull(Datos_original$SO2),
  mllgamma(Datos_original$SO2)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "SO2" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$SO2),
  mlexp(Datos_original$SO2),
  mlinvgamma(Datos_original$SO2),
  mlgamma(Datos_original$SO2),
  mllnorm(Datos_original$SO2),
  mlrayleigh(Datos_original$SO2),
  mlinvgauss(Datos_original$SO2),
  mlweibull(Datos_original$SO2),
  mlinvweibull(Datos_original$SO2),
  mllgamma(Datos_original$SO2)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$SO2,
     main = "Distribución del contenido de SO2 en el Aire",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mllgamma(Datos_original$SO2), lwd = 2, lty = 1, col = "blue")
lines(mlinvgauss(Datos_original$SO2), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("lgamma", "invgauss"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$SO2)

##################################################################################################
ggplot(data = Datos_original) +
         geom_histogram(aes(x = SO2, y =  after_stat(density)),
                        bins = 40,
                        alpha = 0.3, color = "black") +
         geom_rug(aes(x = SO2)) +
         stat_function(fun = function(.x){dml(x = .x, obj = mllgamma(Datos_original$SO2))},
                       aes(color = "log-gamma"),
                       size = 1) +
         stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(Datos_original$SO2))},
                       aes(color = "inverse-gaussian"),
                       size = 1) +
         scale_color_manual(breaks = c("log-gamma", "inverse-gaussian"),
                            values = c("log-gamma" = "red", "inverse-gaussian" = "blue")) +
         labs(title = "Distribución del contenido de SO2 en el Aire",
              color = "Distribución") +
         theme_bw() +
         theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion log-gamma a los datos de SO2
distribucion <- mllgamma(x = Datos_original$SO2)
summary(distribucion)

# Se ajusta una distribucion inverse-gaussian a los datos de SO2
distribucion1 <- mlinvgauss(x = Datos_original$SO2)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de SO2 de acorde a la distribucion ajustada ############

# Muestreo de nuevos valores de la distribucion ajustada log-gamma
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de nuevos valores de la distribucion ajustada inverse-gaussian
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



###################################################################################################
#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "SO2" de los Datos Sinteticos, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlexp(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvgamma(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlgamma(Datos_sinteticos[["syn"]][[1]]$SO2),
  mllnorm(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlrayleigh(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvgauss(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlweibull(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvweibull(Datos_sinteticos[["syn"]][[1]]$SO2),
  mllgamma(Datos_sinteticos[["syn"]][[1]]$SO2)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "SO2" de los Datos Sinteticos, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlexp(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvgamma(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlgamma(Datos_sinteticos[["syn"]][[1]]$SO2),
  mllnorm(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlrayleigh(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvgauss(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlweibull(Datos_sinteticos[["syn"]][[1]]$SO2),
  mlinvweibull(Datos_sinteticos[["syn"]][[1]]$SO2),
  mllgamma(Datos_sinteticos[["syn"]][[1]]$SO2)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS SINTETICOS ##
#==================================================================================================
hist(Datos_sinteticos[["syn"]][[1]]$SO2,
     main = "Distribución del contenido de SO2 en el Aire",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mlinvgauss(Datos_sinteticos[["syn"]][[1]]$SO2), lwd = 2, lty = 1, col = "blue")
lines(mlgamma(Datos_sinteticos[["syn"]][[1]]$SO2), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("invgauss", "lgamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_sinteticos[["syn"]][[1]]$SO2)

##################################################################################################
ggplot(data = Datos_sinteticos[["syn"]][[1]]) +
  geom_histogram(aes(x = SO2, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = SO2)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(Datos_sinteticos[["syn"]][[1]]$SO2))},
                aes(color = "inverse-gaussian"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgamma(Datos_sinteticos[["syn"]][[1]]$SO2))},
                aes(color = "log-gamma"),
                size = 1) +
  scale_color_manual(breaks = c("inverse-gaussian", "log-gamma"),
                     values = c("inverse-gaussian" = "red", "log-gamma" = "blue")) +
  labs(title = "Distribución del contenido de SO2 en el Aire",
       color = "Distribución",
       y = "SDF1") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================



#==================================================================================================
############### Ajustes de las distribuciones de los datos sinteticos ###############

# Se ajusta una distribucion inverse-gaussian a los datos de la base1 sintetica
distribucion <- mlinvgauss(x = Datos_sinteticos[["syn"]][[1]]$SO2)
summary(distribucion)

# Se ajusta una distribucion log-gamma a los datos de base1 sintetica
distribucion1 <- mllgamma(x = Datos_sinteticos[["syn"]][[1]]$SO2)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos sinteticos ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de la base1 sintetica de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada inverse-gaussian
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada log-gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



###################################################################################################
#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "SO2" de los Datos Sinteticos, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlexp(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvgamma(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlgamma(Datos_sinteticos[["syn"]][[2]]$SO2),
  mllnorm(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlrayleigh(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvgauss(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlweibull(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvweibull(Datos_sinteticos[["syn"]][[2]]$SO2),
  mllgamma(Datos_sinteticos[["syn"]][[2]]$SO2)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "temp" de los Datos Sinteticos, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlexp(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvgamma(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlgamma(Datos_sinteticos[["syn"]][[2]]$SO2),
  mllnorm(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlrayleigh(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvgauss(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlweibull(Datos_sinteticos[["syn"]][[2]]$SO2),
  mlinvweibull(Datos_sinteticos[["syn"]][[2]]$SO2),
  mllgamma(Datos_sinteticos[["syn"]][[2]]$SO2)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS SINTETICOS ##
#==================================================================================================
hist(Datos_sinteticos[["syn"]][[2]]$SO2,
     main = "Distribución del contenido de SO2 en el Aire",
     freq = FALSE,
     ylim = c(0, 0.0025))
lines(mlinvweibull(Datos_sinteticos[["syn"]][[2]]$SO2), lwd = 2, lty = 1, col = "blue")
lines(mlinvgamma(Datos_sinteticos[["syn"]][[2]]$SO2), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("invweibull", "invgamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_sinteticos[["syn"]][[2]]$SO2)

##################################################################################################
ggplot(data = Datos_sinteticos[["syn"]][[2]]) +
  geom_histogram(aes(x = SO2, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = SO2)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvweibull(Datos_sinteticos[["syn"]][[2]]$SO2))},
                aes(color = "inverse-weibull"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgamma(Datos_sinteticos[["syn"]][[2]]$SO2))},
                aes(color = "inverse-gamma"),
                size = 1) +
  scale_color_manual(breaks = c("inverse-weibull", "inverse-gamma"),
                     values = c("inverse-weibull" = "red", "inverse-gamma" = "blue")) +
  labs(title = "Distribución del contenido de SO2 en el Aire",
       color = "Distribución",
       y = "SDF2") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos sinteticos ###############

# Se ajusta una distribucion inverse-weibull a los datos de la base2 sintetica
distribucion <- mlinvweibull(x = Datos_sinteticos[["syn"]][[2]]$SO2)
summary(distribucion)

# Se ajusta una distribucion inverse-gamma a los datos de base2 sintetica
distribucion1 <- mlinvgamma(x = Datos_sinteticos[["syn"]][[2]]$SO2)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos sinteticos ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de la base2 sintetica de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada inverse-weibull
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada inverse-gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "temp" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$temp),
  mlexp(Datos_original$temp),
  mlinvgamma(Datos_original$temp),
  mlgamma(Datos_original$temp),
  mllnorm(Datos_original$temp),
  mlrayleigh(Datos_original$temp),
  mlinvgauss(Datos_original$temp),
  mlweibull(Datos_original$temp),
  mlinvweibull(Datos_original$temp),
  mllgamma(Datos_original$temp)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "temp" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$temp),
  mlexp(Datos_original$temp),
  mlinvgamma(Datos_original$temp),
  mlgamma(Datos_original$temp),
  mllnorm(Datos_original$temp),
  mlrayleigh(Datos_original$temp),
  mlinvgauss(Datos_original$temp),
  mlweibull(Datos_original$temp),
  mlinvweibull(Datos_original$temp),
  mllgamma(Datos_original$temp)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$temp,
     main = "Distribución de la temperatura media anual",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mlinvweibull(Datos_original$temp), lwd = 2, lty = 1, col = "blue")
lines(mlinvgamma(Datos_original$temp), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("invweibull", "invgamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$temp)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = temp, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = temp)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvweibull(Datos_original$temp))},
                aes(color = "inverse-weibull"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgamma(Datos_original$temp))},
                aes(color = "inverse-gamma"),
                size = 1) +
  scale_color_manual(breaks = c("inverse-weibull", "inverse-gamma"),
                     values = c("inverse-weibull" = "red", "inverse-gamma" = "blue")) +
  labs(title = "Distribución de la temperatura media anual",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion inverse-weibull a los datos de temp
distribucion <- mlinvweibull(x = Datos_original$temp)
summary(distribucion)

# Se ajusta una distribucion inverse-gamma a los datos de manu
distribucion1 <- mlinvgamma(x = Datos_original$temp)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de SO2 de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada inverse-weibull
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada inverse-gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "manu" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$manu),
  mlexp(Datos_original$manu),
  mlinvgamma(Datos_original$manu),
  mlgamma(Datos_original$manu),
  mllnorm(Datos_original$manu),
  mlrayleigh(Datos_original$manu),
  mlinvgauss(Datos_original$manu),
  mlweibull(Datos_original$manu),
  mlinvweibull(Datos_original$manu),
  mllgamma(Datos_original$manu)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "manu" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$manu),
  mlexp(Datos_original$manu),
  mlinvgamma(Datos_original$manu),
  mlgamma(Datos_original$manu),
  mllnorm(Datos_original$manu),
  mlrayleigh(Datos_original$manu),
  mlinvgauss(Datos_original$manu),
  mlweibull(Datos_original$manu),
  mlinvweibull(Datos_original$manu),
  mllgamma(Datos_original$manu)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$manu,
     main = "Distribución del numero de empresas manufactureras",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mllnorm(Datos_original$manu), lwd = 2, lty = 1, col = "blue")
lines(mllgamma(Datos_original$manu), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("lnorm", "lgamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$manu)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = manu, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = manu)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllnorm(Datos_original$manu))},
                aes(color = "log-normal"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllgamma(Datos_original$manu))},
                aes(color = "log-gamma"),
                size = 1) +
  scale_color_manual(breaks = c("log-normal", "log-gamma"),
                     values = c("log-normal" = "red", "log-gamma" = "blue")) +
  labs(title = "Distribución del numero de empresas manufactureras",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion log-normal a los datos de manu
distribucion <- mllnorm(x = Datos_original$manu)
summary(distribucion)

# Se ajusta una distribucion log-gamma a los datos de manu
distribucion1 <- mllgamma(x = Datos_original$manu)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de manu de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada log-normal
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada log-gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "popul" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$popul),
  mlexp(Datos_original$popul),
  mlinvgamma(Datos_original$popul),
  mlgamma(Datos_original$popul),
  mllnorm(Datos_original$popul),
  mlrayleigh(Datos_original$popul),
  mlinvgauss(Datos_original$popul),
  mlweibull(Datos_original$popul),
  mlinvweibull(Datos_original$popul),
  mllgamma(Datos_original$popul)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "manu" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$popul),
  mlexp(Datos_original$popul),
  mlinvgamma(Datos_original$popul),
  mlgamma(Datos_original$popul),
  mllnorm(Datos_original$popul),
  mlrayleigh(Datos_original$popul),
  mlinvgauss(Datos_original$popul),
  mlweibull(Datos_original$popul),
  mlinvweibull(Datos_original$popul),
  mllgamma(Datos_original$popul)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$popul,
     main = "Distribución del tamaño de la poblacion en miles",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mllnorm(Datos_original$popul), lwd = 2, lty = 1, col = "blue")
lines(mllgamma(Datos_original$popul), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("lnorm", "lgamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$popul)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = popul, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = popul)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllnorm(Datos_original$popul))},
                aes(color = "log-normal"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllgamma(Datos_original$popul))},
                aes(color = "log-gamma"),
                size = 1) +
  scale_color_manual(breaks = c("log-normal", "log-gamma"),
                     values = c("log-normal" = "red", "log-gamma" = "blue")) +
  labs(title = "Distribución del tamaño de la poblacion en miles",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion log-normal a los datos de popul
distribucion <- mllnorm(x = Datos_original$popul)
summary(distribucion)

# Se ajusta una distribucion log-gamma a los datos de popul
distribucion1 <- mllgamma(x = Datos_original$popul)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de popul de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada log-normal
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada log-gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "wind" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$wind),
  mlexp(Datos_original$wind),
  mlinvgamma(Datos_original$wind),
  mlgamma(Datos_original$wind),
  mllnorm(Datos_original$wind),
  mlrayleigh(Datos_original$wind),
  mlinvgauss(Datos_original$wind),
  mlweibull(Datos_original$wind),
  mlinvweibull(Datos_original$wind),
  mllgamma(Datos_original$wind)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "wind" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$wind),
  mlexp(Datos_original$wind),
  mlinvgamma(Datos_original$wind),
  mlgamma(Datos_original$wind),
  mllnorm(Datos_original$wind),
  mlrayleigh(Datos_original$wind),
  mlinvgauss(Datos_original$wind),
  mlweibull(Datos_original$wind),
  mlinvweibull(Datos_original$wind),
  mllgamma(Datos_original$wind)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$wind,
     main = "Distribución de la velocidad media anual del viento",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mlgamma(Datos_original$wind), lwd = 2, lty = 1, col = "blue")
lines(mllnorm(Datos_original$wind), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("gamma", "lnorm"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$wind)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = wind, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = wind)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgamma(Datos_original$wind))},
                aes(color = "gamma"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllnorm(Datos_original$wind))},
                aes(color = "log-normal"),
                size = 1) +
  scale_color_manual(breaks = c("gamma", "log-normal"),
                     values = c("gamma" = "red", "log-normal" = "blue")) +
  labs(title = "Distribución de la velocidad media anual del viento",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion gamma a los datos de wind
distribucion <- mlgamma(x = Datos_original$wind)
summary(distribucion)

# Se ajusta una distribucion log-normal a los datos de wind
distribucion1 <- mllnorm(x = Datos_original$wind)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de wind de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada gamma
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada log-normal
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "precip" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$precip),
  mlexp(Datos_original$precip),
  mlinvgamma(Datos_original$precip),
  mlgamma(Datos_original$precip),
  mllnorm(Datos_original$precip),
  mlrayleigh(Datos_original$precip),
  mlinvgauss(Datos_original$precip),
  mlweibull(Datos_original$precip),
  mlinvweibull(Datos_original$precip),
  mllgamma(Datos_original$precip)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "precip" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$precip),
  mlexp(Datos_original$precip),
  mlinvgamma(Datos_original$precip),
  mlgamma(Datos_original$precip),
  mllnorm(Datos_original$precip),
  mlrayleigh(Datos_original$precip),
  mlinvgauss(Datos_original$precip),
  mlweibull(Datos_original$precip),
  mlinvweibull(Datos_original$precip),
  mllgamma(Datos_original$precip)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$precip,
     main = "Distribución de la precipitacion media anual",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mlweibull(Datos_original$precip), lwd = 2, lty = 1, col = "blue")
lines(mlgamma(Datos_original$precip), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("weibull", "gamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$precip)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = precip, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = precip)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(Datos_original$precip))},
                aes(color = "weibull"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgamma(Datos_original$precip))},
                aes(color = "gamma"),
                size = 1) +
  scale_color_manual(breaks = c("weibull", "gamma"),
                     values = c("weibull" = "red", "gamma" = "blue")) +
  labs(title = "Distribución de la precipitacion media anual",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion weibull a los datos de precip
distribucion <- mlweibull(x = Datos_original$precip)
summary(distribucion)

# Se ajusta una distribucion gamma a los datos de precip
distribucion1 <- mlgamma(x = Datos_original$precip)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de precip de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada weibull
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================



#==================================================================================================
#Se comparan únicamente las distribuciones con un dominio [0, +inf)

#### Comparacion de la variable "predays" de los Datos Originales, con la metrica de ajuste "AIC" ####
comparacion_aic <- AIC(
  mlbetapr(Datos_original$predays),
  mlexp(Datos_original$predays),
  mlinvgamma(Datos_original$predays),
  mlgamma(Datos_original$predays),
  mllnorm(Datos_original$predays),
  mlrayleigh(Datos_original$predays),
  mlinvgauss(Datos_original$predays),
  mlweibull(Datos_original$predays),
  mlinvweibull(Datos_original$predays),
  mllgamma(Datos_original$predays)
)
comparacion_aic %>% rownames_to_column(var = "distribucion") %>% arrange(AIC)

#### Comparacion de la variable "predays" de los Datos Originales, con la metrica de ajuste "BIC" ####
comparacion_bic <- BIC(
  mlbetapr(Datos_original$predays),
  mlexp(Datos_original$predays),
  mlinvgamma(Datos_original$predays),
  mlgamma(Datos_original$predays),
  mllnorm(Datos_original$predays),
  mlrayleigh(Datos_original$predays),
  mlinvgauss(Datos_original$predays),
  mlweibull(Datos_original$predays),
  mlinvweibull(Datos_original$predays),
  mllgamma(Datos_original$predays)
)
comparacion_bic %>% rownames_to_column(var = "distribucion") %>% arrange(BIC)
#==================================================================================================


## REPRESENTACION GRAFICA DE LAS DISTRIBUCIONES QUE SE MEJOR SE AJUSTAN DE LOS DATOS ORIGINALES ##
#==================================================================================================
hist(Datos_original$predays,
     main = "Distribución del numero medio de dias con precipitacion",
     freq = FALSE,
     ylim = c(0, 0.00025))
lines(mlweibull(Datos_original$predays), lwd = 2, lty = 1, col = "blue")
lines(mlgamma(Datos_original$predays), lwd = 2, lty = 2, col = "red")
legend(x = 15000, y = 0.0001, legend = c("weibull", "gamma"),
       col = c("blue", "red"), lty = 1:2)
rug(Datos_original$predays)

##################################################################################################
ggplot(data = Datos_original) +
  geom_histogram(aes(x = predays, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = predays)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(Datos_original$predays))},
                aes(color = "weibull"),
                size = 1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgamma(Datos_original$predays))},
                aes(color = "gamma"),
                size = 1) +
  scale_color_manual(breaks = c("weibull", "gamma"),
                     values = c("weibull" = "red", "gamma" = "blue")) +
  labs(title = "Distribución del numero medio de dias con precipitacion",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom") 
#==================================================================================================


#==================================================================================================
############### Ajustes de las distribuciones de los datos originales ###############

# Se ajusta una distribucion weibull a los datos de predays
distribucion <- mlweibull(x = Datos_original$predays)
summary(distribucion)

# Se ajusta una distribucion gamma a los datos de predays
distribucion1 <- mlgamma(x = Datos_original$predays)
summary(distribucion1)
#==================================================================================================


#==================================================================================================
########## Intervalos de confianza por bootstraping de los datos originales ##########

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion, probs = c(0.05, 0.95), reps = 1000)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(distribucion1, probs = c(0.05, 0.95), reps = 1000)
#==================================================================================================


#==================================================================================================
############ Muestras nuevas de predays de acorde a la distribucion ajustada ############

# Muestras de la distribucion ajustada weibull
set.seed(1500)
rml(n = 5, obj = distribucion)

# Muestras de la distribucion ajustada gamma
set.seed(1500)
rml(n = 5, obj = distribucion1)
#==================================================================================================
