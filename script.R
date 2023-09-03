#install.packages("moments")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages("PerformanceAnalytics")
#install.packages("MASS")
#install.packages("survival")
#installed.packages("fitdistrplus")
library(MASS)
library(survival)
library(fitdistrplus)
library(moments)
library(readr)
library(ggplot2)
library(fdth)
library(dplyr)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

#data <- read.csv("Impacto del Covid 19")

###################################
######VARIABLES CUANTITATIVAS######
###################################

######Edad######
media_Edad <- mean(data$Edad)
mediana_Edad <- median(data$Edad)
desviacion_estandar_Edad <- sd(data$Edad)
cuartiles_Edad <- quantile(data$Edad, probs = c(0.25, 0.5, 0.75)) 
curtosis_Edad <- kurtosis(data$Edad)
sesgo_Edad <- skewness(data$Edad)

cat("Media:", media_Edad, "\n")
cat("Mediana:", mediana_Edad, "\n")
cat("Desviación Estándar:", desviacion_estandar_Edad, "\n")
cat("Cuartiles:", cuartiles_Edad, "\n")
cat("Curtosis:", curtosis_Edad, "\n")
cat("Sesgo:", sesgo_Edad, "\n")

tabla_Edad <- fdth::fdt(data$Edad)
tabla_Edad 

boxplot(data$Edad,
        main = "Edades",
        ylab = "Edad",
        col = "dodgerblue",
        ylim = c(15,30))

hist(data$Edad, 
     main = "Histograma de edades", 
     xlab = "Rango de edades", 
     ylab = "Frecuencia",
     col = brewer.pal(8,"Set2"))

######H_Clases_virtuales######
media_H_Clases_virtuales <- mean(data$`H_Clases virtuales`)
mediana_H_Clases_virtuales <- median(data$`H_Clases virtuales`)
desviacion_estandar_H_Clases_virtuales <- sd(data$`H_Clases virtuales`)
cuartiles_H_Clases_virtuales <- quantile(data$`H_Clases virtuales`, probs = c(0.25, 0.5, 0.75))
curtosis_H_Clases_virtuales <- kurtosis(data$`H_Clases virtuales`)
sesgo_H_Clases_virtuales <- skewness(data$`H_Clases virtuales`)

cat("Media:", media_H_Clases_virtuales, "\n")
cat("Mediana:", mediana_H_Clases_virtuales, "\n")
cat("Desviación Estándar:", desviacion_estandar_H_Clases_virtuales, "\n")
cat("Cuartiles:", cuartiles_H_Clases_virtuales, "\n")
cat("Curtosis:", curtosis_H_Clases_virtuales, "\n")
cat("Sesgo:", sesgo_H_Clases_virtuales, "\n")

tabla_H_Clases_virtuales <- fdth::fdt(data$`H_Clases virtuales`)
tabla_H_Clases_virtuales

boxplot(data$`H_Clases virtuales`, 
        main = "# Horas de clases virtuales", 
        ylab = "Horas",
        col = "goldenrod")

hist(data$`H_Clases virtuales`, 
     main = "Histograma de horas de clases virtuales", 
     xlab = "Numero de horas dedicadas", 
     ylab = "Frecuencia",
     col = brewer.pal(5,"Set1"))

######H_Estudio_Autonomo######
media_H_Est_Auto <- mean(data$H_EStudioAutonomo)
mediana_H_Est_Auto <- median(data$H_EStudioAutonomo)
desviacion_estandar_H_Est_Auto <- sd(data$H_EStudioAutonomo)
cuartiles_H_Est_Auto <- quantile(data$H_EStudioAutonomo, probs = c(0.25, 0.5, 0.75)) 
curtosis_H_Est_Auto <- kurtosis(data$H_EStudioAutonomo)
sesgo_H_Est_Auto <- skewness(data$H_EStudioAutonomo)

cat("Media:", media_H_Est_Auto, "\n")
cat("Mediana:", mediana_H_Est_Auto, "\n")
cat("Desviación Estándar:", desviacion_estandar_H_Est_Auto, "\n")
cat("Cuartiles:", cuartiles_H_Est_Auto, "\n")
cat("Curtosis:", curtosis_H_Est_Auto, "\n")
cat("Sesgo:", sesgo_H_Est_Auto, "\n")

tabla_H_Est_Auto <- fdth::fdt(data$H_EStudioAutonomo)
tabla_H_Est_Auto

boxplot(data$H_EStudioAutonomo, 
        main = "# Horas de Estudio Autonomo", 
        ylab = "Horas",
        col = "#a8d8d0")

hist(data$H_EStudioAutonomo, 
     main = "# Horas de Estudio Autonomo", 
     xlab = "Numero de horas", 
     ylab = "Frecuencia",
     col = brewer.pal(10,"Set3"),
     ylim = c(0,25))

######Promedio_Cv######
media_promedio <- mean(data$Promedio_Cv)
mediana_promedio <- median(data$Promedio_Cv)
desviacion_estandar_promedio <- sd(data$Promedio_Cv)
cuartiles_promedio <- quantile(data$Promedio_Cv, probs = c(0.25, 0.5, 0.75)) 
curtosis_promedio <- kurtosis(data$Promedio_Cv)
sesgo_promedio <- skewness(data$Promedio_Cv)

cat("Media:", media_promedio, "\n")
cat("Mediana:", mediana_promedio, "\n")
cat("Desviación Estándar:", desviacion_estandar_promedio, "\n")
cat("Cuartiles:", cuartiles_promedio, "\n")
cat("Curtosis:", curtosis_promedio, "\n")
cat("Sesgo:", sesgo_promedio, "\n")

tabla_promedio <- fdth::fdt(data$Promedio_Cv)
tabla_promedio

boxplot(data$Promedio_Cv, 
        main = "Promedio en clases virtuales", 
        ylab = "Puntaje",
        col = "#a8d8d1")

hist(data$Promedio_Cv, 
     main = "Promedio en clases virtuales", 
     xlab = "Puntaje", 
     ylab = "Frecuencia",
     col = brewer.pal(10,"RdYlGn"))

######H_Dormir######
media_H_dormir <- mean(data$H_Dormir)
mediana_H_dormir <- median(data$H_Dormir)
desviacion_estandar_H_dormir <- sd(data$H_Dormir)
cuartiles_H_dormir <- quantile(data$H_Dormir, probs = c(0.25, 0.5, 0.75)) 
curtosis_H_dormir <- kurtosis(data$H_Dormir)
sesgo_H_dormir <- skewness(data$H_Dormir)

cat("Media:", media_H_dormir, "\n")
cat("Mediana:", mediana_H_dormir, "\n")
cat("Desviación Estándar:", desviacion_estandar_H_dormir, "\n")
cat("Cuartiles:", cuartiles_H_dormir, "\n")
cat("Curtosis:", curtosis_H_dormir, "\n")
cat("Sesgo:", sesgo_H_dormir, "\n")

tabla_H_dormir <- fdth::fdt(data$H_Dormir)
tabla_H_dormir

boxplot(data$H_Dormir, 
        main = "Horas de sueño", 
        ylab = "Horas",
        col = "#c4d8a8")

hist(data$H_Dormir, 
     main = "Horas de sueño",
     xlab = "Horas", 
     ylab = "Frecuencia",
     col = brewer.pal(8,"Accent"))



####################################
#######VARIABLES CUALITATIVAS#######
#################################### 

######Genero######
tabla_genero <- table(data$Genero)

#tabla10 <- data %>%
#  group_by(Genero) %>%
#  summarize(Frecuencia = n()) %>%
#  mutate(FrecuenciaRelativa = Frecuencia / sum(Frecuencia))
#tabla10

barplot(tabla_genero,
        main = "Genero",
        ylab = "Frecuencia",
        col = brewer.pal(3,"Set2"))

######Ex_Clases_Virtuales######
#media_Exp_Clases_virtuales <- mean(data$Ex_ClasesVirtuales)
#mediana_Exp_Clases_virtuales <- median(data$Ex_ClasesVirtuales)
#desviacion_estandar_Exp_Clases_virtuales <- sd(data$Ex_ClasesVirtuales)
#cuartiles_Exp_Clases_virtuales <- quantile(data$Ex_ClasesVirtuales, 
#                                           probs = c(0.25, 0.5, 0.75)) 

#cat("Media:", media_Exp_Clases_virtuales, "\n")
#cat("Mediana:", mediana_Exp_Clases_virtuales, "\n")
#cat("Desviación Estándar:", desviacion_estandar_Exp_Clases_virtuales, "\n")
#cat("Cuartiles:", cuartiles_Exp_Clases_virtuales, "\n")

tabla_Exp_Clases_virtuales <- fdth::fdt(data$Ex_ClasesVirtuales)
tabla_Exp_Clases_virtuales

boxplot(data$Ex_ClasesVirtuales, 
        main = "Experiencia en clases virtuales", 
        ylab = "Valoracion")

barplot(table(data$Ex_ClasesVirtuales),
        main = "Experiencia en clases virtuales", 
        xlab = "Valoracion", 
        ylab = "Frecuencia",
        col = terrain.colors(5))

######Tiempo_Estudio######
#media_Satis_Tiempo_Est <- mean(data$TiempoEstudio)
#mediana_Satis_Tiempo_Est <- median(data$TiempoEstudio)
#desviacion_estandar_Satis_Tiempo_Est <- sd(data$TiempoEstudio)
#cuartiles_Satis_Tiempo_Est <- quantile(data$TiempoEstudio, probs = c(0.25, 0.5, 0.75)) 

#cat("Media:", media_Satis_Tiempo_Est, "\n")
#cat("Mediana:", mediana_Satis_Tiempo_Est, "\n")
#cat("Desviación Estándar:", desviacion_estandar_Satis_Tiempo_Est, "\n")
#cat("Cuartiles:", cuartiles_Satis_Tiempo_Est, "\n")

tabla_Satis_Tiempo_Est <- fdth::fdt(data$TiempoEstudio)
tabla_Satis_Tiempo_Est

boxplot(data$TiempoEstudio, 
        main = "Satisfaccion del tiempo de estudio autonomo", 
        ylab = "Valoracion",
        col = "#a8d8c0")

barplot(table(data$TiempoEstudio),
        main = "Satisfaccion del tiempo de estudio autonomo", 
        xlab = "Valoracion", 
        ylab = "Frecuencia",
        col = terrain.colors(5))


######Estres_CV######
#media_Nivel_Estres <- mean(data$Estres_CV)
#mediana_Nivel_Estres <- median(data$Estres_CV)
#desviacion_estandar_Nivel_Estres <- sd(data$Estres_CV)
#cuartiles_Nivel_Estres <- quantile(data$Estres_CV, probs = c(0.25, 0.5, 0.75)) 

#cat("Media:", media_Nivel_Estres, "\n")
#cat("Mediana:", mediana_Nivel_Estres, "\n")
#cat("Desviación Estándar:", desviacion_estandar_Nivel_Estres, "\n")
#cat("Cuartiles:", cuartiles_Nivel_Estres, "\n")

tabla_Nivel_Estres <- fdth::fdt(data$Estres_CV)
tabla_Nivel_Estres

boxplot(data$Estres_CV, 
        main = "Nivel de estres en clases virtuales", 
        ylab = "Valoracion",
        col = "#d8d6a8")

barplot(table(data$Estres_CV), 
        main = "Nivel de estres en clases virtuales", 
        xlab = "Valoracion", 
        ylab = "Frecuencia",
        col = heat.colors(5, rev=T))


######Realiza_Deportes######
tabla_Actividades <- table(data$Realiza_Deportes)

#tabla12 <- data %>%
#  group_by(Realiza_Deportes) %>%
#  summarize(Frecuencia = n()) %>%
#  mutate(FrecuenciaRelativa = Frecuencia / sum(Frecuencia))
#tabla12

barplot(tabla_Actividades,
        main = "¿Realiza otras actividades?",
        ylab = "Frecuencia",
        col = brewer.pal(3,"Set2"))

####################################################################
####################Analisis bivariante#############################
####################################################################

#Diagramas de cajas segmentados

#Promedio segmentado por Genero
boxplot(data$Promedio_Cv~data$Genero,
        main = "Promedio segmentado por Genero",
        col = brewer.pal(3, "Set2"),
        ylab= "Genero",
        xlab= "Promedio en clases virtuales",
        horizontal = T)

#Horas de estudio autonomo segmentado por Genero
boxplot(data$H_EStudioAutonomo~data$Genero,
        main = "Horas de estudio autonomo segmentado por Genero",
        col = brewer.pal(3, "Set3"),
        ylab= "Genero",
        xlab= "Horas de estudio autonomo",
        horizontal = T)

#Promedio segmentado por Experiencia en clases virtuales
boxplot(data$Promedio_Cv~data$Ex_ClasesVirtuales,
        main = "Promedio segmentado por Experiencia en clases virtuales",
        col = brewer.pal(5, "Set2"),
        ylab= "Experiencia en clases virtuales",
        xlab= "Promedio en clases virtuales",
        horizontal = T)

#Promedio segmentado por Nivel de estres
boxplot(data$Promedio_Cv~data$Estres_CV,
        main = "Promedio segmentado por Nivel de estres",
        col = brewer.pal(5, "Set1"),
        ylab= "Nivel de estres",
        xlab= "Promedio en clases virtuales",
        horizontal = T)

#Nivel de Estres segmentado por Realizacion de otras actividades
boxplot(data$Estres_CV~data$Realiza_Deportes,
        main = "Promedio segmentado por Realizacion de otras actividades",
        col = brewer.pal(3, "Set1"),
        ylab= "¿Realiza o no?",
        xlab= "Nivel de estres en clases virtuales",
        horizontal = T)

#Diagramas de dispersion

#Horas de estudio autonomo y promedio en clases virtuales
plot(data$H_EStudioAutonomo,data$Promedio_Cv,
     main = "",pch=19, col="black")

#Matriz de correlacion
mc = data[,2:9]
matriz_correlacion <- round(cor(mc,method = "pearson"),2)
matriz_correlacion
corrplot(matriz_correlacion, method="number", type="upper")
corrplot.mixed(matriz_correlacion)

#Dependencia/Independencia de variables - Tablas de contingencia

#Promedio_Cv, Estres_CV
tabla_contingencia_Prom_Estres <- table(data$Promedio_Cv,data$Estres_CV)
tabla_completa_Prom_Estres <- addmargins(tabla_contingencia_Prom_Estres)
tabla_completa_Prom_Estres

cat("Ho: Promedio_Cv y Estres_CV son independientes\n")
cat("Ha: Promedio_Cv y Estres_CV son dependientes\n")

chi_cuadrado1 <- chisq.test(tabla_contingencia_Prom_Estres)

cat("p = ", chi_cuadrado1$p.value, "\n")

if(chi_cuadrado1$p.value < 0.05){
  cat("Valor p muy pequeño. Se rechaza Ho. Por lo tanto Promedio_Cv y Estres_CV son dependientes\n")
} else{
  cat("Valor p no tan pequeño. No se rechaza Ho. Por lo tanto Promedio_Cv y Estres_CV son independientes\n")
}

#Promedio_Cv, H_EstudioAutonomo
tabla_contingencia_Prom_EstAu <- table(data$Promedio_Cv,data$H_EStudioAutonomo)
tabla_completa_Prom_EstAu <- addmargins(tabla_contingencia_Prom_EstAu)
tabla_completa_Prom_EstAu

cat("Ho: Promedio_Cv y H_EstudioAutonomo son independientes\n")
cat("Ha: Promedio_Cv y H_EstudioAutonomo son dependientes\n")

chi_cuadrado2 <- chisq.test(tabla_contingencia_Prom_EstAu)

cat("p = ", chi_cuadrado2$p.value, "\n")

if(chi_cuadrado2$p.value < 0.05){
  cat("Valor p muy pequeño. Se rechaza Ho. Por lo tanto Promedio_Cv y H_EstudioAutonomo son dependientes\n")
} else{
  cat("Valor p no tan pequeño. No se rechaza Ho. Por lo tanto Promedio_Cv y H_EstudioAutonomo son independientes\n")
}
#####Prueba de bondad de ajuste#######

#Ho: Cumple con una distribucion normal
#Ha: No cumple con una distribucion normal
#Bajo un supuesto del 0.05 de significancia

#Promedio_Cv
require(MASS)
ajuste_Promedio <- fitdist(data$Promedio_Cv, "norm")
ajuste_Promedio
#Si el valor P es menor al nivel de significancia de 0.05, se rechaza la Ho
ks_Promedio <- ks.test(data$Promedio_Cv, "pnorm", mean=ajuste_Promedio$estimate[1], sd=ajuste_Promedio$estimate[2])
ks_Promedio
if(ks_Promedio$p.value < 0.05){
  cat("Valor P menor al nivel de significancia, se rechaza Ho, por lo tanto no sigue una distribucion normal\n")
} else{
  cat("Valor P fuera de region de rechazo, no se rechaza Ho, por lo tanto sigue una distribucion normal")
}

#H_EstudioAutonomo
ajuste_H_EstudioAutonomo <- fitdist(data$H_EStudioAutonomo, "norm")
ajuste_H_EstudioAutonomo
ks_H_EstudioAutonomo <- ks.test(data$H_EStudioAutonomo, "pnorm", mean=ajuste_H_EstudioAutonomo$estimate[1], sd=ajuste_H_EstudioAutonomo$estimate[2])
ks_H_EstudioAutonomo
if(ks_H_EstudioAutonomo$p.value < 0.05){
  cat("Valor P menor al nivel de significancia, se rechaza Ho, por lo tanto no sigue una distribucion normal\n")
} else{
  cat("Valor P fuera de region de rechazo, no se rechaza Ho, por lo tanto sigue una distribucion normal")
} 

#H_Dormir
ajuste_H_Dormir <- fitdist(data$H_Dormir, "norm")
ajuste_H_Dormir
#Si el valor P es menor al nivel de significancia de 0.05, se rechaza la Ho
ks_H_Dormir <- ks.test(data$H_Dormir, "pnorm", mean=ajuste_H_Dormir$estimate[1], sd=ajuste_H_Dormir$estimate[2])
ks_H_Dormir
if(ks_H_Dormir$p.value < 0.05){
  cat("Valor P menor al nivel de significancia, se rechaza Ho, por lo tanto no sigue una distribucion normal\n")
} else{
  cat("Valor P fuera de region de rechazo, no se rechaza Ho, por lo tanto sigue una distribucion normal")
}  
#####################################################################
##########################Regresion lineal###########################
#####################################################################

#Modelo Regresion Lineal Promedio_Cv, H_EstudioAutonomo
cat('Modelo de Regresion Lineal entre el promedio y las horas de estudio autonomo\n')

modelo1 <- data.frame(data$Promedio_Cv,data$H_EStudioAutonomo)
modeloRegresion_Prom_EstAu <- lm(data$Promedio_Cv ~ data$H_EStudioAutonomo, data = modelo1, na.action = na.exclude )
summary(modeloRegresion_Prom_EstAu)

#Significancia
summary(modeloRegresion_Prom_EstAu)$coefficients

B0_m1 <- round(modeloRegresion_Prom_EstAu$coefficients[1],2)
B1_m1 <- round(modeloRegresion_Prom_EstAu$coefficients[2],2)

cat('Y = ',B0_m1,'+',B1_m1,'X\n')
cat('Intercepto (B0): ',B0_m1,'\n')
cat('Pendiente (B1): ',B1_m1,'\n')

plot(data$H_EStudioAutonomo,data$Promedio_Cv, xlab='Horas de estudio autonomo', ylab='Promedio')
abline(modeloRegresion_Prom_EstAu, col='red')

#Modelo Regresion Lineal Promedio_Cv, Estres_CV
cat('Modelo de Regresion Lineal entre el promedio y el nivel de estres\n')

modelo2 <- data.frame(data$Promedio_Cv,data$Estres_CV)
modeloRegresion_Prom_Estres <- lm(data$Promedio_Cv ~ data$Estres_CV, data = modelo2, na.action = na.exclude )
summary(modeloRegresion_Prom_Estres)

#Significancia
summary(modeloRegresion_Prom_Estres)$coefficients

B0_m2 <- round(modeloRegresion_Prom_Estres$coefficients[1],2)
B1_m2 <- round(modeloRegresion_Prom_Estres$coefficients[2],2)

cat('Y = ',B0_m2,'+',B1_m2,'X\n')
cat('Intercepto (B0): ',B0_m2,'\n')
cat('Pendiente (B1): ',B1_m2,'\n')

plot(data$Estres_CV,data$Promedio_Cv, xlab='Nivel de estrés', ylab='Promedio')
abline(modeloRegresion_Prom_Estres, col='red')

#Prueba de hipotesis para una media

cat('H0: Promedio de horas de sueño menor a 7\n')
cat('Ha: Promedio de horas de sueño mayor o igual a 7\n')

valor_z <- (media_H_dormir - 7) / (desviacion_estandar_H_dormir / sqrt(100))

#-valor p, cola derecha
valor_p <- 1 - pnorm(valor_z)
cat('p = ',valor_p,"\n")

if(valor_p < 0.05){
  cat("Valor p muy pequeño. Se rechaza Ho. Por lo tanto, el promedio de horas de sueño es mayor o igual a 7\n")
}else{
  cat("Valor p no tan pequeño. No se rechaza Ho. Por lo tanto, el promedio de horas de sueño es menor a 7\n")
}
