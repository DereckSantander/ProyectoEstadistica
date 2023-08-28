#install.packages("moments")
library(moments)
library(readr)
library(ggplot2)
library(fdth)
library(dplyr)
library(RColorBrewer)


data <- read_csv("Impacto del Covid-19.csv")

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

tabla10 <- data %>%
  group_by(Genero) %>%
  summarize(Frecuencia = n()) %>%
  mutate(FrecuenciaRelativa = Frecuencia / sum(Frecuencia))
tabla10

barplot(tabla_genero,
        main = "Genero",
        ylab = "Frecuencia",
        col = brewer.pal(3,"Set2"))

######Ex_Clases_Virtuales######
media_Exp_Clases_virtuales <- mean(data$Ex_ClasesVirtuales)
mediana_Exp_Clases_virtuales <- median(data$Ex_ClasesVirtuales)
desviacion_estandar_Exp_Clases_virtuales <- sd(data$Ex_ClasesVirtuales)
cuartiles_Exp_Clases_virtuales <- quantile(data$Ex_ClasesVirtuales, 
                                           probs = c(0.25, 0.5, 0.75)) 

cat("Media:", media_Exp_Clases_virtuales, "\n")
cat("Mediana:", mediana_Exp_Clases_virtuales, "\n")
cat("Desviación Estándar:", desviacion_estandar_Exp_Clases_virtuales, "\n")
cat("Cuartiles:", cuartiles_Exp_Clases_virtuales, "\n")

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
media_Satis_Tiempo_Est <- mean(data$TiempoEstudio)
mediana_Satis_Tiempo_Est <- median(data$TiempoEstudio)
desviacion_estandar_Satis_Tiempo_Est <- sd(data$TiempoEstudio)
cuartiles_Satis_Tiempo_Est <- quantile(data$TiempoEstudio, probs = c(0.25, 0.5, 0.75)) 

cat("Media:", media_Satis_Tiempo_Est, "\n")
cat("Mediana:", mediana_Satis_Tiempo_Est, "\n")
cat("Desviación Estándar:", desviacion_estandar_Satis_Tiempo_Est, "\n")
cat("Cuartiles:", cuartiles_Satis_Tiempo_Est, "\n")

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


######N_Estes_CV######
media_Nivel_Estres <- mean(data$N_Estes_CV)
mediana_Nivel_Estres <- median(data$N_Estes_CV)
desviacion_estandar_Nivel_Estres <- sd(data$N_Estes_CV)
cuartiles_Nivel_Estres <- quantile(data$N_Estes_CV, probs = c(0.25, 0.5, 0.75)) 

cat("Media:", media_Nivel_Estres, "\n")
cat("Mediana:", mediana_Nivel_Estres, "\n")
cat("Desviación Estándar:", desviacion_estandar_Nivel_Estres, "\n")
cat("Cuartiles:", cuartiles_Nivel_Estres, "\n")

tabla_Nivel_Estres <- fdth::fdt(data$N_Estes_CV)
tabla_Nivel_Estres

boxplot(data$N_Estes_CV, 
        main = "Nivel de estres en clases virtuales", 
        ylab = "Valoracion",
        col = "#d8d6a8")

barplot(table(data$N_Estes_CV), 
        main = "Nivel de estres en clases virtuales", 
        xlab = "Valoracion", 
        ylab = "Frecuencia",
        col = heat.colors(5, rev=T))


######Realiza_Deportes######
tabla_Actividades <- table(data$Realiza_Deportes)

tabla12 <- data %>%
  group_by(Realiza_Deportes) %>%
  summarize(Frecuencia = n()) %>%
  mutate(FrecuenciaRelativa = Frecuencia / sum(Frecuencia))
tabla12

barplot(tabla_Actividades,
        main = "¿Realiza otras actividades?",
        ylab = "Frecuencia",
        col = brewer.pal(3,"Set2"))

####################################################################

