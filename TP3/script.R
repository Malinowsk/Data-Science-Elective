# trabajando en el ejercicio 1 del practico 3

install.packages("here")

library("readxl") 
library("sqldf")
library("here")

# Leo la tabla generada en el trabajo practico 2
here()
NotaFinales = read_excel(here("NotaFinales.xlsx"))

View(NotaFinales)

cantidadDeFilas = nrow(NotaFinales)

Situacion_Final<-sample(c(0,1), size = cantidadDeFilas, replace = TRUE)

# Agrego columna Situacion_Final
columnaAgregada<-cbind(NotaFinales, Situacion_Final)

# Reemplazo los valores de la columna Situacion_Final por los correctos de acuerdo a la columna notaFinal
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal<4, 0)
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal>=4, 1)

View(columnaAgregada)

# trabajando en el ejercicio 2 del practico 3

# Creacion del conjunto de datos de entrenamiento y de validacion

# 70% del conjunto de datos para entrenamiento
tamanioEntrenamiento <- floor (0.8 * nrow(columnaAgregada))

# Definimos la semilla para reproducibilidad de los datos
set.seed(30)
entrenamiento_ind <- sample (seq_len(nrow(columnaAgregada)), size=tamanioEntrenamiento)

entrenamiento <- columnaAgregada[entrenamiento_ind, ]
validacion <- columnaAgregada[-entrenamiento_ind, ]


# Creacion del modelo de regresion logistica
rl_model <- glm(Situacion_Final~Legajo + materia + cond_regularidad, 
                family = binomial,
                data = entrenamiento)

summary(rl_model)

# Prediccion de valores para validar el modelo
predict_Situacion_Final <- predict (rl_model, validacion, type="response")  # Calcula la probabilidad de que sea 1 en base a sus predictores
predict_Situacion_Final

# Transformacion de probabilidades en tipo binario
predict_Situacion_Final <- ifelse (predict_Situacion_Final>0.58, 1, 0)
predict_Situacion_Final

# Matriz de confusion
table(validacion$Situacion_Final, predict_Situacion_Final)

# Calculo de la presicion
missing_classes <- mean(predict_Situacion_Final != validacion$Situacion_Final)
print(paste('Precision =', 1 - missing_classes))


# 3) Modelo K-Means

nuevaTabla <- columnaAgregada[c(1, 2, 3, 6)]
View(nuevaTabla)


# Busqueda de clusters
clusters <- kmeans(nuevaTabla, 2)

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(clusters$cluster)

View(nuevaTabla)


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "plyr", "reshape2", "RColorBrewer", "scales", "grid")
ipak(packages)


fviz_cluster(clusters, data = nuevaTabla)

#plot( nuevaTabla$Legajo, nuevaTabla$materia, col = factor(nuevaTabla$segmento) )


clusters <- kmeans(nuevaTabla, 3)

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(clusters$cluster)

View(nuevaTabla)

plot( nuevaTabla$Legajo, nuevaTabla$materia, nuevaTabla$cond_regularidad , nuevaTabla$Situacion_Final , col = factor(nuevaTabla$segmento) )

clusters <- kmeans(nuevaTabla, 4)

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(clusters$cluster)

View(nuevaTabla)

# Gr?fico de los segmentos
plot( nuevaTabla$Legajo, nuevaTabla$materia, nuevaTabla$cond_regularidad , nuevaTabla$Situacion_Final , col = factor(nuevaTabla$segmento) )


