# trabajando en el ejercicio 1 del practico 3

install.packages("readxl")
install.packages("sqldf")
install.packages("here")
install.packages("ClusterR")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("NbClust")


library("readxl") 
library("sqldf")
library("here")
library("ClusterR")
library("cluster")
library("factoextra")
library("NbClust")


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

# Calculamos cual es el numero de clusters optimo
fviz_nbclust(nuevaTabla, kmeans, method= "silhouette")  
#En este caso, el numero optimo de clusters es 7



# Busqueda de clusters

## K=2
k2 <- kmeans(nuevaTabla, 2)
fviz_cluster(k2, data = nuevaTabla) #con k=2

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(k2$cluster)

View(nuevaTabla)


## k=3
nuevaTabla <- columnaAgregada[c(1, 2, 3, 6)]
k3 <- kmeans(nuevaTabla, 3)
fviz_cluster(k3, data = nuevaTabla) #con k=3

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(k3$cluster)

View(nuevaTabla)


## k=4
nuevaTabla <- columnaAgregada[c(1, 2, 3, 6)]
k4 <- kmeans(nuevaTabla, 4)
fviz_cluster(k4, data = nuevaTabla) #con k=4

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(k4$cluster)

View(nuevaTabla)

