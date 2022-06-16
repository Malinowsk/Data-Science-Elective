# trabajando en el ejercicio 1 del practico 3

library("readxl") 
library("sqldf")

# Leo la tabla generada en el trabajo practico 2
tablaEjercicio2 = read_excel("../Segunda\ Parte/Regresion-Lineal-en-R/tablaEjercicio2.xlsx")

View(tablaEjercicio2)

cantidadDeFilas = nrow(tablaEjercicio2)

Situacion_Final<-sample(c(0,1), size = cantidadDeFilas, replace = TRUE)

# Agrego columna Situacion_Final
columnaAgregada<-cbind(tablaEjercicio2, Situacion_Final)
View(columnaAgregada)
# Ver cuando respondan el Mail que filtro realizar exactamente
# Reemplazo los valores de la columna Situacion_Final por los correctos de acuerdo a la columna notaFinal
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal<6, 0)
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal>=6, 1)

View(columnaAgregada)


# 2)
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
predict_Situacion_Final <- ifelse (predict_Situacion_Final>0.693, 1, 0)
predict_Situacion_Final

# Matriz de confusión
table(validacion$Situacion_Final, predict_Situacion_Final)

# Cálculo de la presición
missing_classes <- mean(predict_Situacion_Final != validacion$Situacion_Final)
print(paste('Precisión =', 1 - missing_classes))


# 3) Modelo K-Means

nuevaTabla <- columnaAgregada[c(1, 2, 3, 6)]
View(nuevaTabla)


# Búsqueda de clusters
clusters <- kmeans(nuevaTabla, 3)

# Agrego el cluster a los datos
nuevaTabla$segmento <- as.factor(clusters$cluster)

View(nuevaTabla)


# Gráfico de los segmentos
plot (x = nuevaTabla$Legajo,
      nuevaTabla$materia,
      nuevaTabla$cond_regularidad,      
      nuevaTabla$Situacion_Final,
      col = factor(nuevaTabla$segmento))



