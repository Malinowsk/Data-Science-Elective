# trabajando en el ejercicio 1 del practico 3

library("readxl") 
library("sqldf")

# Leo la tabla generada en el trabajo practico 2
tablaEjercicio2 = read_excel("../Segunda\ Parte/Regresion-Lineal-en-R/tablaEjercicio2.xlsx")

View(tablaEjercicio2)

cantidadDeFilas = nrow(tablaEjercicio2)

Situacion_Final<-sample(c("0","1"), size = cantidadDeFilas, replace = TRUE)

# Agrego columna Situacion_Final
columnaAgregada<-cbind(tablaEjercicio2, Situacion_Final)

# Ver cuando respondan el Mail que filtro realizar exactamente
# Reemplazo los valores de la columna Situacion_Final por los correctos de acuerdo a la columna notaFinal
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal<6, 0)
columnaAgregada$Situacion_Final <- replace(columnaAgregada$Situacion_Final,columnaAgregada$notaFinal>=6, 1)

View(columnaAgregada)
