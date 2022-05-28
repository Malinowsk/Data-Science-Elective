# trabajando en el ejercicio 1

library(readxl) # sirve? supongo que si

cursadas = read_excel("Datos excel/C.xlsx")        # asignacion tabla de excel (cursadas) en variable
cursadas_filtro_1 = subset(cursadas,carrera==206 & plan==2011) # filtro carrera y plan
cursadas_filtro_2 <- cursadas_filtro_1[c(2,5,9,11)]          # seleccion columnas
cursadas_filtro_3 <- subset(cursadas_filtro_2, nota != "NA") # elimino nulos de columna nota

primerAño <- subset(cursadas_filtro_3, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )  // filtro por materias de primer año

View(primerAño) # muestro tabla
summary(primerAño) # info de la tabla
cor(primerAño) # correlacion lineal

write.csv(x = primerAño, file = "primerAño.csv",row.names = FALSE) # guardo tabla en un archivo csv para probar en infostat


############probando comandos#####################


hist(primerAño$nota, main= "Notas", xlab="Notas", col="green") #hitorgrama de la columna notas
hist(primerAño$cond_regularidad, main= "Regularidad", xlab="Regularidad", col="yellow") #hitorgrama de la columna notas
hist(primerAño$Legajo, main= "Legajo", xlab="Legajo", col="blue")

boxplot(primerAño$nota, col="red")


#install.packages("vioplot")
library("vioplot")

vioplot(primerAño$nota, col="red")
vioplot(primerAño$Legajo, col="blue")
