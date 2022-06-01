# trabajando en el ejercicio 1

library("readxl")
library("sqldf")

cursadas = read_excel("Datos excel/C.xlsx")        # asignacion tabla de excel (cursadas) en variable
cursadas_filtro_1 = subset(cursadas,carrera==206 & plan==2011) # filtro carrera y plan
cursadas_filtro_2 <- cursadas_filtro_1[c(2,5,9,11)]          # seleccion columnas
cursadas_filtro_3 <- subset(cursadas_filtro_2, nota != "NA") # elimino nulos de columna nota

cursadas_filtro_3 <- subset(cursadas_filtro_3, cond_regularidad != 100)

cursadas_filtro_3 <- subset(cursadas_filtro_3, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )  # filtro por materias de primer año

#cursadas_filtro_3 <- subset(cursadas_filtro_3, (Legajo > 7000) & (Legajo < 8000)  )


alumnos = read_excel("Datos excel/A.xlsx")

alumnos_filtro = subset(alumnos, carrera==206 & plan==2011) # filtro carrera y plan
alumnos_filtro <- subset(alumnos_filtro, fecha_ingreso >= ymd_hms( "2020-01-01 00:00:00" ) & fecha_ingreso <= ymd_hms( "2021-01-01 00:00:00" ))

alumnos_filtro = alumnos_filtro[c(1,8)]

View(alumnos_filtro)



cursadas_filtro_4 = sqldf(" select cursadas_filtro_3.Legajo, cursadas_filtro_3.materia , cursadas_filtro_3.cond_regularidad , cursadas_filtro_3.nota from cursadas_filtro_3 join alumnos_filtro on (alumnos_filtro.legajo = cursadas_filtro_3.Legajo)")

cursadas_filtro_4

primerAño = sqldf("select  Legajo , materia, cond_regularidad, AVG(nota) as 'nota' from cursadas_filtro_4 group by cursadas_filtro_4.Legajo , cursadas_filtro_4.Materia")


View(primerAño) # muestro tabla
summary(primerAño) # info de la tabla
cor(primerAño) # correlacion lineal , devuelve la matriz de correlacion (coeficiente a priori)

mostrar <- lm(nota~Legajo+cond_regularidad+materia,primerAño) #regresion lineal multiple
mostrar # se muestra los valores de los coeficientes (A,B,c,D) de la funcion y=Ax+Bz+Cw+D, siendo "y" la variable nota  
summary(mostrar) # varios datos entre ellos el coeficiente de determinacion (coeficiente a posteriori) "Adjusted R-squared:  0.06694" muy bajo


write.csv(x = primerAño, file = "primerAño.csv",row.names = FALSE) # guardo tabla en un archivo csv para probar en infostat


############probando comandos#####################
class(cursadas_filtro_3)
mean(cursadas_filtro_3$nota)

for (i in cursadas_filtro_3 ){
  print(i)
}


hist(primerAño$nota, main= "Notas", xlab="Notas", col="green") #hitorgrama de la columna notas
hist(primerAño$cond_regularidad, main= "Regularidad", xlab="Regularidad", col="yellow") #hitorgrama de la columna notas
hist(primerAño$Legajo, main= "Legajo", xlab="Legajo", col="blue")

boxplot(primerAño$nota, col="red")


#install.packages("vioplot")
library("vioplot")

vioplot(primerAño$nota, col="red")
vioplot(primerAño$Legajo, col="blue")


#install.packages("scatterplot3d")
library("scatterplot3d")

s3d<-scatterplot3d(primerAño[,c("nota","materia","cond_regularidad","Legajo")], type="h", highlight.3d=TRUE,angle=55, scale.y=0.7, pch=16, main="lindo quilombo")

#s3d$plane3d(reg2, lty.box="solid")

plot(primerAño)

