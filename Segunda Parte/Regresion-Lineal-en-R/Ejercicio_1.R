# ejercicio 1

library("readxl")
library("sqldf")
library("lubridate")

# asignacion tabla de excel (cursadas) en variable
cursadas = read_excel("Datos excel/C.xlsx")

View(cursadas)

# filtro carrera y plan
cursadasIng2011 = subset(cursadas,carrera==206 & plan==2011)

# seleccion columnas (legajo, materia, cond_regularidad, nota)
cursadasIng2011 <- cursadasIng2011[c(2,5,9,11)]

View(cursadasIng2011)

# elimino nulos de columna nota
cursadasIng2011ColumnasSinNotasNulas <- subset(cursadasIng2011, nota != "NA")

View(cursadasIng2011ColumnasSinNotasNulas)

# desaprobado
cursadasIng2011ColumnasSinNotasNulas$cond_regularidad <- replace(cursadasIng2011ColumnasSinNotasNulas$cond_regularidad,cursadasIng2011ColumnasSinNotasNulas$cond_regularidad==3,   2)
# insuficiente
cursadasIng2011ColumnasSinNotasNulas$cond_regularidad <- replace(cursadasIng2011ColumnasSinNotasNulas$cond_regularidad,cursadasIng2011ColumnasSinNotasNulas$cond_regularidad==100, 1)
# aprobado
cursadasIng2011ColumnasSinNotasNulas$cond_regularidad <- replace(cursadasIng2011ColumnasSinNotasNulas$cond_regularidad,cursadasIng2011ColumnasSinNotasNulas$cond_regularidad==0,   3)

View(cursadasIng2011ColumnasSinNotasNulas)

# Nos quedamos con las materias del primer año
cursadasIng2011PrimerAnio <- subset(cursadasIng2011ColumnasSinNotasNulas, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )  # filtro por materias de primer aÃ±o


##################################################################################################

# asignacion tabla de excel (alumnos) en variable
alumnos = read_excel("Datos excel/A.xlsx")

alumnosIngresantes2020 = subset(alumnos, carrera==206 & plan==2011 & fecha_ingreso >= ymd_hms( "2020-01-01 00:00:00" ) & fecha_ingreso <= ymd_hms( "2021-01-01 00:00:00" ))

View(alumnosIngresantes2020)


###################################################################################################

#filtramos haciendo un join con la tabla alumnos

cursadasPrimerAnioIngresantes2020 = sqldf(" select c.Legajo, c.materia , c.cond_regularidad , c.nota from cursadasIng2011PrimerAnio c join alumnosIngresantes2020 a on (a.legajo = c.Legajo)")


View(cursadasPrimerAnioIngresantes2020)


cor(cursadasPrimerAnioIngresantes2020) # correlacion lineal , devuelve la matriz de correlacion (coeficiente a priori)

regresionLinealMultiple <- lm(nota~Legajo+cond_regularidad+materia, cursadasPrimerAnioIngresantes2020) #regresion lineal multiple

regresionLinealMultiple # se muestra los valores de los coeficientes (A,B,c,D) de la funcion y=Ax+Bz+Cw+D, siendo "y" la variable nota  

summary(regresionLinealMultiple) # varios datos entre ellos el coeficiente de determinacion (coeficiente a posteriori) "Adjusted R-squared:  0.06694" muy bajo


write.csv(x = cursadasPrimerAnioIngresantes2020, file = "tablaEjercicio1.csv",row.names = FALSE) # guardo tabla en un archivo csv para probar en infostat
