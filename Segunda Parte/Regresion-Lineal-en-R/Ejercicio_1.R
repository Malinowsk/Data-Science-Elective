# trabajando en el ejercicio 1

library("readxl")
library("sqldf")
# install.packages("lubridate")
library("lubridate")

# asignacion tabla de excel (cursadas) en variable
cursadas = read_excel("Datos excel/C.xlsx")

# filtro carrera y plan
cursadasIng2011 = subset(cursadas,carrera==206 & plan==2011)

# seleccion columnas (legajo, materia, cond_regularidad, nota)
cursadasIng2011 <- cursadasIng2011[c(2,5,9,11)]

# elimino nulos de columna nota
cursadasIng2011ColumnasSinNotasNulas <- subset(cursadasIng2011, nota != "NA")

# pensar que onda con esto
cursadasIng2011ColumnasSinNotasNulas <- subset(cursadasIng2011ColumnasSinNotasNulas, cond_regularidad != 100)

cursadasIng2011PrimerAnio <- subset(cursadasIng2011ColumnasSinNotasNulas, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )  # filtro por materias de primer año

#cursadas_filtro_3 <- subset(cursadasIng2011PrimerAnio, (Legajo > 7000) & (Legajo < 8000)  )


alumnos = read_excel("Datos excel/A.xlsx")

alumnosIngresantes2020 = subset(alumnos, carrera==206 & plan==2011 & fecha_ingreso >= ymd_hms( "2020-01-01 00:00:00" ) & fecha_ingreso <= ymd_hms( "2021-01-01 00:00:00" ))

# legajoAlumnosIngresantes2020 = alumnosIngresantes2020[8] # nos quedamos con columna legajo

# View(legajoAlumnosIngresantes2020)

cursadasPrimerAnioIngresantes2020 = sqldf(" select c.Legajo, c.materia , c.cond_regularidad , c.nota from cursadasIng2011PrimerAnio c join alumnosIngresantes2020 a on (a.legajo = c.Legajo)")

cursadasPrimerAnioIngresantes2020

# View(cursadasPrimerAnioIngresantes2020) # muestro tabla
summary(cursadasPrimerAnioIngresantes2020) # info de la tabla NO SE QUE HACE
cor(cursadasPrimerAnioIngresantes2020) # correlacion lineal , devuelve la matriz de correlacion (coeficiente a priori)

regresionLinealMultiple <- lm(nota~Legajo+cond_regularidad+materia, cursadasPrimerAnioIngresantes2020) #regresion lineal multiple
regresionLinealMultiple # se muestra los valores de los coeficientes (A,B,c,D) de la funcion y=Ax+Bz+Cw+D, siendo "y" la variable nota  
summary(regresionLinealMultiple) # varios datos entre ellos el coeficiente de determinacion (coeficiente a posteriori) "Adjusted R-squared:  0.06694" muy bajo


write.csv(x = cursadasPrimerAnioIngresantes2020, file = "tablaEjercicio1.csv",row.names = FALSE) # guardo tabla en un archivo csv para probar en infostat
