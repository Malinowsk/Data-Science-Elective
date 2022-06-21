# trabajando en el ejercicio 2

library("readxl") 
library("writexl") 
library("sqldf")

tablaEjercicio1 = read_excel("tablaEjercicio1.xlsx") # carga de datos del ejecicio 1

cursadasAprobadas = subset(tablaEjercicio1, nota >= 4) # filtro por cursadas aprobadas
View(cursadasAprobadas)

finales = read_excel("Datos excel/F.xlsx") # carga de datos del tabla finales

finalesAprobados = subset(finales, carrera == 206 & plan == 2011) # filtro aprobados, ing plan 2011
View(finalesAprobados)

cursadasYfinales = sqldf(" select c.Legajo, c.materia , c.cond_regularidad , c.nota as notaCursada, f.nota as notaFinal 
                            from cursadasAprobadas c join finalesAprobados f 
                            on (c.legajo = f.Legajo) and (c.materia = f.materia)")
View(cursadasYfinales)

cor(cursadasYfinales) # correlacion lineal, retorna la matriz de correlacion

regresionLinealMultiple <- lm(notaFinal~Legajo+cond_regularidad+materia+notaCursada, cursadasYfinales) #regresion lineal multiple

regresionLinealMultiple # se muestra los valores de los coeficientes (A,B,C,D) de la funcion y=Ax+Bz+Cw+D, siendo "y" la variable notaFinal  

summary(regresionLinealMultiple) # varios datos entre ellos el coeficiente de determinacion (coeficiente a posteriori)

write_xlsx(cursadasYfinales,"tablaEjercicio2.xlsx")
write.csv(cursadasYfinales, file = "tablaEjercicio2.csv",row.names = FALSE)


