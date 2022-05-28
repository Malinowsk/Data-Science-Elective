# trabajando en el ejercicio 2

library(readxl) # sirve? supongo que si

cursadas = read_excel("Datos excel/C.xlsx")        # asignacion tabla de excel (cursadas) en variable
cursadas_filtro_1 = subset(cursadas,carrera==206 & plan==2011) # filtro carrera y plan
cursadas_filtro_2 <- cursadas_filtro_1[c(2,5,9,11)]          # seleccion columnas
cursadas_filtro_3 <- subset(cursadas_filtro_2, nota != "NA") # elimino nulos de columna nota

primerAño <- subset(cursadas_filtro_3, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )  # filtro por materias de primer año

View(primerAño)

finales = read_excel("Datos excel/F.xlsx")        # asignacion tabla de excel (finales) en variable
finales_filtro_1 = subset(finales,carrera==206 & plan==2011) # filtro carrera y plan
finales_filtro_2 <- finales_filtro_1[c(2,3,6)]          # seleccion columnas
finales_filtro_3 <- subset(finales_filtro_2, nota != "NA")

primerAñof <- subset(finales_filtro_3, (materia == 1) | (materia == 2)  | (materia == 193)  | (materia == 145) | (materia == 4) | (materia == 5)  | (materia == 7)  | (materia == 125) | (materia == 127) )

View(primerAñof)

# juntar (primerAño con primerAñof) por columnas Legajo y materia 

# luego ver que da los coeficientes y graficas

