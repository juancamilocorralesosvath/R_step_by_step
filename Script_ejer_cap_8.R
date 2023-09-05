# Los archivos con extensión .txt pueden 
# ser leídos empleando la función read.table()
# por ejemplo: 
# leyendo el archivo .txt 
# primero la ruta del archivo entre comillas, header si la informacion tiene encabezados
# sep la manera en como están separados los datos. 
# employee <- read.table("employee.txt", header = TRUE, sep = ",")
# verificando la clase del objeto
# class(employee)
# 
# inspeccionando los primeros 5 datos 
# head(employee, 5)
# inspeccionando los últimos 3 datos 
# tail(employee, 2)
# inspeccionando la clase de cada variable 
# str(employee)
# leyendo el archivo .csv
# employee2 <- read.csv("employee.csv", header = TRUE, sep = ";")
# verificando la clase del objeto
# class(employee2)


# para importar datos desde un excel:
#instalar el paquete si no lo tienes aún instalado
#install.packages("readxl")
# cargando el paquete
# library(readxl)
# leyendo datos del archivo .xlsx que está en hoja2 
# la diferencia es que con la funcion read_excel podemos especificar
# la hoja en la que se encuentra la informacion asi como el rango.
# employee3 <- read_excel("employee.xlsx", sheet = 2, 
#                        range = "C2:E62")
# verificando la clase del objeto
# class(employee3)
# a ver, hagamos el ejemplito:

#install.packages("gapminder")
library(gapminder)
data(package = "gapminder")
help(gapminder)

#y de esta manera cargamos lo que queremos al working space
data("gapminder")
class(gapminder)
help(read.csv)

spotify <- read.csv("spotify_dataset.csv", header = TRUE, sep = ",")

class(spotify)

str(spotify)


#spotify$Streams <- as.integer(gsub(",", "", spotify$Streams))


str(spotify)

# cargando las peliculas:
movies_shows <- read.table("ejercicio_modulo3.txt", header = TRUE, sep = ",")
class(movies_shows)
str(movies_shows)



