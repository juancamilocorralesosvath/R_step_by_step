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

# resulta que estas variables debian ser de tipo factor. No tengo claro el por qué.
movies_shows$Netflix <- as.factor(movies_shows$Netflix)
movies_shows$Hulu <- as.factor(movies_shows$Hulu)
movies_shows$Prime.Video <- as.factor(movies_shows$Prime.Video)
movies_shows$Disney. <- as.factor(movies_shows$Disney.)
movies_shows$Type <- as.factor(movies_shows$Type)
# y este por que numeric?
movies_shows$Rotten.Tomatoes <- as.numeric(gsub("/100", "", movies_shows$Rotten.Tomatoes))

str(movies_shows)
# escribirle al monitor: ¿por qué en este caso se trataba de factors?

preguntasMod3 <- read.csv("archivo_datos.csv", header=TRUE, sep = ",")

class(preguntasMod3)

str(preguntasMod3)


examencito <- read.csv("archivo4.csv", header=TRUE, sep = ",")
class(examencito)
install.packages("MASS")
library(MASS)
data(package = "MASS")
str(Boston)
str(Animals)
# guardar el workspace:
#save.image(file = "Ws_ejer_cap_8.RData")

#4 Geometrías para mostrar evolución
library(ggplot2)
library(dplyr)
library(gapminder)

# se emplea el operador pipe para 
# pasar los datos y filtrar los 
# datos


# Gráfico de líneas
gapminder %>%
  filter(country == "Colombia") %>%
  ggplot( aes(x = year, y =gdpPercap)) +
  geom_line() +
  labs( y="PIB percápita ($US)", 
        x="Año") +
  theme_minimal()

# ahora, que pasa si queremos incluir otros pa+ises ára comparar sus evoluciones
# la variable pais es de clase factor.
gapminder %>%
  filter(country %in% c("Colombia", "Peru", "Chile", "Mexico")) %>%
  ggplot( aes(x = year, y =gdpPercap, color = country)) +
  geom_line() +
  labs( y="PIB percápita ($US)", 
        x="Año", color = "País") +
  theme_minimal()

# Barras agrupadas

# Las barras agrupadas permiten comparar la evolución de la composición en el tiempo de una variable cualitativa de clase factor.

d1 <- gapminder %>% 
  # se agrupan los casos por año y continente
  group_by(year, continent) %>% 
  # se cuentan los países por cada continente, para cada año
  # frecuencia
  summarise( paises = n()) %>% 
  # se crea la nueva variable con la frecuencia relativa
  mutate(Prop_paises = 100 * paises / sum(paises))

glimpse(d1)
# en la capa de datos usamos el objeto d1. en la capa de aesthetics mapeamos los años al eje horizontal y en el eje vertical la participacion porcentual de cada continente
# el relleno de las barras corresponde al respectivo continente.
# position = dodge quiere decir que las barras estaran una al lado de la otra
# stat = identity es para mapear al eje vertical los valores de una columna (y = prop_paises)
ggplot(d1, aes(x = as.character(year), y = Prop_paises, fill = continent))+
    geom_bar(position = "dodge", stat = "identity")+
    labs(y = "% del numero de paises total",
         x = "año", fill = "Continente")+
    theme_minimal()
# El gráfico de barras agrupadas también puede ser empleado para mostrar la evolución de una variable cuantitativa y una cualitativa. Por ejemplo, podemos ver cómo ha cambiado la participación en la población mundial de cada continente

d2 <- gapminder %>% 
  group_by(year, continent) %>% 
  summarise( Poblacion = sum(pop)) %>% 
  group_by(year) %>% 
  mutate(Prop_Poblacion = 100 * Poblacion / sum(Poblacion))

ggplot(d2, aes(x = as.character(year), 
               y = Prop_Poblacion, fill = continent))+
  geom_bar(position = "dodge", stat="identity")+
  labs(y = "% de la población mundial", 
       x = "Año", fill = "Continente")+
  theme_minimal()

# Columnas apiladas
# 1
ggplot(d1, aes(x = as.character(year), y = Prop_paises, fill = continent))+
  geom_bar(position = "stack", stat = "identity")+
  labs(y = "% del numero de paises total",
       x = "año", fill = "Continente")+
  theme_minimal()
# 2
ggplot(d2, aes(x = as.character(year), 
               y = Prop_Poblacion, fill = continent))+
  geom_bar(position = "stack", stat="identity")+
  labs(y = "% de la población mundial", 
       x = "Año", fill = "Continente")+
  theme_minimal()


















