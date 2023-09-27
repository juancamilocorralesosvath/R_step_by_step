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
# datos de tipo factor
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

# Taller Capítulo 4

# Carga de bases de datos
# no tengo este archivo...
pob <- read.table("Pob_cinco_ciudades.txt", header = TRUE, sep = ",")

# Ejercicio 1

# a. Genera una visualización de la evolución de la población de Cali.
library(dplyr)
library(ggplot2)

ggplot(pob, aes(x = Year, y = Cali))+
  geom_line() +
  labs(title = "Evolución de la población de Cali (1985 - 2022)",
       y = "personas",
       x = "año", caption = "fuente: DANE")+
  theme_minimal()





# b. Ahora crea una visualización que permita mostrar la evolución de la población de las 5 ciudades
# para esto tenemos que hacer modificaciones en la base de datos...

bogota <- pob[, c("Year", "Bogota")]
bogota[,3] <- "Bogota"
names(bogota)[2:3] <- c("Pob" , "Ciudad")

medellin <- pob[, c("Year", "Medellin")]
medellin [,3] <- "Medellin "
names(medellin )[2:3] <- c("Pob" , "Ciudad")

cali <- pob[, c("Year", "Cali")]
cali[,3] <- "Cali"
names(cali)[2:3] <- c("Pob" , "Ciudad")

barranquilla <- pob[, c("Year", "Barranquilla")]
barranquilla [,3] <- "Barranquilla "
names(barranquilla )[2:3] <- c("Pob" , "Ciudad")

bucaramanga <- pob[, c("Year", "Bucaramanga")]
bucaramanga[,3] <- "Bucaramanga "
names(bucaramanga)[2:3] <- c("Pob" , "Ciudad")

POB <- rbind(bogota, medellin, cali, barranquilla, bucaramanga )



ggplot(POB, aes(x = Year, y = Pob, color = Ciudad)) +
  geom_line() +
  labs( title= "Evolución de la población de Cali (1985 - 2022)", 
        y = "personas", 
        x = "año",
        caption = "Fuente: DANE") +
  theme_minimal() + 
  theme(legend.position = "bottom")

# sin embargo, hay una mejor opcion de hacer esto mismo, que es menos engorrosa:

library(tidyverse)

pob %>% 
  gather(Ciudad, Poblacion, Bogota, Medellin, Cali, Barranquilla, Bucaramanga) %>% 
  ggplot(aes(x = Year, y = Poblacion, color = Ciudad))+
  geom_line()+
  labs(title = "Evolución de la población de Cali (1985 - 2022)",
       y = "personas",
       x = "año",
       caption = "Fuente: DANE")+
  theme_minimal()+
  theme(legend.position = "bottom")


# Ejercicio 2

# Ahora construye una visualización que permita mostrar cómo ha evolucionado la participación de cada una de las cinco ciudades en la población total de las cinco principales ciudades.

pob %>% 
  gather(Ciudad, Poblacion, Bogota, Medellin, Cali, Barranquilla, Bucaramanga) %>% 
  group_by(Year, Ciudad) %>% 
  summarise(sumaPob = sum(Poblacion)) %>% 
  group_by(Year) %>% 
  mutate(Prop_Poblacion = 100 * sumaPob / sum(sumaPob)) %>% 
  ggplot(aes(x = Year, y = Prop_Poblacion, fill = Ciudad))+
  geom_bar(position = "stack", stat = "identity")+
  labs(y = "% de la poblacion de las 5 principales ciudades",
       x = "año", fill = "Ciudad")+
  theme_minimal()+
  theme(legend.position = "bottom")
# preguntas:
ggplot(pob, aes(x = Year, y = Cali))+
  geom_line(col = "blue")

# capitulo 5 del libro de visualizaciones en R
# La gráfica más empleada para mostrar la relación entre dos variables es el diagrama de dispersión.
# relación entre la expectativa de vida y el PIB percápita para todos los países en 2007
gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(  x="PIB percápita", y="Esp. de vida") +
  theme_minimal()

# panel a 
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_point() + 
  labs(title = "Panel a con geom_point()", 
       x="Año", y="Esp. de vida") +  
  theme_minimal() 
# panel b
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_jitter() + 
  labs(title = "Panel b con geom_jitter()",
       x="Año", y="Esp. de vida") +  
  theme_minimal()
# la decision acerca de si usar geom_jitter o geom_point dependerá de nosotros, puesto que la funcoin geom_jitter pierde un poco de exactitud, brinda una mejor idea de como se relacionan las varibles.

# Gráfico de burbujas
# la idea es poder presentar la información de tres variables.
# cuando en la funcion decimos size, ahi asignamos la tercera variable.

gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             size = pop)) +
  geom_point(col = "royalblue2") +
  labs(  x="PIB percápita ($US)", 
         y="Esp. de vida", 
         size="Población") +
  theme_minimal()
# podemos incluir variables cualitativas en un diagrama de dispersión (variables de tipo factor)
# mapeamos el continente con el color de la pelota
gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             size = pop, col = continent )) +
  geom_point() +
  labs(  x = "PIB percápita ($US)", 
         y = "Esp. de vida", 
         size = "Población",
         col = "Continente") +
  theme_minimal()

# segundo taller del modulo 6:

# Ejercicio 1
# a. Crea una visualización que permita mostrar la relación entre las reproducciones (Streams) y la bailabilidad (Danceability) .

ggplot(spotify, aes(x = Danceability, y = Streams)) +
  geom_point() +
  labs( title= "Relación entre las reproducciones de 
        las canciones \n que han estado en el Top 200 de las 
        listas semanales de Spotify \n y su bailabilidad (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Bailabilidad") +
  theme_minimal()

# Podemos mejorar la visualización jugando un poco con el formato de los ejes.
library(scales)
ggplot(spotify, aes(x = Danceability, y = Streams)) +
  geom_point() +
  scale_y_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  scale_x_continuous(limits=c(0,NA)) +
  labs( title= "Relación entre las reproducciones de las canciones \n que han estado en el Top 200 de las listas semanales de Spotify \n y su bailabilidad (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Bailabilidad") +
  theme_minimal()
# Ahora crea visualiza para la relación entre las reproducciones (Streams) y el tempo (Tempo).
ggplot(spotify, aes(x = Tempo, y = Streams)) +
  geom_point() +
  scale_y_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  scale_x_continuous(limits=c(0,NA)) +
  labs( title= "Relación entre las reproducciones de las canciones \n que han estado en el Top 200 de las listas semanales de Spotify \n y su tempo (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Tempo") +
  theme_minimal()

# Ahora crea visualiza para la relación entre las reproducciones (Streams) y la duración (Duration..ms.). 
ggplot(spotify, aes(x = Duration..ms., y = Streams)) +
  geom_point() +
  scale_y_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  scale_x_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  labs( title= "Relación entre las reproducciones de las canciones \n que han estado en el Top 200 de las listas semanales de Spotify \n y su duración (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Duración (milisegundos)") +
  theme_minimal()

# la parte de limits no lo entiendo muy bien...scale_y_continuous?

# Crea una visualiza para la relación entre las reproducciones (Streams), la duración (Duration..ms.) y tempo (Tempo).
ggplot(spotify, aes(x = Duration..ms., y = Streams, size = Tempo)) +
  geom_point() +
  scale_y_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  scale_x_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  labs( title= "Relación entre las reproducciones de las canciones \n que han estado en el Top 200 de las listas semanales de Spotify, \n su duración y tempo (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Duración (milisegundos)") +
  theme_minimal()

# Finalmente, crea una visualiza para la relación entre las reproducciones (Streams), su popularidad (Popularity) y el acorde (Chord).
ggplot(spotify, aes(x = Popularity, y = Streams, col = Chord)) +
  geom_point() +
  scale_y_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  scale_x_continuous(limits=c(0,NA), labels = comma_format(big.mark = ".",
                                                           decimal.mark = ",")) +
  labs( title= "Relación entre las reproducciones de las canciones \n que han estado en el Top 200 de las listas semanales de Spotify, \n su popularidad y el acorde (2020 y 2021)", 
        y = "Reproducciones", 
        x = "Duración (milisegundos)") +
  theme_minimal() + theme(legend.position = "bottom")

ggplot(spotify, aes(x = Danceability, y = Tempo, size = Duration..ms.,col = Chord))+
  geom_point()
# capitulo 6: recomendaciones para mejorar una visualización
# 
#¿Cuál es el mensaje que queremos comunicar?
#  ¿Qué variables de las disponibles permiten comunicar el mensaje?
#  ¿Quién es mi audiencia?

# 6.1 Ordena los datos
# Por defecto, las visualizaciones que contienen una variable de clase character o factor, suelen ordenarse alfabéticamente
gapminder %>%
  filter(year == 2007, continent == c("Americas")) %>%
  ggplot( aes(x=country, y=gdpPercap) ) +
  geom_bar(stat="identity") +
  labs(x = "país", y = "PIB percápita ($US)") +
  theme_minimal()
# Primero, volteemos los ejes en la capa de 
#Coordenadas con la función coord_flip() . Esto le dará 
#más espacio a los nombres de los países. Y lo segundo 
#que podemos 
#hacer es ordenar los países por su PIB percápita
gapminder %>%
  filter(year == 2007, continent == c("Americas")) %>%
  arrange(gdpPercap) %>%
  mutate(country=factor(country, country)) %>%
  ggplot( aes(x=country, y=gdpPercap) ) +
  geom_bar(stat="identity") +
  labs(x = "País", y = "PIB percápita ($US)") +
  coord_flip() +
  theme_minimal()
# 6.2 Ten cuidado con los colores que no comunican
install.packages("gghighlight")
library(gghighlight)
BLUE1 <- '#174A7E'
# el siguiente es un buen uso de los colores: resaltando una
# sola barras estamos siendo muy claros del mensaje que queremos dar.
# la conclusiòn es que debemos tener mucho cuidado a la hora de seleccionar los colores que vamos a usar.
library(ggplot2)
library(dplyr)

gapminder %>%
  filter(year == 2007, continent == c("Americas")) %>%
  arrange(gdpPercap) %>%
  mutate(country=factor(country, country)) %>%
  ggplot( aes(x=country, y=gdpPercap, fill=country) ) +
  geom_bar(stat="identity", fill = BLUE1) +
  labs(x = "País", y = "PIB percápita ($US)", 
       color = "País") +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  gghighlight(country == 'Colombia', use_direct_label = F) 

# 6.3 Evita los gráficos Spaghetti
# los graficos de lineas son muy buenos. sin embargo, cuando tenemos muchas variables,
# es muy facil confundirse...termina asemejandose a un plato de spaghettis

# atento a la funcion geom_label() me permite poner texto en el grafico en las coordenadas que le indique.
gapminder %>%
  filter(continent == c("Americas")) %>%
  ggplot( aes(x = year, y = gdpPercap, group = country )) +
  geom_line(col= BLUE1) +
  labs(x = "Año", y = "PIB percápita ($US)", 
       color = "País") +
  geom_label( x=1999, y=15000, 
              label="Colombia: leve \n caída en 1999", 
              size=4, color = BLUE1) +
  theme_minimal() + 
  theme(legend.position="none") +  
  gghighlight(country == 'Colombia', use_direct_label = F) 

# 6.4 No uses gráficos de torta
# Una alternativa para presentar los datos con mayor claridad es el gráfico de barras (ver Sección 3.2) con

# --------------
# capitulo 7: haciendo graficos interactivos

# se guarda el gráfico en un objeto
g1 <- gapminder %>% 
  filter(year == 2007) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp,
             size = pop, col = continent )) +
  geom_point() +
  labs(  x = "PIB percápita ($US)", 
         y = "Esp. de vida", 
         size = "Población",
         col = "Continente") +
  theme_minimal() + 
  theme(legend.position = "bottom")

# hora podemos animar este gráfico empleando solo una línea de código. La función ggplotly() del paquete plotly (Sievert, 2020) solo requiere como argumento un objeto de clase ggplot
# instala el paquete si no o tienes
install.packages("plotly")
# cargamos el paquete
library(plotly)
# convertimos a interactivo el gráfico
ggplotly(g1)


# el codigo no funciona y es debido a que la variable country_colors
# no existe. Yo creo que esta variable la construyo el que haya realizado el 
# codigo para la grafica. debio haber determinado unos colores para los paises, la pregunta es como?
library(ggplot2)
library(dplyr)
install.packages("gganimate")
library(gganimate)
g.anidado <- ggplot(gapminder, aes(x = gdpPercap, y= lifeExp, 
                                   size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~continent) +
  theme_minimal() +
  labs(title = 'Año: {frame_time}',
       x = 'PIB percápita', 
       y = 'Esperanza de vida (años)') +
  transition_time(year) +
  ease_aes('linear')

g.anidado 

# ultimo cuestionario. final final xd

ggplot(mtcars, aes(x = am, y = qsec))+ 
  geom_boxplot(fill = "lightblue")


gapminder %>% 
  filter(year == 2002) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap))+
  geom_point(col = "lightblue")+
  theme_minimal()
  

save.image(file = "26septiembre.RData")














