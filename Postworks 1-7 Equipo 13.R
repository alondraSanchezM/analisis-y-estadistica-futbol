
library(lubridate)    # Trabajar con la fechas facilmente
library(dplyr)        # Funciones select y mutate
library(ggplot2)      # Graficar las probabilidades
library(plotly)       # Graficas interactivas que permitan ver mejor el valor
library(RColorBrewer) # Color del Heatmap
library(rsample)      # Uso de función bootstrap
library(fbRanks)      # Uso de la función create.fbRanks.dataframes
library(mongolite)    # Conexión de MongoDB con R 

# ------------------------------------------------------------------------------

#POSTWORK 1

#Importación de los datos.
datosLiga <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Extracción de las columnas FTHG y FTAG.
datosLiga <- datosLiga[ ,c(6:7)]

#Tablas de frecuencias relativas.
(freqLocal <- table(datosLiga$FTHG))
(freqVisitante <- table(datosLiga$FTAG))
(freqAmbos <- table(datosLiga))

#Probabilidad (marginal) de que el equipo que juega en casa anote x goles.
(margLocal <- freqLocal / sum(freqLocal))

#Probabilidad (marginal) de que el equipo que juega como visitante anote x goles.
(margVisitante <- freqVisitante / sum(freqVisitante))

#Probabilidad (conjunta) de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles.
(conjAmbos <- freqAmbos/sum(freqAmbos))

# A partir de los datos resultados obtenidos se observa lo siguiente: 
#   - Los resultados más probables son los que involucran menos goles, a apartir
#     de 2 goles las probabilidades disminuyen considerablemente. 
#   - Los 3 resultados más probables son: empate a 1-1, victoria del local 1-0 y
#     victoria del local 2-0. 
#   - En general, la victoria del equipo local es más probable. 

# Entonces, ¿el resultado de un partido es influenciado por quién juega de local y de
# visitante? 

# Con base en estas gráficas, se posible plantear la siguiente hipótesis:
# "El ser local influye en el resultado de un partido". 

barplot(margLocal, main="Probabilidad marginal de goles/nanotados equipo local",
        xlab="Número de goles",
        ylab="Probabilidad",
        col.main="black", col.lab="chocolate4")

barplot(margVisitante, main="Probabilidad marginal de goles/nanotados equipo visitante",
        xlab="Número de goles",
        ylab="Probabilidad",
        col.main="black", col.lab="chocolate4")


# ------------------------------------------------------------------------------

# Es necesario considerar una mayor cantidad de datos para comprobar la hipótesis.
# Por ello, se prepara un dataset con datos de 3 temporadas de la liga española de 
# futbol el cual se utilizará en los postworks 3 y 4. 

#POSTWORK 2

#Importación de los datos.
Liga1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
Liga1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
Liga1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Estructura de los dataframes.
str(Liga1718);head(Liga1718);View(Liga1718);summary(Liga1718)
str(Liga1819);head(Liga1819);View(Liga1819);summary(Liga1819)
str(Liga1920);head(Liga1920);View(Liga1920);summary(Liga1920)

#Selección de las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR.
Liga1718<-select(Liga1718,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR)
Liga1819<-select(Liga1819,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR)
Liga1920<-select(Liga1920,Date,HomeTeam,AwayTeam,FTHG,FTAG,FTR)

#Asignación a la columna Fecha la clase Date.
Liga1718<-mutate(Liga1718,Date=dmy(Liga1718$Date))
Liga1819<-mutate(Liga1819,Date=dmy(Liga1819$Date))
Liga1920<-mutate(Liga1920,Date=dmy(Liga1920$Date))

#Creación de un único data frame.
Ligas1720<-rbind(Liga1718,Liga1819,Liga1920)
View(Ligas1720)

#Guardar data frame.
#write.csv(Ligas1720, "Ligas1720.csv",row.names = FALSE)


# ------------------------------------------------------------------------------

#POSTWORK 3

# Se realizará el mismo procedimiento del postwork 1, sin embargo, ahora se involucra
# Una mayor cantidad de datos. 

#Importación de los datos. Mismo Data Frame del postwork2
#Ligas1720 <- read.csv("https://raw.githubusercontent.com/AEscajeda/Programacion-y-Estadistica-con-R/main/Ligas1720.csv")

#Tablas de frecuencias relativas.
(freqLocal<-table(Ligas1720$FTHG))
(freqVisitante<-table(Ligas1720$FTAG))
(freqAmbos<-table(Ligas1720$FTAG,Ligas1720$FTHG))

#Probabilidad (marginal) de que el equipo que juega en casa anote x goles.
(margLocal <- freqLocal / sum(freqLocal))
# Comprobando que la suma de las probabilidades = 1.
sum(margLocal)

#Probabilidad (marginal) de que el equipo que juega como visitante anote x goles.
(margVisitante <- freqVisitante / sum(freqVisitante))
# Comprobando que la suma de las probabilidades = 1.
sum(margVisitante)

#Probabilidad (conjunta) de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles.
(conjAmbos<-freqAmbos/sum(freqAmbos))
# Comprobando que la suma de las probabilidades = 1.
sum(conjAmbos)

#Gráfico de barras para las probabilidades marginales estimadas del número de
#goles que anota el equipo de casa.
DFmargLocal <-as.data.frame(margLocal)
colnames(DFmargLocal)[1] <- "No.goles"
colnames(DFmargLocal)[2] <- "Probabilidad"
margLocalGraf <- DFmargLocal %>%
  ggplot() + 
  aes(x = No.goles, y = Probabilidad) +
  geom_bar( col = "black", fill = "aquamarine3", stat = "identity") + 
  ggtitle("Gráfica de barras: Probabilidad marginal FTHG") + 
  ylab("Probabilidad") + 
  xlab("Número de goles") + 
  theme_minimal()
margLocalGraf
#Gráfico interactivo para observar mejor los valores.
ggplotly(margLocalGraf)

#Gráfico de barras para las probabilidades marginales estimadas del número de
#goles que anota el equipo visitante.
DFmargVisitante <-as.data.frame(margVisitante)
colnames(DFmargVisitante)[1] <- "No.goles"
colnames(DFmargVisitante)[2] <- "Probabilidad"
margVisitanteGraf <- DFmargVisitante %>%
  ggplot() + 
  aes(x = No.goles, y = Probabilidad) +
  geom_bar( col = "black", fill = "aquamarine3", stat = "identity") + 
  ggtitle("Gráfica de barras: Probabilidad marginal FTAG") + 
  ylab("Probabilidad") + 
  xlab("Número de goles") + 
  theme_minimal()
margVisitanteGraf
#Gráfico interactivo para observar mejor los valores.
ggplotly(margVisitanteGraf)

#HeatMap para las probabilidades conjuntas estimadas de los números de goles 
#que anotan el equipo de casa y el equipo visitante.
DFconjAmbos <- as.data.frame(conjAmbos)
colnames(DFconjAmbos)[1] <- "Visitante"
colnames(DFconjAmbos)[2] <- "Local"
colnames(DFconjAmbos)[3] <- "Probabilidad"
conjAmbosGraf <- DFconjAmbos %>%
  ggplot() + 
  aes(x = Local, y = Visitante, fill=Probabilidad) +
  geom_tile() + 
  ggtitle("Gráfica de calor: Probabilidad conjunta FTHG-FTAG") + 
  xlab("Local (FTHG)") + 
  ylab("Visitante (FTAG)") + 
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal()
conjAmbosGraf
#Gráfico interactivo para observar mejor los valores.
ggplotly(conjAmbosGraf)

# --- Resultados:

# En el caso del equipo local, se observa que es más probable que el equipo anote 
# uno o dos goles a que no anote ninguno. A más de dos goles, la probabilidad sigue una
# tendencia descendente. 

# En el caso del equipo visitante, se observa que es un poco más probable que el equipo
# no anote a que anote 1 gol. Para 2 y más goles, la probabilidad sigue una tendencia
# descendente. 

# Con el heatmap, se observa que los resultados más probables son aquellos que 
# involucran hasta 2 goles, es decir, 0-0, 1-0, 0-1, 1-1, 2-1, 1-2, 2-2 donde los
# 3 con mayor probabilidad son 1-0, 1-1, 2-1

# Retomando la hipótesis planteada, parece ser que hay un efecto a favor del equipo
# local en los resultados. 


# ------------------------------------------------------------------------------

#POSTWORK 4

#Tabla de cocientes
Multmarg <- outer(X = margVisitante, Y = margLocal, FUN = "*")
(Cocientes <- conjAmbos/Multmarg)

# La teoría de probabilidad nos dice que si el resultado de los cocientes es 1 
# se tiene independencia de las variables. Si se quiere comprobar con mayor seguridad
# la independencia de las variables, es necesario incrementar el tamaño de la muestra.

#Procedimiento de bootstrap: Nuevas muestras de los goles.
Goles<-subset(Ligas1720,select = c(4,5))
set.seed(123)
Muestras<-bootstraps(Goles,1000)

#Listas para almacenar los dataframes y las tablas de cocientes.
ListaMuestras<-list()
ListaCocientes<-list()

#Vectores para almacenar los valores más comunes: marcadores 0-0, 1-0 y 0-1.
valor00=numeric(1000)
valor10=numeric(1000)
valor01=numeric(1000)

for (i in 1:1000){
  #Cada dataframe se coloca en una lista.
  ListaMuestras[[i]]=as.data.frame(Muestras$splits[[i]])
  #Calculo de tablas de cocientes y almacenamiento en una lista.
  ListaCocientes[[i]]=(t(prop.table(table(ListaMuestras[[i]])))/+
                         t(outer(table(ListaMuestras[[i]]$FTHG)/length(ListaMuestras[[i]]$FTHG),+
                                   table(ListaMuestras[[i]]$FTAG)/length(ListaMuestras[[i]]$FTAG)))) 
  
  #Extracción de los valores más comunes: marcadores 0-0 [1], 1-0 [8] y 0-1 [2]
  valor00[i]=ListaCocientes[[i]][1]
  valor10[i]=ListaCocientes[[i]][8]
  valor01[i]=ListaCocientes[[i]][2]
}

#Gráficas de la distribución de medias:
(hist00<-as.data.frame(valor00) %>% ggplot(aes(valor00)) +
    geom_histogram(bins=50,colour="black",fill="steelblue") +
    ggtitle("Distribución de cocientes\npara marcador de 0-0") +
    xlab("Medias") + 
    ylab("Frecuencia") + 
    geom_vline(aes(xintercept=mean(valor00)),color="orangered1", linetype="dashed", size=1) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal())
#Gráfico interactivo para observar mejor los valores.
ggplotly(hist00)

(hist10<-as.data.frame(valor10) %>% ggplot(aes(valor10)) +
    geom_histogram(bins=50,colour="black",fill="steelblue") +
    ggtitle("Distribución de cocientes\npara marcador de 1-0") +
    xlab("Medias") + 
    ylab("Frecuencia") + 
    geom_vline(aes(xintercept=mean(valor10)),color="orangered1", linetype="dashed", size=1) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal())
#Gráfico interactivo para observar mejor los valores.
ggplotly(hist10)

(hist01<-as.data.frame(valor01) %>% ggplot(aes(valor01)) +
    geom_histogram(bins=50,colour="black",fill="steelblue") +
    ggtitle("Distribución de cocientes\npara marcador de 0-1") +
    xlab("Medias") + 
    ylab("Frecuencia") + 
    geom_vline(aes(xintercept=mean(valor01)),color="orangered1", linetype="dashed", size=1) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal())
#Gráfico interactivo para observar mejor los valores.
ggplotly(hist01)  

# ¿En cuáles casos parece razonable suponer que los cocientes de la tabla
# en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
# de las variables aleatorias X y Y)?

#Respuesta:
#   Después de hacer el proceso de bootstraping varias veces y comparar los cocientes
#   resultantes con los cocientes obtenidos a partir de los datos originales, se
#   observó el siguiente compartamiento:
#   - Cuando la cantidad de goles de "x" y "y" en un partido es pequeño, el valor del
#     cociente tiende a ser cercano a uno, mientras que,
#   - a medida de que los goles de "x" y "y" por partido van incrementando, el valor
#     del cociente se va haciendo cada vez mas volátil, alejándose del valor uno. 
#     Por ejemplo, cuando se tienen 8 goles por parte de un equipo, el coeficiente suele ser 
#     o muy pequeño (en el orden de 0.001) o muy grande (mayor a 2).

# Entonces, en los casos que se considera que los cocientes de la tabla del punto 1
# tienen un valor de 1, son los marcadores de 0-0, 1-0 y 0-1, e incluso se pueden considerar
# los marcadores cercanos.

# Una prueba de Chi-cuadrada permite comprobar la independencia de 2 variables, si
# la p estimada por la prueba es menor al valor crítico (0.05) se rechaza que sea 
# independientes
suppressWarnings(chisq.test(freqAmbos))

# Como p=0.8196>p=0.05, asumimos que hay independencia.
# (El warning indica que hay frecuencias muy pequeñas en algunos resultados)

# ¿Qué implica la independencia? 
# Implica que los goles, ya sea del equipo local o visitante, no se hacen más o menos probables 
# en función de los goles anotados durante en el intervalo de 90 minutos, es decir, 
# lo que dura el partido. 


# ------------------------------------------------------------------------------

#POSTWORK 5

#Selección de las columnas Date, HomeTeam, HomeScore, AwayTeam y AwayScore.
Liga1718<-select(Liga1718,Date,HomeTeam,FTHG,AwayTeam,FTAG)
Liga1819<-select(Liga1819,Date,HomeTeam,FTHG,AwayTeam,FTAG)
Liga1920<-select(Liga1920,Date,HomeTeam,FTHG,AwayTeam,FTAG)

#Creación de un único data frame.
soccer<-rbind(Liga1718,Liga1819,Liga1920)
names(soccer)<-c("date","home.team","home.score","away.team","away.score")
View(soccer)

#Guardar data.
#setwd("~/RFiles/Curso BEDU/Posworks")
write.csv(soccer, "soccer.csv",row.names = FALSE)

#Importación del archivo soccer.csv y creación de variables.
listasoccer <- create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

#Creación de vector de fechas de partidos sin repetir.
fecha<-unique(anotaciones$date)
n <- length(fecha)

#Ranking de equipos desde la fecha inicial hasta la penúltima fecha.
ranking <- rank.teams(scores = anotaciones, teams = equipos, 
                      max.date = fecha[n-1], min.date = fecha[1])

#Estimación de los partidos que se jugaron en la última fecha.
predict(ranking, date = fecha[n])

# La función rank.teams replica el trabajo realizado por Dixon y Coles, utiliza  
# un modelo de poisson que toma en consideración: el efecto de local, el performance
# reciente, el poder de ataque y de defensa y la capacidad de los equipos con los que
# han jugado para rankear a los equipos.

# En los resultados del modelo podemos ver que, en algunos casos, el modelo predice de
# manera exitosa los resultados, sin embargo, tiene sus errores. Estos pueden deberse
# a factores no considerados como: la lesión de un jugador, cambios en alineaciones o
# de entrenador, etc. 

# Retomando la hipótesis, podemos decir que la condición de local influye, sin embargo, 
# hay que considerar otros factores como los que considera el modelo, por ejemplo, 
# no esperariamos que el Barcelona pierda un partido de visita contra el Malaga solo por
# el hecho de ser visitante. Se deben considerar también los equipos que juegan y sus
# capacidades. 


# ------------------------------------------------------------------------------

#POSTWORK 6

#Importación de los datos.
match.data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

#Creación de columna con suma de goles por partido.
match.data <- mutate(match.data, sumagoles = match.data$home.score + match.data$away.score)

#Media por mes de los goles por partido.
match.data$date <- as.Date(match.data$date)
match.data$mes <- format(match.data$date, format = "%m")
match.data$anio <- format(match.data$date, format = "%Y")
promedio.mes <- aggregate(sumagoles ~ mes + anio,match.data, mean)
colnames(promedio.mes)[3] <- "promedio.goles"

# Al analizar los datos, se observa que no hay futbol durante los meses de Junio 
# y Julio, sin tomar en cuenta el año 2020, que fue muy irregular. 
# Por ello, la serie de tiempo debe tener una frecuencia de 10, en vez de 12, 
# pues solo hay futbol 10 meses al año.
# Sin embargo, en el año 2013 hubo futbol en Junio. Para facilitar la creación de 
# la serie de tiempo, se eliminaron los datos de Junio 2013.
promedio.mes <- promedio.mes[-c(31),]

#Modificar formato de fecha para permitir la creación de la Serie de Tiempo
promedio.mes$fecha <- paste("01",promedio.mes$mes,promedio.mes$anio)
promedio.mes$fecha <- dmy(promedio.mes$fecha)
promedio.mes <- select(promedio.mes, fecha, promedio.goles)

#Creación de serie de tiempo
ts.promedio.goles <- ts(round(promedio.mes$promedio.goles,3), start = c(2010,08), end = c(2019,12), frequency = 10)

#Gráfica de la serie de tiempo
plot(ts.promedio.goles, xlab = "Tiempo", ylab = "Promedio de goles", main = "Serie de Promedio de goles por mes",
     sub = "Serie mensual: Agosto 2010 a Diciembre 2019", col = "plum4", lwd = 2)

#Análisis de componentes de la serie de tiempo
componentes <- decompose(ts.promedio.goles, type = "multiplicative")
plot (componentes, xlab = "Tiempo", sub = "Descomposición de los datos")
Tendencia <- componentes$trend
Estacionalidad <- componentes$seasonal
Aleatorio <- componentes$random
plot(ts.promedio.goles, 
     xlab = "Tiempo", main = "Datos de Promedio de goles por mes", 
     ylab = "Promedio de goles por mes", lwd = 1,
     sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")
lines(Tendencia, lwd = 2, col = "blue")

# OBSERVACIONES: 
# - El mes con menor promedio de goles fue agosto del 2015, en el inicio de la temporada 2015-2016.
# - El promedio de goles más alto fue octubre del 2016. Durante la primera mitad de la temporada
# 2016-2017, la liga española fue la que mayor promedio goleador tuvo en toda Europa.

# Así mismo, se observan periodos en los cuales el promedio de goles disminuye e incrementa.

# - Entre los años 2015-2016 el promedio de goles disminuyo notablemente 
# a comparacion de los años anteriores. Con un promedio de 1.75.
min(ts.promedio.goles)

# - Entre los años 2016 y 2018, se alcanzó un promedio de hasta 3.38 goles.
max(ts.promedio.goles)

# ------------------------------------------------------------------------------

#POSTWORK 7

#Conexión con MongoDb mediante localhost usando mongodb community server.
mongo_conn <- mongo(collection = "match", db = "match_games",
                    url = "mongodb://localhost:27017",
                    verbose = FALSE,
                    options = ssl_options())

#Conocer el número de documentos que se tiene.
mongo_conn$count()

#Observando la colección
all.collection <- mongo_conn$find('{}')
print(all.collection)

#Consulta del número de goles que metió el Real Madrid el 20 de diciembre de 2015 y contra que equipo jugó. 
mongo_conn$find('{"date":"2015-12-20", "$or": [ { "home.team": "Real Madrid" }, { "away.team" : "Real Madrid" }]}')

# Observaciones: 

# El Real madrid metió 10 goles siendo local, jugó contra Vallecano y resultó ganador.

# Como desconocíamos si el Real Madrid jugaba como local o visitante, ocupamos el or
# dentro de la agregación para que nos devolviera cualquiera de los dos

#Cerrar conexión
mongo_conn$disconnect(gc = TRUE)
rm(conetion)

# Importante comentar que es posible realizar dicha conexión con un servidor remoto de la siguiente forma:
# mongo_conn <- mongo(collection = "match", db = "match_games",
#                     url = "mongodb+srv://<username>:<password>@cluster0.adore.mongodb.net/match_games",
#                     verbose = FALSE,
#                     options = ssl_options())
# Añadiendo además a la URL el usuario y la constraseña correspondiente.
# Para los fines del postwork se realizó haciendo uso del localhost.


# ------------------------------------------------------------------------------

#POSTWORK 8

# Para el postwork 8 se genera un dashboard en un solo archivo app.R. 
# Link al archivo: https://github.com/AEscajeda/Programacion-y-Estadistica-con-R/tree/main/Postwork%208
