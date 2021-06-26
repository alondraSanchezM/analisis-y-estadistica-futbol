#Librerias
library(lubridate) # Para trabajar con la fechas facilmente
library(dplyr) # Para funciones select y mutate
library(ggplot2) # Para graficar las probabilidades
library(plotly) # Para graficas interactivas que permitan ver mejor el valor
library(RColorBrewer) # Para el color del Heatmap

# ------------------------------------------------------------------------------

#POSTWORK 1

#Importación de los datos.
datosLiga <-  read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

#Extracción de las columnas FTHG y FTAG.
datosLiga<- datosLiga[ ,c(6:7)]

#Tablas de frecuencias relativas.
(freqLocal<-table(datosLiga$FTHG))
(freqVisitante<-table(datosLiga$FTAG))
(freqAmbos<-table(datosLiga))

#Probabilidad (marginal) de que el equipo que juega en casa anote x goles.
(margLocal <- freqLocal / sum(freqLocal))

#Probabilidad (marginal) de que el equipo que juega como visitante anote x goles.
(margVisitante <- freqVisitante / sum(freqVisitante))

#Probabilidad (conjunta) de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles.
(conjAmbos<-freqAmbos/sum(freqAmbos))

# ------------------------------------------------------------------------------

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

#Importación de los datos. Mismo Data Frame del postwork2
Ligas1720 <- read.csv("https://raw.githubusercontent.com/AEscajeda/Programacion-y-Estadistica-con-R/main/Ligas1720.csv")

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

# ------------------------------------------------------------------------------

#POSTWORK 4

#Tabla de cocientes
Multmarg <- outer(X = margVisitante, Y = margLocal, FUN = "*")
(Cocientes <- conjAmbos/Multmarg)

#Procedimiento de bootstrap
GolesLocal <- sample(Ligas1720$FTHG,length(Ligas1720$FTHG),replace = TRUE)
GolesVisitante <- sample(Ligas1720$FTAG,length(Ligas1720$FTAG),replace = TRUE)

#Probabilidades de nuevos datos:
#Probabilidad marginal Local
freqLocal1 <- table(GolesLocal)
(margLocal1 <- freqLocal1 / sum(freqLocal1))
# Comprobando que la suma de las probabilidades = 1.
sum(margLocal1)

#Probabilidad marginal Visitante
freqVisitante1 <- table(GolesVisitante)
(margVisitante1 <- freqVisitante1 / sum(freqVisitante1))
# Comprobando que la suma de las probabilidades = 1.
sum(margVisitante1)

#Probabilidad conjunta
freqAmbos1 <- table(GolesVisitante, GolesLocal)
(conjAmbos1 <- freqAmbos1 / sum(freqAmbos1))
# Comprobando que la suma de las probabilidades = 1.
sum(conjAmbos1)

#Tabla de cocientes de nuevos datos
Multmarg1 <- outer(X = margVisitante1, Y = margLocal1, FUN = "*")
(Cocientes1 <- conjAmbos1/Multmarg1)

#Comparación entre cocientes originales y cocientes de nuevos datos
Cocientes; Cocientes1

#Distribución de los cocientes
par(mfrow = c(1, 2))
hist(Cocientes, main = "Histograma de\nCocientes originales", xlab = "Cocientes originales", ylab = "Frecuencia", col = "tan3")
hist(Cocientes1, main = "Histograma de\nCocientes nuevos", xlab = "Cocientes nuevos", ylab = "Frecuencia", col = "rosybrown3")
#   Al analizar los histogramas de los cocientes, se observa que estos cocientes tienden 
#   a una distribución de tipo exponencial, cuando se grafican de manera individual.  

# ¿En cuáles casos parece razonable suponer que los cocientes de la tabla
# en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
# de las variables aleatorias X y Y)?
# Respuesta:
#   Después de hacer el proceso de bootstraping varias veces y comparar los cocientes
#   resultantes con los cocientes obtenidos a partir de los datos originales, se
#   observó el siguiente compartamiento:
#   - Cuando la cantidad de goles de "x" y "y" en un partido es pequeño, el valor del
#     cociente tiende a ser cercano a uno, mientras que,
#   - a medida de que los goles de "x" y "y" por partido van incrementando, el valor
#     del cociente se va haciendo cada vez mas volátil, alejándose del valor uno. 
#     Por ejemplo, cuando se tienen 8 goles por parte de un equipo, el coeficiente suele ser 
#     o muy pequeño (en el orden de 0.001) o muy grande (mayor a 2).
