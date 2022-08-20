################ INSTALACIÓN DE PAQUETE Y DESCARGA DE ARCHIVOS #################

# Paquete para instalar librerias desde repositorios de GitHub
# install.packages("remotes")
# library(remotes)
# install_github("cran/fbRanks")
# install.packages(dplyr)
# install.packages(gplot2)
library(fbRanks)
library(dplyr)
library(ggplot2)

# No es necesario cambiar el directorio de trabajo en RStudioCloud
# setwd("Direccion de carpeta")

# Como los csv ya están en el repositorio del proyecto no es necesario descargarlos nuevamente
# Descarga de archivos
#u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
#u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
#u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
#u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
#u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
#u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
#u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
#u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
#u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
#u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#download.file(url = u1011, destfile ="SP1-1011.csv", mode = "wb")
#download.file(url = u1112, destfile ="SP1-1112.csv", mode = "wb")
#download.file(url = u1213, destfile ="SP1-1213.csv", mode = "wb")
#download.file(url = u1314, destfile ="SP1-1314.csv", mode = "wb")
#download.file(url = u1415, destfile ="SP1-1415.csv", mode = "wb")
#download.file(url = u1516, destfile ="SP1-1516.csv", mode = "wb")
#download.file(url = u1617, destfile ="SP1-1617.csv", mode = "wb")
#download.file(url = u1718, destfile ="SP1-1718.csv", mode = "wb")
#download.file(url = u1819, destfile ="SP1-1819.csv", mode = "wb")
#download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")



####################### LECTURA Y PROCESAMIENTO DE DATOS #######################

# Lectura de datos

csv_s <- list("SP1-1011.csv", "SP1-1112.csv", "SP1-1213.csv", "SP1-1314.csv",
             "SP1-1415.csv", "SP1-1516.csv", "SP1-1617.csv", "SP1-1718.csv",
             "SP1-1819.csv", "SP1-1920.csv")
lista <- lapply(csv_s, read.csv)


# Procesamiento de datos

lapply(lista, colnames) 
# Notamos que cada csv tiene diferente cantidad de columnas con diferentes nombres.
# Queremos seleccionar las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG, 
# BbMx.2.5, BbAv.2.5, BbMx.2.5.1 y BbAv.2.5.1.

# Observamos que los primeros nueve dataframes tienen las columnas Date, HomeTeam, 
# AwayTeam, FTHG, FTAG juntas, pero el último dataframe tiene una columna extra 
# llamada Time entre la columna Date y HomeTeam
# Además que las columnas BbMx.2.5, BbAv.2.5, BbMx.2.5.1 y BbAv.2.5.1 tienen otros
# nombres, a saber, Max.2.5, Max.2.5.1, Avg.2.5, Avg.2.5.1.

# Para facilitar la manipulación simultánea de los DataFrames, modificaremos los
# detalles mencionados en el último dataframe

# Eliminamos columna extra
lista[[10]]<-select(lista[[10]], -Time)
# Renombramos columnas
lista[[10]]<-rename(lista[[10]], BbMx.2.5=Max.2.5, BbAv.2.5=Avg.2.5,
                    BbMx.2.5.1=Max.2.5.1, BbAv.2.5.1=Avg.2.5.1)
# Conservamos solo las columnas requeridas
lista <- lapply(lista, select, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
lapply(lista, colnames)

# Observamos que se seleccionaron las columnas deseadas de todos los dataframes
# pero en el último no están en el mismo orden de los primeros 9, por lo que
# ordenamos sus columnas
lista[[10]] <- select(lista[[10]], colnames(lista[[9]]))

# Convertimos las fechas a tipo de dato Date
lista <- lapply(lista,mutate, Date = as.Date(Date, format = "%d/%m/%y"))

# Unimos los dataframes
d1020s <- do.call(rbind,lista)

# Renombramos las columnas
d1020s <- rename(d1020s, date = Date, home.team = HomeTeam, home.score = FTHG, 
                 away.team = AwayTeam, away.score = FTAG, Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, Max.2.5.U = BbMx.2.5.1, Avg.2.5.U = BbAv.2.5.1)
names(d1020s)

# Ordenamos columnas
data <- select(d1020s, date, home.team, home.score, away.team, away.score:Avg.2.5.U)

# Visualizamos algunos registros del dataframe
head(data, n = 2L); tail(data, n = 2L)

# Data frames de partidos y equipos
md <- select(data, date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv") 
# df es una lista que contiene 4 data frames, entre ellos scores tiene toda la 
# información de match.data.csv y teams tiene los nombres de todos los equipos

#Extraemos dichos dataframes por separado
teams <- df$teams; scores <- df$scores
head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)


# Conjuntos iniciales de entrenamiento y de prueba
# Extraemos mes y año de las fechas de todos los partidos
Ym <- format(scores$date, "%Y-%m") 
# Obtenemos las fechas (mes y añoo) sin repetición en que se jugaron partidos
Ym <- unique(Ym)
# Obtenemos todos los partidos que se jugaron en el mes 15 (diciembre de 2011)
places <- which(Ym[15]==format(scores$date, "%Y-%m")) 
# Fecha final conjunto de entrenamiento / último día en que se jugó un partido en el mes 15.
ffe <- scores$date[max(places)]

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. 
# Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos 
# hasta el ffe 
train <- scores %>% filter(date <= ffe) # Partidos jugados hasta del mes 15 
test <- scores %>% filter(date > ffe)   # Partidos jugados después del mes 15

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)



######################## AJUSTE DE MODELO Y PREDICCIONES #######################

# Primer ajuste del modelo
# Vectores de fechas únicas en que se jugaron partidos
traindate <- unique(train$date) 
testdate <- unique(test$date)

# Ajustar usando rank.teams (de fbRanks)
ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])
# Nota: el modelo se ajusta utilizando la función glm (función para ajustar
# lineales generalizados)


# Primera predicción
pred <- predict(ranks, date = testdate[1])
# Se imprimen los partidos de la primera fecha de prueba, mostrando:
# - Equipo de casa vs Equipo visitante
# - Probabilidad de que gane el equipo de casa HW (Home Win)
# - Probabilidad de que gane el equipo vistante AW (Away Win)
# - Probabilidad de empate T (Tie)
# - Predicción del marcador en 'pred score' y el verdadero marcador 'actual'

# Almacenar información en vectores separados
phs <- pred$scores$pred.home.score  # 'pred score' de los equipos de casa
pas <- pred$scores$pred.away.score  # 'pred score' de los equipos visitantes
pht <- pred$scores$home.team        # Equipos de casa
pat <- pred$scores$away.team        # Equipos visitantes
phs; pas; pht; pat


# Ajustar y predecir ajustando y prediciendo
phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL

for(i in 1:(length(unique(scores$date))-170)){
  # Se ajusta el modelo a partidos jugados en 170 fechas consecutivas
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  # Se predicen los partidos de la fecha inmediata posterior
  pred <- predict(ranks, date = unique(scores$date)[i+170], silent = TRUE) 
  # Nota: El parametro silent evita que se impriman los resultados en cada iteración
  
  # Agregar al final del vector los resultados de la predicción.
  phs <- c(phs, pred$scores$pred.home.score)
  pas <- c(pas, pred$scores$pred.away.score)
  pht <- c(pht, pred$scores$home.team)
  pat <- c(pat, pred$scores$away.team)
}

# Eliminamos NA's
# Indices de elementos donde no hubo NA en las predicciones de goles del equipo
# de casa ni en las predicciones de goles del equipo viisitante.
# Nota: Negación de la disyunción es la conjunción de las negaciones.
buenos <- !(is.na(phs) | is.na(pas))
# Conservar elementos buenos
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions



######################## ANÁLISIS DE RESULTADOS (MOMIOS) #######################

# Se selecciona toda la información de los partidos que por la fecha de juego
# pertenecen al conjunto de prueba
momio <- data %>% filter(date >= unique(scores$date)[171])

# Seleccionamos los partidos que tuvieron una 'buena' predicción (sin valores nulos)
momio <- momio[buenos,]

# Proporción de aciertos en el pronóstico de equipos de casa y equipos visitantes
mean(pht == momio$home.team); mean(pat == momio$away.team)

# Proporción de aciertos en el pronóstico de los partidos que tuvieron más de
# 2.5 goles totales
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)

# Proporción de aciertos en el pronóstico de los partidos que tuvieron menos de 
# 2.5 goles totales
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)

# Guardar datos de goles anotados en vectores por aparte
hs <- momio$home.score
as <- momio$away.score


# Apuesta Over (Under) X: Apuestas a favor de que la cantidad total de goles 
# anotados en un partido sea mayor (menor) que X. Por ejemplo, apostar a Over 1.5
# es apostar a que el juego tendrá más de 1,5 goles en total."

# Probabilidades condicionales P(A|B)=P(A^B)/P(B)

# Probabilidad condicional estimada de ganar en over 2.5
# Proporciónn de partidos con más de tres goles según el modelo (P(B))
mean(phs + pas > 3)

# Probabilidad de que el verdadero marcador sume más de 2.5 goles dado que el 
# modelo pronosticó que el marcador sumaría más de 3 goles
p.over <- mean(hs + as > 2.5 & phs + pas > 3)/mean(phs + pas > 3) 
p.over <- round(pc.over.2.5, 2)


# Probabilidad condicional estimada de ganar en under 2.5
# Proporción de partidos con menos de 2.1 goles según el modelo (P(B))
mean(phs + pas < 2.1)

# Probabilidad de que el verdadero marcador sume menos de 2.5 goles dado que 
# se pronosticó que el marcador sumaría menos de 2.1 goles
p.under <- mean(hs + as < 2.5 & phs + pas < 2.1)/mean(phs + pas < 2.1) 
p.under <- round(pc.under.2.5,2)



############################ SIMULACION DE APUESTAS ############################
# Juegos con momios máximos

# Inicia la apuesta con 50000
# g guardará las fluctuaciones del capital
cap <- 50000; g <- NULL

# Iteramos en cada partido con una apuesta de 1000
for(j in 1:length(phs)){
  # Si el modelo predice más de 3 goles y la tasa de ganancia bruta esperada* es
  # mayor a 1, se elige apostar por Over 2.5
  if((phs[j] + pas[j] > 3) & (p.over * momio$Max.2.5.O[j] > 1)){
    # Si el pronóstico fue acertado, aumentamos el capital con la ganancia neta
    if(hs[j] + as[j] > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    # Si el pronóstico fallá, perdemos lo apostado del capital
    else cap <- cap - 1000
  }

  # Si el modelo predice menos de 2.1 goles y la tasa de ganancia bruta esperada*
  # es mayor a 1, se elige apostar por Under 2.5
  if((phs[j] + pas[j] < 2.1) & (p.under * momio$Max.2.5.U[j] > 1)){
    # Si el pronóstico fue acertado, aumentamos el capital con la ganancia neta
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    # Si el pronóstico fallá, perdemos lo apostado del capital
    else cap <- cap - 1000
  }

  g <- c(g, cap) # Se captura el capital después de cada apuesta
}
# *Nota: TGB esperada = Prob(Ganar) x Momio + Prob(Perder) x 0
#                     = Prob(Ganar) x Momio

# Escenario con momios máximos
# Convertimos el vector g a dataframe para usar ggplot
g <- data.frame(Num_Ap = 1:length(g), Capital = g)
# Gráfica
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + 
  geom_line(color="pink", size=1.5) + 
  geom_point(color="red",size=0.3) +
  geom_hline(yintercept=50000, linetype = "dashed") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Escenario con momios máximos") +
  theme(plot.title = element_text(size=12)) +
  theme(axis.text.x = element_text(color="red" , size = 10, angle = 0, hjust = 1),
        axis.text.y = element_text(color="red" , size = 10, angle = 0, hjust = 1))
# color, ángulo y estilo de las abscisas y ordenadas 



# Escenario con momios promedio 
# El razonamiento es análogo al de momios máximos pero varía la ponderación de
# las ganancias con el momio over o under promedio, según corresponda

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if((phs[j] + pas[j] > 3) & (p.over * momio$Avg.2.5.O[j] > 1)){
    if(hs[j] + as[j] > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
  }
  
  if((phs[j] + pas[j] < 2.1) & (p.under * momio$Avg.2.5.U[j] > 1)){
    if(hs[j] + as[j] < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
  }

  g <- c(g, cap)
}

# Escenario con momios promedio
g <- data.frame(Num_Ap = 1:length(g), Capital = g)
# Gráfica
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + 
  geom_line(color="cyan", size=1.5) + 
  geom_point(color="blue",size=0.3) +
  geom_hline(yintercept=50000, linetype = "dashed") +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Escenario con momios promedio") +
  theme(plot.title = element_text(size=12)) +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 0, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 0, hjust = 1))
