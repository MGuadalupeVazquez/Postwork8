library(ggplot2)
library(reshape2)

#Elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
data <- read.csv("match.data.csv")
names(data)

#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,) FTHG
(casa <- prop.table(x=table(data$home.score)))
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,) FTAG
(visitante <- prop.table(x=table(data$away.score)))
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y 
#el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
(conjunta <- prop.table(x=table(data$home.score,data$away.score)))

#Realiza lo siguiente:
casa <- as.data.frame(casa)
casa <- rename(casa,Goles=Var1, Probabilidad=Freq)
visitante <- as.data.frame(visitante)
visitante <- rename(visitante,Goles=Var1, Probabilidad=Freq)

#Un gráfico de barras para las probabilidades marginales estimadas del número de 
#goles que anota el equipo de casa.
ggplot(casa, aes(x=Goles, y=Probabilidad))+
  geom_bar(stat="identity", fill="Pink")+
  ggtitle("Equipo de casa")

#Un gráfico de barras para las probabilidades marginales estimadas del número de
#goles que anota el equipo visitante.
ggplot(visitante, aes(x=Goles, y=Probabilidad))+
  geom_bar(stat="identity", fill="cadetblue2")+
  ggtitle("Equipo visitante")

#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles 
#que anotan el equipo de casa y el equipo visitante en un partido.
conjunta <- melt(conjunta)
conjunta <- rename(conjunta, GolCasa=Var1, GolVisitante=Var2, Probabilidad=value)
ggplot(conjunta, aes(GolCasa, GolVisitante)) + 
  geom_tile(aes(fill=Probabilidad))+
  labs(x = "Goles equipo de casa", 
       y = "Goles equipo visitante",
       title = "Probabilidades conjuntas")+
  scale_fill_gradient(low="lightcyan", high="coral3")+
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_fixed(ratio=1)+
  theme(axis.text.x=element_text(angle=0, hjust=0))+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  scale_y_continuous(breaks = seq(0, 8, by = 1))
