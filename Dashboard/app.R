# Postwork Sesión 8
# Este dashboard presenta algunas características de los datos de soccer de las 
# temporadas del 2010 al 2020 de la primera división de la liga española.
# El dashboard contiene 4 pestañas diferentes:
# 1. Gráficas de barras de los goles del equipo local y visitante por ciudad.
# 2. Probabilidades marginales y conjuntas para el número de goles que anotan el equipo de casa o 
#    el equipo visitante en un partido.
# 3. Conjunto de datos match.data.csv
# 4. Gráficas de los factores de ganancia promedio y máximo.

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)

#Sección para definir la interfaz de usuario para la aplicación con 4 pestañas.
ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      #Título de la aplicación.
      dashboardHeader(title = "Proyecto"),
      
      #Barra lateral con los nombres de las pestañas.
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Gráficas de barras", tabName = "barras", icon = icon("chart-area")),
          menuItem("Probabilidades", tabName = "prob", icon = icon("file-picture-o")),
          menuItem("Conjunto de datos", tabName = "data_table", icon = icon("table")),
          menuItem("Promedio y máximo", tabName = "img", icon = icon("file-picture-o"))
        )
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          #Pestaña de las gráficas de barras.
          
          tabItem(tabName = "barras",
                  
                  fluidRow(
                    box(title = "Histograma del número de goles por equipo", 
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        selectInput("x", "Seleccione el valor de X",
                                    choices = c("home.score", "away.score")),
                        
                        #Barra lateral con una entrada deslizante para el número de bins.
                        box(
                          sliderInput("bins", "Número de observaciones:", 1, 15, 8)
                        )
                    ),
                    
                    #Muestra la gráfica de barras con la configuración deseada.
                    plotOutput("plotb", width="auto", height = 600)
                  )
          ),
          
          #Pestaña de las probabilidades marginales y conjuntas.
          
          tabItem(tabName = "prob", 
                  fluidRow(
                    box(title = "Probabilidad marginal del número de goles que anota el equipo de casa", 
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        img(src = "ProbMargCasa.png", 
                            width = "99%", height = "100%")),
                    box(title = "Probabilidad marginal del número de goles que anota el equipo visitante", 
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        img(src = "ProbMargVisitante.png", 
                            width = "99%", height = "100%")),
                    box(title = "Probabilidad conjunta de los goles que anotan el equipo de casa y visitante en un partido",
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        img(src = "ProbConjunta.png", 
                            width = "99%", height = "100%"))
                  )
          ),
          
          #Pestaña del conjunto de datos.
          
          tabItem(tabName = "data_table",
                  fluidRow( 
                    box(title = "Datos de la liga española de soccer", 
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        dataTableOutput ("data_table")
                    ))
          ), 
          
          #Pestaña de las gráficas de los factores de ganancia promedio y máximo.
          
          tabItem(tabName = "img",
                  fluidRow(
                    tabBox(title = "Gráficas de los factores de ganancia",
                           id = "location_plot_tabs",
                           # height = ctmmweb:::STYLES$height_location_box,
                           width = 12,
                           tabPanel("Promedio",
                                    img(src = "promedio.png", width = "99%", height = "100%")),
                           tabPanel("Máximo",
                                    img(src = "maximo.png", width = "99%", height = "100%")))
                    
                  )
          )
          
        )
      ),
      skin = "green"
    )
  )


#Sección donde se define la lógica del servidor requerida para la aplicación.
server <- function(input, output) {
  
  #Carga del conjunto de datos match.data.csv
  data <- read.csv("match.data.csv")
  
  #Cambiamos el formato de la columna fecha.
  data <- mutate(data, date = as.Date(date, "%Y-%m-%d"))
  
  output$plotb <- renderPlot({
    
    #Generaramos los bins basados en input$bins de la sección ui
    x <- data[,input$x]
    bin <- seq(min(x), max(x), length.out = input$bins + 1)
    
    
    #Generamos las gráficas de barras con el número especificado de bins.
    ggplot(data, aes(x, color=factor(away.team))) + 
      geom_histogram(breaks = bin) +
      labs(xlim = c(0, max(x))) + 
      theme_light() + 
      theme(legend.position="none")+
      theme(axis.title=element_text(size=10,face="bold")) +
      xlab(input$x) + ylab("Frecuencia") + 
      facet_wrap("away.team")
    
    
  })
  
  
  output$data_table <- renderDataTable({data},
                                       #Muestra las opciones en una lista de selección para la longitud de registros en 
                                       #la página.
                                       options = list(LengthMenu = c(10, 25, 50, 100),
                                                      #Número de registros iniciales para mostrar en la página.
                                                      pageLength = 10)
  )
  
}

#Comando para ejecutar la aplicación.
shinyApp(ui, server)