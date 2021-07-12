#POSTWORK 8

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# UI de la aplicación               
ui <- fluidPage(

    dashboardPage(
        
        dashboardHeader(title = "Dashboard de la liga española de fútbol"),
        
        dashboardSidebar(
            
            sidebarMenu(
                menuItem("Gráfico de barras", tabName = "barras", icon = icon("bar-chart")),
                menuItem("Probabilidad de anotar", tabName = "probabilidades", icon = icon("calculator")),
                menuItem("Tabla de datos", tabName = "tabla", icon = icon("table")),
                menuItem("Factores", tabName = "apuestas", icon = icon("money"))
            )
            
        ),
        
        dashboardBody(
            
            tabItems(
                
                # Gráfico de barras
                tabItem(tabName = "barras",
                        fluidRow(
                            titlePanel("Goles anotados"), 
                            selectInput("x", "Seleccione el valor de X",
                                        choices = c("Goles Local"="home.score",
                                                    "Goles Visitante" = "away.score")),
                            box(plotOutput("plot1", width = 800)),
                            
                        )
                ),
                
            
                # Gráficas con probabilidades
                tabItem(tabName = "probabilidades", 
                        fluidRow(
                            titlePanel(h3("Probabilidades marginales y conjuntas de anotar gol")),
                            img( src = "postwork3.png")
                        )
                ),
                
                # Data table
                tabItem(tabName = "tabla",
                        fluidRow(        
                            titlePanel(h3("Tabla de datos")),
                            dataTableOutput ("tabla")
                        )
                ), 
                
                # Gráficas de actores de ganancia mínimo y máximo
                tabItem(tabName = "apuestas",
                        fluidRow(
                            titlePanel(h3("Factores de Ganancia Máximo")),
                            img( src = "maximo.png"),
                            titlePanel(h3("Factores de Ganancia Promedio")),
                            img(src = "promedio.png"),
                        )
                )
                
            )
        )
    )
)

# Definición de la lógica del servidor
server <- function(input, output) {
    
    #Gráfico de barras
    output$plot1 <- renderPlot({
        x <- match_data[,input$x]
        
        ggplot(match_data, aes(x, fill = as.factor(away.team))) + 
            geom_bar(  ) +
            labs( xlim = c(0, max(x))) + 
            theme_light() + 
            xlab(input$x) + ylab("Frecuencia") + 
            facet_wrap("away.team", scales = "free")+
            theme(legend.position="none")
        
    })
    
    
    #Data Table
    output$tabla<- renderDataTable( {match_data}, 
                                          options = list(aLengthMenu = c(10,20,50),
                                                         iDisplayLength = 10)
    )
    
}

# Carga de datos
match_data<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv")

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)
