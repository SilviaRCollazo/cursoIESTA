#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  titlePanel("PIB AGRO,Revisiones y Tendencia"),
  sidebarLayout(
    sidebarPanel("Consultar PIB AGRO", 
                selectInput(inputId="accion",
                            label="Elija una serie:",
                            choices= c("AGRO_1T_17","AGRO_2T_17","AGRO_3T_17","AGRO_4T_17","AGRO_1T_18","AGRO_2T_18")),
                checkboxInput("logs",
                               "Transformación logarítmica",
                               value=FALSE),
                checkboxInput("x13",
                               "Componente Tendencial (X13-SEATS)",
                               value=FALSE)),
        
  mainPanel(" IVF del PIB del Agro y sus revisiones",
            h1('Gráfico del PIB AGRO'),
      plotOutput('grafico_agro'),
      h2('Tasas de crecimiento interanual'),
      tableOutput("tabla_agro")
      )
  )
)

server <- function(input, output) {
  output$grafico_agro<-renderPlot({
    agro <- read.csv("AGRO.csv", sep=";",dec=',')
    y_agro<-agro[,input$accion]
    y_agro<-y_agro[!is.na(y_agro)]
    y_agro<-ts(y_agro,frequency=4,start=c(1997,1))
    eje_y<-input$accion
    if(input$logs) {
      y_agro<-log(y_agro)
      eje_y<-paste('log(',eje_y,')',sep='')
    }
        plot(y_agro,type='l',ylab=eje_y,xlab='trimestres')
    if(input$x13) {
      library(seasonal)
      x13_agro<-seas(y_agro,transform.function='none')
      # tendencia
      lines(x13_agro$data[,'trend'],col='red',lwd=2)
      # seasonal
      #lines(x13$data[,'seasonaladj'],col='green',lwd=2)
      legend('topleft',c('serie','tendencia'),col=c(1,2),lwd=c(1,2),bty='n')
    }
  })
  tasa_pib_agro <- read.csv("Tasa_crecimiento_agro.csv", sep=";",dec=',')
  output$tabla_agro<-renderTable(
    tasa_pib_agro, striped=FALSE,hover=FALSE, bordered=FALSE,spacing="s", width="auto",align="c", rownames=FALSE,colnames=TRUE, digits=1
  )
}
shinyApp(ui = ui, server = server)
