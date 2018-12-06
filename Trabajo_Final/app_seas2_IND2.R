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
  titlePanel("PIB IND,Revisiones y Tendencia"),
  sidebarLayout(
    sidebarPanel("Consultar PIB IND", 
                selectInput(inputId="accion",
                            label="Elija una serie:",
                            choices= c("IND_1T_17","IND_2T_17","IND_3T_17","IND_4T_17","IND_1T_18","IND_2T_18")),
                checkboxInput("logs",
                               "Transformación logarítmica",
                               value=FALSE),
                checkboxInput("x13",
                               "Componente Tendencial (X13-SEATS)",
                               value=FALSE)),
        
  mainPanel(" IVF del PIB de  la Industria y sus revisiones",
            h1('Gráfico del PIB IND'),
      plotOutput('grafico_ind'),
      h2('Tasas de crecimiento interanual'),
      tableOutput("tabla_ind")
      )
  )
)

server <- function(input, output) {
  output$grafico_ind<-renderPlot({
    ind <- read.csv("IND.csv", sep=";",dec=',')
    y_ind<-ind[,input$accion]
    y_ind<-y_ind[!is.na(y_ind)]
    y_ind<-ts(y_ind,frequency=4,start=c(1997,1))
    eje_y<-input$accion
    if(input$logs) {
      y_ind<-log(y_ind)
      eje_y<-paste('log(',eje_y,')',sep='')
    }
        plot(y_ind,type='l',ylab=eje_y,xlab='trimestres')
    if(input$x13) {
      library(seasonal)
      x13_ind<-seas(y_ind,transform.function='none')
      # tendencia
      lines(x13_ind$data[,'trend'],col='red',lwd=2)
      # seasonal
      #lines(x13$data[,'seasonaladj'],col='green',lwd=2)
      legend('topleft',c('serie','tendencia'),col=c(1,2),lwd=c(1,2),bty='n')
    }
  })
  tasa_pib_ind <- read.csv("Tasa_crecimiento_ind.csv", sep=";",dec=',')
  output$tabla_ind<-renderTable(
    tasa_pib_ind, striped=FALSE,hover=FALSE, bordered=FALSE,spacing="s", width="auto",align="c", rownames=FALSE,colnames=TRUE, digits=1
  )
}
shinyApp(ui = ui, server = server)
