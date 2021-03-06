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
  titlePanel("PIB,Revisiones y Tendencia"),
  sidebarLayout(
    sidebarPanel("Consultar PIB", 
                selectInput(inputId="accion",
                            label="Elija una serie:",
                            choices= c("IVF_PIB_1T_17","IVF_PIB_2T_17","IVF_PIB_3T_17","IVF_PIB_4T_17","IVF_PIB_1T_18","IVF_PIB_2T_18")),
                checkboxInput("logs",
                               "Transformación logarítmica",
                               value=FALSE),
                checkboxInput("x13",
                               "Componente Tendencial (X13-SEATS)",
                               value=FALSE)),
        
  mainPanel(" IVF del PIB de Uruguay y sus revisiones",
            h1('Gráfico del PIB'),
      plotOutput('grafico'),
      h2('Tasas de crecimiento interanual'),
      tableOutput("tabla")
      )
  )
)

server <- function(input, output) {
  output$grafico<-renderPlot({
    pib_uy <- read.csv("pib_uy.csv", sep=";",dec=',')
    y<-pib_uy[,input$accion]
    y<-y[!is.na(y)]
    y<-ts(y,frequency=4,start=c(1997,1))
    eje_y<-input$accion
    if(input$logs) {
      y<-log(y)
      eje_y<-paste('log(',eje_y,')',sep='')
    }
        plot(y,type='l',ylab=eje_y,xlab='trimestres')
    if(input$x13) {
      library(seasonal)
      x13<-seas(y,transform.function='none')
      # tendencia
      lines(x13$data[,'trend'],col='red',lwd=2)
      # seasonal
      #lines(x13$data[,'seasonaladj'],col='green',lwd=2)
      legend('topleft',c('serie','tendencia'),col=c(1,2),lwd=c(1,2),bty='n')
    }
  })
  tasa_pib <- read.csv("Tasa_crecimiento.csv", sep=";",dec=',')
  output$tabla<-renderTable(
    tasa_pib, striped=FALSE,hover=FALSE, bordered=FALSE,spacing="s", width="auto",align="c", rownames=FALSE,colnames=TRUE, digits=1
  )
}
shinyApp(ui = ui, server = server)
