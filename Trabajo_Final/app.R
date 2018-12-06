#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pib_uy <- read.csv("pib_uy.csv", sep=";",dec=',')
ui <- fluidPage(
  titlePanel("PIB_UY"),
  sidebarLayout(
    sidebarPanel("Consultar PIB", 
                selectInput(inputId="accion",
                            label="Elegir:",
                            choices= c("PIB_1T_17","PIB_2T_17","PIB_3T_17","PIB_4T_17","PIB_1T_18","PIB_2T_18")),
                numericInput(inputId="obs",
                             label="número de observaciones a graficar:",
                             value=80)),
        
  mainPanel("Gráfico del PIB_UY",
            h1('Gráfico del PIB'),
            plotOutput('grafico'))
  )
)

server <- function(input, output) {
  output$grafico<-renderPlot({
    n=nrow(pib_uy)
    # aca hago cosas
    plot(pib_uy[(n-input$obs):n,input$accion],type='l',ylab='IVF',xlab='trimestres')})
}
shinyApp(ui = ui, server = server)
