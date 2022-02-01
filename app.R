library(shiny)
library(shinyjs)
library(rgdal)
library(spdplyr)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(ggplot2)
library(readxl)

sapo <- read_excel("sapo.xlsx")

n_casos <- sample(1:100, 5476, replace = TRUE)

df <- data.frame(n_casos, stringsAsFactors = TRUE)

shape <- rgdal::readOGR('municipios_AMC_2000_2010.shp')

df$clu_final <- shape@data$clu_final

shape <- left_join(shape, df)

tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 20px;
  }
"))

title <- tags$div(
    tag.map.title, HTML("Porcentagem de infectados por cidade")
)  

cores <- leaflet::colorQuantile("OrRd", shape@data$n_casos, n = 9)

ui <- fluidPage(
    titlePanel("Situacao geral da doenca do Sapo-triste"),
    sidebarLayout(
        sidebarPanel(
            textInput("texto1","Digite seu nome:"),
            actionButton("botao", "Clique para iniciar o exame"),
            textOutput("texto1"),
            hidden(
                div("Seu resultado:", verbatimTextOutput("texto2"))),
            plotOutput("grafico")
            
        ),
        mainPanel( leafletOutput("mymap", width = "700px", height = "700px")
                   
        )
    ))

server <- function(input, output, session){
    
    observeEvent(input$botao,{
        output$texto1 <- renderText(isolate(paste(input$texto1)))
        toggle("Seu resultado:")
        output$texto2 <- renderText(paste("Voce esta ", sample(1:100,1)," por cento sapo, e ",sample(1:100,1),"por cento triste"))
    })
    
    output$mymap <- renderLeaflet({
        
        leaflet(shape) %>%
            addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~cores(n_casos)) %>%
            addTiles() %>%
            addControl(title, position = "topleft", className="map-title") %>%
            addLegend(position = "bottomright", pal = cores, values = ~shape@data$n_casos,
                      title = "Pct de casos", labFormat = labelFormat(prefix = ""),
                      opacity = 1)
    })
    
    output$grafico <- renderPlot({
        
        p <- ggplot(sapo, aes(dia, casos)) +
            geom_line(color = "red", size = 2) +
            ggtitle("Projecao de casos para os proximos dias") +
            theme_minimal()
        p
        
    })
    
    
}

shinyApp(ui, server)

