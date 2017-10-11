library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(plotly)


shinyUI(
  pageWithSidebar(
    
    
    headerPanel(title="Service Disruption at Bishan Station"),

    sidebarPanel(width=2,
                 selectInput("radius", "Radius:",
                             c("400m" = 400,
                               "800m" = 800,
                               "1000m" = 1000,
                               "1500m" = 1500,
                               "2000m" = 2000),
                             selected = 800)
    ),

    mainPanel(
      
                 fluidRow(
                   leafletOutput("groundMap")
                 ), 
                 fluidRow("Matching Commuters to Destinations", align="center"), 
                 fluidRow(plotlyOutput(outputId = "plotlyBarChart")
                   ),
                 fluidRow(plotlyOutput(outputId = "plotlySurfaceChart")
                 )
                 
    )  

  )
)

