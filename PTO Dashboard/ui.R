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
                             c("200m" = 200,
                               "400m" = 400,
                               "600m" = 800))
    ),

    mainPanel(
                 fluidRow(
                   leafletOutput("groundMap")
                 )
    )  

  )
)

