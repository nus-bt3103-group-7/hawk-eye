library(shiny)
library(plotly)
library(ggplot2)
library(leaflet)
library(plotly)
library(shinyjs)

shinyUI(pageWithSidebar(
  
  headerPanel(title="Service Disruption at Bishan Station"),
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     width=1,
                     sliderInput("radius", "Radius Around Affected Station to Observe (m):",
                                 min = 200,
                                 max = 2000,
                                 step= 100,
                                 value = 800),
                     selectInput("clusters", "Clusters to Identify:",
                                 c("1" = 1,
                                   "2" = 2,
                                   "3" = 3,
                                   "4" = 4,
                                   "5" = 5,
                                   "6" = 6,
                                   "7" = 7,
                                   "8" = 8),
                                 selected = 4),
                     sliderInput("commutersInCluster", "Minimum Number of Commuters in Cluster:",
                                 min = 20,
                                 max = 100,
                                 step = 10,
                                 value = 30)
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     #yikun and yixin - add sidebar panel codes here
                     textInput("destination","Enter destination:"),
                     selectInput("radius", "Select radius(m)",
                                          c(200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500),
                                          selected = 800),
                     actionButton("go","Go"),
                     shinyjs::useShinyjs(),
                     div(
                       id = "form",
                       textInput("can_number", "CAN number"),
                       actionButton("submit", "Submit", class = "btn-primary")
                     ),
                     shinyjs::hidden(
                       div(
                         id = "thankyou_msg",
                         h3("Thank you, traffic fee will be refunded upon validation.")
                       )
                     )  
                     
    ) ,
    
    conditionalPanel(condition="input.conditionedPanels==3",
                     
                     shinyjs::useShinyjs(),
                     div(
                       id = "form",
                       textInput("can_number", "CAN number"),
                       actionButton("submit", "Submit", class = "btn-primary")
                     ),
                     shinyjs::hidden(
                       div(
                         id = "thankyou_msg",
                         h3("Thank you, traffic fee will be refunded upon validation.")
                       )
                     )  
                     
    )

  ),
  mainPanel(
    tabsetPanel(
      tabPanel("PTO View", value=1,
               fluidRow(
                 leafletOutput("groundMap")
               ), 
               fluidRow("Matching Commuters to Destinations", align="center"), 
               fluidRow(textOutput("label")), 
               fluidRow(plotlyOutput(outputId = "plotlyBarChart")
               ),
               fluidRow(plotlyOutput(outputId = "plotlySurfaceChart")
               )
               ), 
      
      tabPanel("Comuter View ", 
               #yikun and yixin - add ui main panel codes here,
               fluidRow(h3(textOutput("warning"))),
               fluidRow(leafletOutput("mymap")),
               fluidRow(plotlyOutput("barchart")),
               value=2),
       id = "conditionedPanels"
    )
  )
))
