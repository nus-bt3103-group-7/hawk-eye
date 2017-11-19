library(shiny)
library(shinydashboard)
library(leaflet)
library(geosphere)
library(shiny)
library(jsonlite)
library(httr)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Hawk-Eye"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Commuter View", icon = icon("dashboard"),
               menuSubItem("Crowdedness Overview",tabName = "View1",selected = TRUE),
               menuSubItem("Reimbursement System",tabName = "View2")),
      menuItem("PTO View", tabName = "PTO", icon = icon("dashboard"))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "View1", 
              textInput("destination","Enter destination:"),
              selectInput("radius", "Select radius(m)",
                          c(200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500),
                          selected = 800),
              actionButton("go","Go"),
              fluidRow(h3(textOutput("warning"))),
              fluidRow(
                column(7,leafletOutput("mymap")),
                column(5,plotlyOutput("barchart"))
              )),
      tabItem(tabName = "View2",
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
              )),
      tabItem(tabName = "PTO",
              sidebarPanel(
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
              mainPanel(
                fluidRow(
                  leafletOutput("groundMap")
                ), 
                fluidRow("Matching Commuters to Destinations", align="center"), 
                fluidRow(textOutput("label")), 
                fluidRow(plotlyOutput(outputId = "plotlyBarChart")
                ),
                fluidRow(plotlyOutput(outputId = "plotlySurfaceChart")
                )
              )
      ))
  ))