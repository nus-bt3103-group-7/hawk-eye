library(leaflet)
library(geosphere)
library(shiny)
library(jsonlite)
library(httr)
library(plotly)
source("darylFunctions.R")

#################################################################################
#data

# required data sets
# static: allStoppingPoints (all bus stops)
# dynamic: commuter's locations - simulated for now

################ function to get data from firebase
pullFromFirebase <- function(url){
  r <- GET(url)
  con <- content(r, as="text")
  dataList <- fromJSON(con)
  as.data.frame(dataList)
}
###############
#pulling data from firebase db

commuterData <- pullFromFirebase("https://bt3101-07.firebaseio.com/user_data.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R")
allStoppingPoints <- pullFromFirebase("https://bt3101-07.firebaseio.com/bus_stop.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R")
colnames(allStoppingPoints)[3] <- "longtitude"

#################################################################################
#static parameters

#station name
stationName <- "Bishan MRT"
#kent ridge coordinates
stationLatLong <-c(103.8469711, 1.3513141)

#################################################################################

shinyServer(function(input, output) {
  
  output$label <- renderText({
    #default values in case shiny input fails   
    scopeRadius <- 800
    numberOfClusters <- 4
    minCommutersInCluster <- 30
    #radius to focus on
    scopeRadius <- as.numeric(input$radius)
    numberOfClusters <- as.numeric(input$clusters)
    minCommutersInCluster <- as.numeric(input$commutersInCluster)
    #call global function from DarylFunctions.R
    generateDataForGraphs(commuterData, allStoppingPoints, scopeRadius, numberOfClusters, minCommutersInCluster, stationLatLong)
  })
  
  output$groundMap <- renderLeaflet({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    clusterCentres <- read.csv("clusterCentres.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generateGroundMap(commuterData, allStoppingPoints, clusterCentres, stationLatLong, scopeRadius)
  })
  
  output$plotlyBarChart <- renderPlotly({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    odTableWide <- read.csv("odTableWide.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generatePlotlyBarChart(odTableWide)
  })
  

  output$plotlySurfaceChart <- renderPlotly({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    odTableWide <- read.csv("odTableWide.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generatePlotlySurfaceChart(odTableWide)
  })
  
  })
