library(leaflet)
library(geosphere)
library(shiny)
library(jsonlite)
library(httr)
library(plotly)
source("darylFunctions.R")
source("yixinFunction.R")

#################################################################################
#data
fieldsMandatory <- c("can_number")

################ function to get data from firebase
pullFromFirebase <- function(url){
  r <- GET(url)
  con <- content(r, as="text")
  dataList <- fromJSON(con)
  as.data.frame(dataList)
}

insertCanNumber <- function(can_number) {
  can_number = toJSON(list(CAN=can_number, timestamp_inserted=Sys.time()), pretty=TRUE, auto_unbox= TRUE)
  POST("https://bt3101-07.firebaseio.com/can_number.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R",
       body=can_number)
}

###############
#pulling data from firebase db

commuterData <- pullFromFirebase("https://bt3101-07.firebaseio.com/user_data.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R")
allStoppingPoints <- pullFromFirebase("https://bt3101-07.firebaseio.com/bus_stop.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R")
colnames(allStoppingPoints)[3] <- "longtitude"

commuterData_yixin <- pullFromFirebase("https://bt3103demo-9d97e.firebaseio.com/commuter/-Kz5cyikkDnicPa3lHcp.json")
allStoppingPoints_yixin <- pullFromFirebase("https://bt3103demo-9d97e.firebaseio.com/bus_stop/-Kz5cy9G1y7PyBSocOsX.json")

#################################################################################
#static parameters

#station name
stationName <- "Bishan MRT"
#kent ridge coordinates
stationLatLong <-c(103.8469711, 1.3513141)

#################################################################################

shinyServer(function(input, output) {
  observeEvent(input$go, {
    #insertNew(input$destination)
    destination <- input$destination
    radius <- as.numeric(input$radius)
    current <- generateCurrentLocation(radius)
    if(destination =="") {
      output$warning <- renderText("Please enter your destination")
      insertNew(current,data.frame(list(lon=NA,lat=NA)))
    }
    else {
      # Not NA
      dest <- geocode(destination)
      longitude <- dest$lon
      latitude <- dest$lat
      if(is.na(longitude)==TRUE && is.na(latitude)==TRUE) {
        output$warning <- renderText("Invalid address")
        insertNew(current,dest)
        output$mymap <- renderLeaflet({generateMap(commuterData_yixin,allStoppingPoints_yixin,radius,current)
        })
        output$barchart <- renderPlotly({generateBarChart(commuterData_yixin,allStoppingPoints_yixin,radius)})
      }
      else {
        if(floor(longitude) != 103 && floor(latitude) != 1) {
          output$warning <- renderText("Address is not in Singapore!")
          insertNew(current,dest)
          output$mymap <- renderLeaflet({generateMap(commuterData_yixin,allStoppingPoints_yixin,radius,current)})
          output$barchart <- renderPlotly({generateBarChart(commuterData_yixin,allStoppingPoints_yixin,radius)})
        }
        else {
          # Valid address
          insertNew(current,dest)
          output$mymap <- renderLeaflet({mapWithDestination(commuterData_yixin,allStoppingPoints_yixin,radius,current,dest)
          })
          output$barchart <- renderPlotly({generateBarChart(commuterData_yixin,allStoppingPoints_yixin,radius)})
        }
      }
    }
  }
  )
  
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
  
  #this will render the top/first graph (leafelt map) in the PTO's dashboard
  output$groundMap <- renderLeaflet({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    clusterCentres <- read.csv("clusterCentres.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generateGroundMap(commuterData, allStoppingPoints, clusterCentres, stationLatLong, scopeRadius)
  })
  
  #this will render the middle/second graph (bar chart) in the PTO's dashboard
  output$plotlyBarChart <- renderPlotly({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    odTableWide <- read.csv("odTableWide.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generatePlotlyBarChart(odTableWide)
  })
  

  #this will render the bottom/third graph (3D surface chart) in the PTO's dashboard
  output$plotlySurfaceChart <- renderPlotly({
    scopeRadius <- input$radius
    clusterCount <- input$clusters
    minCommutersInCluster <- input$commutersInCluster
    odTableWide <- read.csv("odTableWide.csv", stringsAsFactors = F)
    #call global function from DarylFunctions.R
    generatePlotlySurfaceChart(odTableWide)
  })
  
  observeEvent(input$submit, {
    insertCanNumber(input$can_number)
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
})