library(leaflet)
library(geosphere)
library(shiny)

shinyServer(function(input, output) {
  
  #################################################################################
  #data
  #currently a dataframe is created from a .csv read - but should actually be pulled from firebase
  #commuterData.csv is simulated using another R file (see "Data Generator.R")

  commuterData <- read.csv("commuterData.csv")
  #################################################################################
  #bus stop master file
  allStoppingPoints <- read.csv("allstoppingpoints.csv")
  #################################################################################
  
  output$groundMap <- renderLeaflet({

  #################################################################################
  #input parameters
  
  #station name
  stationName <- "Bishan MRT"
  
  #kent ridge coordinates
  stationLatLong <- c(103.7827029, 1.2924922)
  stationLatLong <-c(103.8469711, 1.3513141)
  
  #radius to focus on
  scopeRadius <- as.numeric(input$radius)
  
  #################################################################################
  #get affected stops within radius
  
  allStoppingPoints$distanceFromStation <- NA
  colDistance <- which(colnames(allStoppingPoints)=="distanceFromStation")
  
  for (i in 1:nrow(allStoppingPoints)){
    long <- allStoppingPoints[i,5]
    lat <- allStoppingPoints[i,6]
    longLat <- c(allStoppingPoints[i,5], allStoppingPoints[i,6])
    allStoppingPoints[i,colDistance] <-  distGeo(stationLatLong, longLat)
  }
  
  proximityStations <- subset(allStoppingPoints, distanceFromStation < scopeRadius)
  
    ################################################################################
    #leaflet aestetics 
    
    mrtIcon <- icons(iconUrl = 'mrt_logo.png',   iconWidth = 25, iconHeight = 30)
    
    #################################################################################
    #generate leaflet
    leaflet() %>%
      addLayersControl(
        overlayGroups = c("Nearby Stations", "Commuters' Destination", "All Bus Stops"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng=stationLatLong[1], lat=stationLatLong[2], icon = mrtIcon, popup=scopeRadius, group = "Affected Station") %>%
      hideGroup("All Bus Stops") %>% 
      addCircles(lng =proximityStops$coords.x1, lat=proximityStops$coords.x2, group = "Nearby Stations") %>%
      addCircles(lng =commuterData$current_long, lat=commuterData$current_lat, group = "Commuters' Location") %>%
      addCircles(lng =commuterData$destination_long, lat=commuterData$destination_lat, col = "red", group = "Commuters' Destination") %>%
      addCircles(lng =allStoppingPoints$coords.x1, lat=allStoppingPoints$coords.x2, col = "black", group = "All Bus Stops")

  })

})
