library(leaflet)
library(geosphere)
library(shiny)

#new changes here hello

shinyServer(function(input, output) {
  
  #################################################################################
  #data
  
  #required data sets
  # static: allStoppingPoints (all bus stops)
  # dynamic: commuter's locations - simulated for now
  
  allStoppingPoints <- read.csv("allstoppingpoints.csv")
  commuterData <- read.csv("commuterData.csv")
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
  
  #################################################################################
  #perform clustering
  cbind(commuterData$destination_lat, commuterData$destination_long)
  
  kmeansOutput <- kmeans(cbind(commuterData$destination_lat, commuterData$destination_long), centers = 10)
  
  clusterCentres <- as.data.frame(kmeansOutput$centers)
  colnames(clusterCentres) <- c("lat", "long")
  clusterCentres$commutersDestinationInside <- NA
  
  for (clusterCentreRow in 1:nrow(clusterCentres)){
    
    clusterCentre <- c(clusterCentres[clusterCentreRow,2], clusterCentres[clusterCentreRow,1])
    
    tempDestinations <- cbind.data.frame(commuterData$destination_lat, commuterData$destination_long, NA)
    colnames(tempDestinations) <- c("lat", "long", "distanceFromClusterCentre")
    
    for (i in 1:nrow(tempDestinations)){
      longLat <- c(tempDestinations[i,2], tempDestinations[i,1])
      tempDestinations[i,3] <- distGeo(clusterCentre, longLat)
    }
    
    destinationsInCluster <- nrow(subset(tempDestinations, distanceFromClusterCentre < 2000))
    
    clusterCentres[clusterCentreRow,3] <- destinationsInCluster
  }
  
  clusterCentres <- subset(clusterCentres, commutersDestinationInside > 30)
    ################################################################################
    #leaflet aestetics 
    
    mrtIcon <- icons(iconUrl = 'mrt_logo.png',   iconWidth = 25, iconHeight = 30)
    
    #################################################################################
    #generate leaflet
    leaflet() %>%
      addLayersControl(
        overlayGroups = c("Commuters' Destination", "Suggested Clusters", "All Bus Stops"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addProviderTiles("CartoDB.Positron") %>%
      addMarkers(lng=stationLatLong[1], lat=stationLatLong[2], icon = mrtIcon, popup=scopeRadius, group = "Affected Station") %>%
      hideGroup("All Bus Stops") %>% 
      addLegend('bottomright', title = "Commuters' Data", colors =c("blue", "red"), labels =c("Current", "Destinations")) %>%
      # addCircles(lng =proximityStations$coords.x1, lat=proximityStations$coords.x2, group = "Nearby Stations") %>%
      addCircles(lng =commuterData$current_long, lat=commuterData$current_lat, group = "Commuters' Location") %>%
      addCircles(lng =commuterData$destination_long, lat=commuterData$destination_lat, col = "red", group = "Commuters' Destination") %>%
      addCircles(lng =allStoppingPoints$coords.x1, lat=allStoppingPoints$coords.x2, col = "black", group = "All Bus Stops", radius = 10) %>%
      addCircles(lng =clusterCentres$long, lat=clusterCentres$lat, col = "green", opacity = 0.2, radius = 2000, stroke=FALSE, group = "Suggested Clusters") 
  })

})
