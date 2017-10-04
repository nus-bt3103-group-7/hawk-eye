library(leaflet)
library(geosphere)
library(shiny)

#################################################################################
#data

#required data sets
# static: allStoppingPoints (all bus stops)
# dynamic: commuter's locations - simulated for now

allStoppingPoints <- read.csv("allstoppingpoints.csv")
commuterData <- read.csv("commuterData.csv")

#################################################################################

shinyServer(function(input, output) {
  
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
  
  output$plotlyBarChart <- renderPlotly({
    
    ################################################################################
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
    
    #radius for bus stops
    allStoppingPoints$distanceFromStation <- NA
    colDistance <- which(colnames(allStoppingPoints)=="distanceFromStation")
    for (i in 1:nrow(allStoppingPoints)){
      longLat <- c(allStoppingPoints[i,5], allStoppingPoints[i,6])
      allStoppingPoints[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    proximityStops <- subset(allStoppingPoints, distanceFromStation < scopeRadius)
    
    #radius for commuters
    commuterData$distanceFromStation <- NA
    colDistance <- which(colnames(commuterData)=="distanceFromStation")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,5], commuterData[i,4])
      commuterData[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    commuterData <- subset(commuterData, distanceFromStation < scopeRadius)
    
    #################################################################################
    #assign commuter to nearest bus stop 
    commuterData$closestBusStop <- NA
    closestBusStopCol <- which(colnames(commuterData)=="closestBusStop")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,5], commuterData[i,4])
      
      tempProximityBusStops <- proximityStops
      tempProximityBusStops$distanceToCommuter <- NA
      distanceToCommuterCol <- which(colnames(tempProximityBusStops) == "distanceToCommuter")
      
      for (tempProximityBusStopsRow in 1:nrow(tempProximityBusStops)){
        stopLongLat <- c(tempProximityBusStops[tempProximityBusStopsRow,5], tempProximityBusStops[tempProximityBusStopsRow,6])
        tempProximityBusStops[tempProximityBusStopsRow,distanceToCommuterCol] <-  distGeo(stopLongLat, longLat)
      }
      
      
      commuterData[i,closestBusStopCol] <- as.character(tempProximityBusStops[order(tempProximityBusStops$distanceToCommuter),][1,2])
    }
    #################################################################################
    #kmeans
    kmeansOutput <- kmeans(cbind(commuterData$destination_lat, commuterData$destination_long), centers = 10)
    
    ##########################
    #get number of commuters within clusters and get the df of commuters within a cluster
    
    clusterCentres <- as.data.frame(kmeansOutput$centers)
    colnames(clusterCentres) <- c("lat", "long")
    
    clusterCentres$id <- 1:nrow(clusterCentres)
    clusterCentres$commutersDestinationInside <- NA
    
    commutersInProximityOfClusterCentre <- data.frame()
    
    for (clusterCentreRow in 1:nrow(clusterCentres)){
      clusterCentre <- c(clusterCentres[clusterCentreRow,2], clusterCentres[clusterCentreRow,1])
      clusterId <- clusterCentres[clusterCentreRow,3]
      tempDestinations <- cbind.data.frame(commuterData$destination_lat, commuterData$destination_long, NA, commuterData$closestBusStop)
      colnames(tempDestinations) <- c("lat", "long", "distanceFromClusterCentre", "closestBusStop")
      
      for (i in 1:nrow(tempDestinations)){
        longLat <- c(tempDestinations[i,2], tempDestinations[i,1])
        tempDestinations[i,3] <- distGeo(clusterCentre, longLat)
      }
      #rbind to compile a dataframe of commuters within a cluster
      commutersWithinSpecificCluster <- subset(tempDestinations, distanceFromClusterCentre < 2000)
      if(nrow(commutersWithinSpecificCluster) > 0){
        commutersWithinSpecificCluster$cluster_id <- clusterId
      }
      commutersInProximityOfClusterCentre <- rbind(commutersInProximityOfClusterCentre,commutersWithinSpecificCluster)
      
      destinationsInCluster <- nrow(commutersWithinSpecificCluster)
      clusterCentres[clusterCentreRow,4] <- destinationsInCluster
    }
    
    ###############
    
    #keep on clusters with more than 30 commuters
    clusterCentres <- subset(clusterCentres, commutersDestinationInside > 30)
    
    #keep only commuters in proximity of a populated cluster
    commutersInProximityOfClusterCentre <- subset(commutersInProximityOfClusterCentre, cluster_id %in% unique(clusterCentres$id))
    
    ##########################
    #get closest bus stop for cluster aka cluster name
    
    clusterCentres$closestStation <- NA
    closestStationCol <- which(colnames(clusterCentres) == "closestStation")
    
    for (clusterCentreRow in 1:nrow(clusterCentres)){
      clusterCentre <- c(clusterCentres[clusterCentreRow,2], clusterCentres[clusterCentreRow,1])
      tempAllStoppingPoints <- allStoppingPoints[grep("STN", allStoppingPoints$Name), ]
      tempAllStoppingPoints <- cbind.data.frame(tempAllStoppingPoints$Name, tempAllStoppingPoints$coords.x2, tempAllStoppingPoints$coords.x1, NA)
      colnames(tempAllStoppingPoints) <- c("name", "lat", "long", "distanceFromClusterCentre")
      for (i in 1:nrow(tempAllStoppingPoints)){
        longLat <- c(tempAllStoppingPoints[i,3], tempAllStoppingPoints[i,2])
        tempAllStoppingPoints[i,4] <- distGeo(clusterCentre, longLat)
      }
      clusterCentres[clusterCentreRow, closestStationCol] <- as.character(tempAllStoppingPoints[order(tempAllStoppingPoints$distanceFromClusterCentre),][1,1])
    }
    
    #get station name of commutersInProximityOfClusterCentre by refering to cluster_id
    commutersInProximityOfClusterCentre <- merge(commutersInProximityOfClusterCentre, clusterCentres[ , c("id", "closestStation")],
                                                 by.x=c("cluster_id"), by.y=c("id"))
    
    #################################################################################
    #get breakdown of each destination by origin
    odTable <- as.data.frame(table(commutersInProximityOfClusterCentre$closestStation, commutersInProximityOfClusterCentre$closestBusStop))
    #################################################################################
    #prepare data for polty plot
    
    odTableWide <- reshape(odTable, idvar = "Var1", timevar = "Var2", direction = "wide")
    names(odTableWide) <- gsub("Freq.", "", names(odTableWide), fixed = TRUE)
    odTableWide$sum <- rowSums(odTableWide[, -1])
    odTableWide$Var1 <- factor(odTableWide$Var1, levels = unique(odTableWide$Var1)[order(odTableWide$sum, decreasing = TRUE)])
    
    #################################################################################
    #generate plotly
    
    plotlyBarChart <- plot_ly(odTableWide, x = ~as.factor(Var1), y = odTableWide[,2], name = colnames(odTableWide)[2], type = 'bar') %>%
      layout(yaxis = list(title = 'Current Locations'), xaxis = list(title = 'Current Destinations'), barmode = 'stack', title = "")
    
    for (i in 3:(ncol(odTableWide)-1)){
      plotlyBarChart <- add_trace(plotlyBarChart, y = odTableWide[,i], name = colnames(odTableWide)[i])
    }
    
    plotlyBarChart
  })

})
