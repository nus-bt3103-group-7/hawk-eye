library(leaflet)
library(geosphere)
library(shiny)
library(jsonlite)
library(httr)
library(plotly)

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
#get column ids
current_lat_col <- which(colnames(commuterData) == "current_lat")
current_long_col <- which(colnames(commuterData) == "current_long")
destination_long_col <- which(colnames(commuterData) == "destination_long")
destination_lat_col <- which(colnames(commuterData) == "destination_lat")
station_long_col <- which(colnames(commuterData) == "station_long")
station_lat_col <- which(colnames(commuterData) == "station_lat")
stop_lat_col <- which(colnames(allStoppingPoints) == "latitude")
stop_long_col <- which(colnames(allStoppingPoints) == "longtitude")

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
    longLat <- c(allStoppingPoints[i,stop_long_col], allStoppingPoints[i,stop_lat_col])
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
      # addCircles(lng =proximityStationslongtitude, lat=proximityStationslatitude, group = "Nearby Stations") %>%
      addCircles(lng =commuterData$current_long, lat=commuterData$current_lat, group = "Commuters' Location") %>%
      addCircles(lng =commuterData$destination_long, lat=commuterData$destination_lat, col = "red", group = "Commuters' Destination") %>%
      addCircles(lng =allStoppingPoints$longtitude, lat=allStoppingPoints$latitude, col = "black", group = "All Bus Stops", radius = 10) %>%
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
      longLat <- c(allStoppingPoints[i,stop_long_col], allStoppingPoints[i,stop_lat_col])
      allStoppingPoints[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    proximityStops <- subset(allStoppingPoints, distanceFromStation < scopeRadius)
    
    #radius for commuters
    commuterData$distanceFromStation <- NA
    colDistance <- which(colnames(commuterData)=="distanceFromStation")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,current_long_col], commuterData[i,current_lat_col])
      commuterData[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    commuterData <- subset(commuterData, distanceFromStation < scopeRadius)
    
    #################################################################################
    #assign commuter to nearest bus stop 
    commuterData$closestBusStop <- NA
    closestBusStopCol <- which(colnames(commuterData)=="closestBusStop")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,current_long_col], commuterData[i,current_lat_col])
      
      tempProximityBusStops <- proximityStops
      tempProximityBusStops$distanceToCommuter <- NA
      distanceToCommuterCol <- which(colnames(tempProximityBusStops) == "distanceToCommuter")
      
      for (tempProximityBusStopsRow in 1:nrow(tempProximityBusStops)){
        stopLongLat <- c(tempProximityBusStops[tempProximityBusStopsRow,stop_long_col], tempProximityBusStops[tempProximityBusStopsRow,stop_lat_col])
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
      tempAllStoppingPoints <- allStoppingPoints[grep("STN", allStoppingPoints$name), ]
      tempAllStoppingPoints <- cbind.data.frame(tempAllStoppingPoints$name, tempAllStoppingPoints$latitude, tempAllStoppingPoints$longtitude, NA)
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
  
  output$plotlySurfaceChart <- renderPlotly({
    
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
      longLat <- c(allStoppingPoints[i,stop_long_col], allStoppingPoints[i,stop_lat_col])
      allStoppingPoints[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    proximityStops <- subset(allStoppingPoints, distanceFromStation < scopeRadius)
    
    #radius for commuters
    commuterData$distanceFromStation <- NA
    colDistance <- which(colnames(commuterData)=="distanceFromStation")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,current_long_col], commuterData[i,current_lat_col])
      commuterData[i,colDistance] <-  distGeo(stationLatLong, longLat)
    }
    commuterData <- subset(commuterData, distanceFromStation < scopeRadius)
    
    #################################################################################
    #assign commuter to nearest bus stop 
    commuterData$closestBusStop <- NA
    closestBusStopCol <- which(colnames(commuterData)=="closestBusStop")
    for (i in 1:nrow(commuterData)){
      longLat <- c(commuterData[i,current_long_col], commuterData[i,current_lat_col])
      
      tempProximityBusStops <- proximityStops
      tempProximityBusStops$distanceToCommuter <- NA
      distanceToCommuterCol <- which(colnames(tempProximityBusStops) == "distanceToCommuter")
      
      for (tempProximityBusStopsRow in 1:nrow(tempProximityBusStops)){
        stopLongLat <- c(tempProximityBusStops[tempProximityBusStopsRow,stop_long_col], tempProximityBusStops[tempProximityBusStopsRow,stop_lat_col])
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
      tempAllStoppingPoints <- allStoppingPoints[grep("STN", allStoppingPoints$name), ]
      tempAllStoppingPoints <- cbind.data.frame(tempAllStoppingPoints$name, tempAllStoppingPoints$latitude, tempAllStoppingPoints$longtitude, NA)
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
    #surface plot
    matrix3d <- as.matrix(odTableWide[,-c(1,ncol(odTableWide))])
    
    xAxisLabels <- colnames(matrix3d)
    yAxisLables <- odTableWide$Var1
    
    x <- list(title = "",
              ticktext=xAxisLabels,
              tickvals = c(0:(ncol(odTableWide)-2)),
              tickfont = list(size=10))
    y <- list(title = "",
              ticktext=yAxisLables,
              tickvals = c(0:(ncol(odTableWide)-2)),
              tickfont = list(size=10))
    
    z <- list(title = "Commuter Count")
    
    plotlySurfacePlot <- plot_ly(z = matrix3d, showscale = FALSE) %>% 
      add_surface() %>% 
      layout(scene = list(xaxis = x, yaxis = y, zaxis = z))
    
    plotlySurfacePlot
  })

})
