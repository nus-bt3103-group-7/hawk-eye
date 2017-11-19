library(shiny)
library(jsonlite)
library(httr)
library(leaflet)
library(geosphere)
library(plotly)
library(ggmap)
library(ggplot2)

#station name
stationName <- "Bishan MRT"
#kent ridge coordinates
stationLatLong <-c(103.8469711, 1.3513141)
stationLat <- 1.3513141
stationLong <- 103.8469711



#### Function to generate Leaflet Map ####
generateMap <- function(commuterData, allStoppingPoints, scopeRadius,currentLocation) {
  ### filter bus stop data ###
  distanceFromCentre = by(allStoppingPoints, 1:nrow(allStoppingPoints), function(row) { distHaversine(c(row$longitude, row$latitude), stationLatLong)})
  allStoppingPoints$distance <- distanceFromCentre
  newStop <- allStoppingPoints[which(allStoppingPoints$distance<=scopeRadius),]
  current <- generateCurrentLocation(commuterData,scopeRadius)
  
  
  ### filter commuter data ###
  commuterFromCentre = by(commuterData, 1:nrow(commuterData), function(row) { distHaversine(c(row$current_long, row$current_lat), stationLatLong)})
  commuterData$distance <- commuterFromCentre
  filterCommuter <- commuterData[which(commuterData$distance<=scopeRadius),]
  
  ### plot the map ###
  mrtIcon <- icons(iconUrl = '../resources/mrt_logo.png',   iconWidth = 25, iconHeight = 30)
  stationIcon <- icons(iconUrl = '../resources/bus_station_icon.png', iconWidth = 20,iconHeight = 20)
  m <- leaflet()
  m <- addLayersControl(m,
                        overlayGroups = c("Commuters' Location","All Bus Stops"),
                        options = layersControlOptions(collapsed = FALSE)
  )
  m <- addProviderTiles(m,"CartoDB.Positron")
  m <- addMarkers(m,lng=stationLatLong[1], lat=stationLatLong[2], icon = mrtIcon, popup=scopeRadius, group = "Affected Station")
  #hideGroup("All Bus Stops") %>% 
  #addLegend('bottomright', title = "Commuters' Data", colors =c("blue", "red"), labels =c("Current", "Destinations")) %>%
  m <- addMarkers(m,lng =newStop$longitude, lat=newStop$latitude, icon = stationIcon, group = "All Bus Stops")
  m <- addCircleMarkers(m,lng =filterCommuter$current_long, lat=filterCommuter$current_lat, group = "Commuters' Location",
                        clusterOptions = markerClusterOptions(),radius = 5,color="red") 
  m <- addMarkers(m,lng = current[2], lat = current[1],label = "You are here") 
  
  
}

mapWithDestination <- function(commuterData,allStoppingPoints,scopeRadius,currentLocation,destination) {
  ### filter bus stop data ###
  distanceFromCentre = by(allStoppingPoints, 1:nrow(allStoppingPoints), function(row) { distHaversine(c(row$longitude, row$latitude), stationLatLong)})
  allStoppingPoints$distance <- distanceFromCentre
  newStop <- allStoppingPoints[which(allStoppingPoints$distance<=scopeRadius),]
  
  ### filter commuter data ###
  commuterFromCentre = by(commuterData, 1:nrow(commuterData), function(row) { distHaversine(c(row$current_long, row$current_lat), stationLatLong)})
  commuterData$distance <- commuterFromCentre
  filterCommuter <- commuterData[which(commuterData$distance<=scopeRadius),]
  
  ### plot the map ###
  mrtIcon <- icons(iconUrl = '../resources/mrt_logo.png',   iconWidth = 25, iconHeight = 30)
  stationIcon <- icons(iconUrl = '../resources/bus_station_icon.png', iconWidth = 20,iconHeight = 20)
  m <- leaflet()
  m <- addLayersControl(m,
                        overlayGroups = c("Commuters' Location","All Bus Stops"),
                        options = layersControlOptions(collapsed = FALSE)
  )
  m <- addProviderTiles(m,"CartoDB.Positron")
  m <- addMarkers(m,lng=stationLatLong[1], lat=stationLatLong[2], icon = mrtIcon, popup=scopeRadius, group = "Affected Station")
  #hideGroup("All Bus Stops") %>% 
  #addLegend('bottomright', title = "Commuters' Data", colors =c("blue", "red"), labels =c("Current", "Destinations")) %>%
  m <- addMarkers(m,lng =newStop$longitude, lat=newStop$latitude, icon = stationIcon, group = "All Bus Stops")
  m <- addCircleMarkers(m,lng =filterCommuter$current_long, lat=filterCommuter$current_lat, group = "Commuters' Location",
                        clusterOptions = markerClusterOptions(),radius =5, color = "red") 
  m <- addMarkers(m,lng = currentLocation[2], lat = currentLocation[1],label = "You are here") 
  m <- addMarkers(m,lng = destination$lon,lat = destination$lat,label = "Your destination")
  
}

### Function to plot bar chart ###
generateBarChart <- function (commuterData,allStoppingPoints,scopeRadius) {
  
  current_lat_col <- which(colnames(commuterData) == "current_lat")
  current_long_col <- which(colnames(commuterData) == "current_long")
  destination_long_col <- which(colnames(commuterData) == "destination_long")
  destination_lat_col <- which(colnames(commuterData) == "destination_lat")
  station_long_col <- which(colnames(commuterData) == "station_long")
  station_lat_col <- which(colnames(commuterData) == "station_lat")
  stop_lat_col <- which(colnames(allStoppingPoints) == "latitude")
  stop_long_col <- which(colnames(allStoppingPoints) == "longitude")
  stop_name_col <- which(colnames(allStoppingPoints) == "name")
  
  ### filter bus stop data ###
  distanceFromCentre = by(allStoppingPoints, 1:nrow(allStoppingPoints), function(row) { distHaversine(c(row$longitude, row$latitude), stationLatLong)})
  allStoppingPoints$distance <- distanceFromCentre
  filterStop <- allStoppingPoints[which(allStoppingPoints$distance<=scopeRadius),]
  
  ### filter commuter data ###
  commuterFromCentre = by(commuterData, 1:nrow(commuterData), function(row) { distHaversine(c(row$current_long, row$current_lat), stationLatLong)})
  commuterData$distance <- commuterFromCentre
  filterCommuter <- commuterData[which(commuterData$distance<=scopeRadius),]
  
  ### Assign commuter to nearby bus stop ### 
  filterStop$crowdedness <- 0
  crowdednessCol <- which(colnames(filterStop) == "crowdedness")
  
  for (i in 1:nrow(filterCommuter)){
    commuterLongLat <- c(filterCommuter[i,current_long_col],filterCommuter[i,current_lat_col])
    distanceToStop <- c()
    for (j in 1:nrow(filterStop)) {
      stopLongLat <- c(filterStop[j,stop_long_col],filterStop[j,stop_lat_col])
      tempDistance <- distGeo(stopLongLat,commuterLongLat)
      distanceToStop <- c(distanceToStop,tempDistance)
    }
    index <- which.min(distanceToStop)
    filterStop[index,crowdednessCol] = filterStop[index,crowdednessCol] + 1
  }
  
  finalStop <- filterStop[which(filterStop$crowdedness >0),]
  ### Rearrange in descresing order by crowdedness
  finalStop <- finalStop[order(finalStop$crowdedness, decreasing = TRUE),]
  if(length(finalStop[,1]) > 3) {
    finalStop <- finalStop[1:3,]
  }
  p <-ggplot(finalStop, aes(name, crowdedness))
  p +geom_bar(stat = "identity",fill = "#FF6666",width = 0.5) +ggtitle("Top 3 crowdedness bus stop")
}

### Function to generate user current location within 1000m square from affected MRT station
generateCurrentLocation <- function(commuterData,radius) {
  a <- sqrt(radius/2)
  longDisplaceUnits <- (a * 1/111111)
  latDisplaceUnits <- a * (0.2 / cos(stationLat) / 111111)
  
  longLower <- stationLong - longDisplaceUnits
  longUpper <- stationLong + longDisplaceUnits
  latLower <- stationLat - latDisplaceUnits
  latUpper <- stationLat + latDisplaceUnits
  c(runif(1,latLower,latUpper), runif(1,longLower,longUpper))
  
}

#### Function to generate new full data and insert into db ####
insertNew <- function(commuterData,curLocation,destination) {
  new <- toJSON(list(commuter_id=length(commuterData[,1])+1,
                     current_lat = curLocation[1],
                     current_long = curLocation[2],
                     destination_lat = destination$lat,
                     destination_long = destination$lon,
                     station_lat = stationLat,
                     station_long = stationLong),pretty=TRUE, auto_unbox= TRUE)
  POST("https://bt3103demo-9d97e.firebaseio.com/insert.json",body = new)
  
}