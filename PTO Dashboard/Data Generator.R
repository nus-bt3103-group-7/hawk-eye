library(leaflet)
library(geosphere)

#################################################################################
#read data
allStoppingPoints <- read.csv("allstoppingpoints.csv")

#################################################################################
#set paramaters

affectedStation <- c(1.3513141, 103.8469711)
singaporeCentral <- c(1.3283594, 103.8129097)

clusters <- allStoppingPoints[grep("STN", allStoppingPoints$Name), ]
clusters <- clusters[!duplicated(clusters$Name),]
randomClusters <- clusters[sample(nrow(clusters), 20), ]

###############################
#get affected stops within radius

scopeRadius <- 500

allStoppingPoints$distanceFromStation <- NA
colDistance <- which(colnames(allStoppingPoints)=="distanceFromStation")

for (i in 1:nrow(allStoppingPoints)){
  long <- allStoppingPoints[i,5]
  lat <- allStoppingPoints[i,6]
  longLat <- c(allStoppingPoints[i,5], allStoppingPoints[i,6])
  affectedStationLongLat <- c(affectedStation[2], affectedStation[1])
  allStoppingPoints[i,colDistance] <-  distGeo(affectedStationLongLat, longLat)
}

proximityStops <- subset(allStoppingPoints, distanceFromStation < scopeRadius)
proximityStops <- proximityStops[order(proximityStops$distanceFromStation),]
#################################################################################
generateRandomPoint <- function(lat, long, displace){
  #displace lat and long
  
  # pythagorean theorem
  axisDisplace <- sqrt(displace/2)
  
  #displace long
  longDisplaceUnits <- (axisDisplace * 1/111111)
  
  #new lat
  latDisplaceUnits <- axisDisplace * (0.2 / cos(lat) / 111111)
  
  longLower <- long - longDisplaceUnits
  longUpper <- long + longDisplaceUnits
  
  latLower <- lat - latDisplaceUnits
  latUpper <- lat + latDisplaceUnits
  
 c(runif(1,latLower,latUpper), runif(1,longLower,longUpper))

}

#################################################################################
#initialise empty data frame

commuterData <- data.frame(commuter_id = numeric(),
                           station_lat = numeric(), station_long = numeric(),
                           # busStop_lat = numeric(), busStop_long = numeric(),
                           current_lat = numeric(), current_long = numeric(),
                           # clusterCentral_lat = numeric(), clusterCentral_long = numeric(),
                           destination_lat = numeric(), destination_long = numeric())

#################################################################################
#populate simulated data

commuter_id_col <- which(colnames(commuterData) == "commuter_id")
station_lat_col <- which(colnames(commuterData) == "station_lat")
station_long_col <- which(colnames(commuterData) == "station_long")
# busStop_lat_col <- which(colnames(commuterData) == "busStop_lat")
# busStop_long_col <- which(colnames(commuterData) == "busStop_long")
current_lat_col <- which(colnames(commuterData) == "current_lat")
current_long_col <- which(colnames(commuterData) == "current_long")
# clusterCentral_lat_col <- which(colnames(commuterData) == "clusterCentral_lat")
# clusterCentral_long_col <- which(colnames(commuterData) == "clusterCentral_long")
destination_lat_col <- which(colnames(commuterData) == "destination_lat")
destination_long_col <- which(colnames(commuterData) == "destination_long")


for (i in 1:500){
    commuterData[i, commuter_id_col] <- i
    
    commuterData[i, station_lat_col] <- affectedStation[1]
    commuterData[i, station_long_col] <- affectedStation[2]
    
    #determine the current location of the commuter
      busStopOrRandom <- runif(1,0,1)
      
      if (busStopOrRandom < 0.9) {
        originBusStop <- runif(1,0,1)
        
        if (originBusStop <= 0.4){
          assignedBusStop <- 1
        } else if (originBusStop <= 0.7){
          assignedBusStop <- 2
        } else if (originBusStop <= 0.9){
          assignedBusStop <- 3
        } else if (originBusStop <= 0.95){
          assignedBusStop <- 4
        } else{
          assignedBusStop <- 5
        }
        
        assignedBusStop_lat <- proximityStops[assignedBusStop, 6]
        assignedBusStop_long <- proximityStops[assignedBusStop, 5]
        
        current_latLong <- generateRandomPoint(assignedBusStop_lat, assignedBusStop_long, 800)
      } else {
        current_latLong <- generateRandomPoint(commuterData[i, station_lat_col], commuterData[i, station_long_col], 50000)
      }
    
      commuterData[i,current_lat_col] <- current_latLong[1]
      commuterData[i,current_long_col] <- current_latLong[2]
      
    #determine the destination of the commuter
      clusterOrRandom <- runif(1,0,1)
      
      if (clusterOrRandom < 0.9) {
        destinationClusterRandom <- runif(1,0,1)
        
        if (destinationClusterRandom <= 0.2){
          assignedCluster <- 1
        } else if (destinationClusterRandom <= 0.4){
          assignedCluster <- 2
        } else if (destinationClusterRandom <= 0.5){
          assignedCluster <- 3
        } else if (destinationClusterRandom <= 0.6){
          assignedCluster <- 4
        } else if (destinationClusterRandom <= 0.7){
          assignedCluster <- 5
        } else if (destinationClusterRandom <= 0.75){
          assignedCluster <- 6
        } else if (destinationClusterRandom <= 0.8){
          assignedCluster <- 7
        } else if (destinationClusterRandom <= 0.85){
          assignedCluster <- 8
        } else if (destinationClusterRandom <= 0.9){
          assignedCluster <- 9
        } else if (destinationClusterRandom <= 0.95){
          assignedCluster <- 10
        } else{
          assignedCluster <- 11
        }
        
        assignedCluster_lat <- randomClusters[assignedCluster, 6]
        assignedCluster_long <- randomClusters[assignedCluster, 5]
        
        destination_latLong <- generateRandomPoint(assignedCluster_lat, assignedCluster_long, 2000000)
      } else {
        destination_latLong <- generateRandomPoint(affectedStation[2], affectedStation[1], 50000)
      }
      
      commuterData[i,destination_lat_col] <- destination_latLong[1]
      commuterData[i,destination_long_col] <- destination_latLong[2]
}

commuterData <- na.omit(commuterData)
################################################################################
#leaflet aestetics 
mrtIcon <- icons(iconUrl = 'mrt_logo.png',   iconWidth = 25, iconHeight = 30)
#########
#generate leaflet to visualise the simulated data

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(lng=affectedStation[2], lat=affectedStation[1], icon = mrtIcon, popup="affected", group = "Affected Station") %>%
  addCircles(lng =commuterData$current_long, lat=commuterData$current_lat, radius = 0.5, col = "blue")  %>%
  addCircles(lng =commuterData$destination_long, lat=commuterData$destination_lat, radius = 0.5, col = "red") 

write.csv(commuterData, "commuterData.csv", row.names = F)



