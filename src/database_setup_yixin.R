library(jsonlite)
library(httr)

commuter <- read.csv("commuterData.csv")
bus_stop <- read.csv("allstoppingpoints.csv")
bus_stop$BFC.marker.ID = NULL
colnames(bus_stop) = c('id','name', 'road_name', 'longitude', 'latitude')
POST("https://bt3103demo-9d97e.firebaseio.com/bus_stop.json", 
    body = toJSON(bus_stop, pretty = TRUE))
POST("https://bt3103demo-9d97e.firebaseio.com/commuter.json", 
    body = toJSON(commuter, pretty = TRUE))


pullFromFirebase <- function(url){
  link <- GET(url)
  content <- content(link, as="text")
  list <- fromJSON(content)
  as.data.frame(list)
}

data <- pullFromFirebase("https://bt3103demo-9d97e.firebaseio.com/bus_stop/-Kz5cy9G1y7PyBSocOsX.json")
commuter <- pullFromFirebase("https://bt3103demo-9d97e.firebaseio.com/commuter/-Kz5cyikkDnicPa3lHcp.json")
