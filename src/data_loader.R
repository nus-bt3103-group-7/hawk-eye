library(jsonlite)
library(httr)

# CLICK `Session -> Set Working Directory -> To Source File Location`
# load bus stop data
bus_stops <- read.csv("../resources/allstoppingpoints.csv")
bus_stops$BFC.marker.ID = NULL
colnames(bus_stops) = c('id','name', 'road_name', 'longitue', 'latitude')
PUT("https://bt3101-07.firebaseio.com/bus_stop.json?auth=MULTPLyGcPig4Hd2aCplVibPdIm3bpHoiT1LJG3R", 
    body = toJSON(bus_stops, pretty = TRUE))

