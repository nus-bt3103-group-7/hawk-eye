library(jsonlite)
library(httr)

a <- list(commuter_id=59,
                     current_lat = 1,
                     current_long = 2,
                     destination_lat = 3,
                     destination_long =4,
                     station_lat = 5,
                     station_long = 6)

data <- toJSON(a,pretty=TRUE, auto_unbox= TRUE)
POST("https://bt3103demo-9d97e.firebaseio.com/commuter/-Kz5cyikkDnicPa3lHcp.json",body = data)


