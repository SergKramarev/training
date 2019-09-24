library(dplyr)
library(lubridate)
library(plotly)

# reading data
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear_explosions <- nuclear_explosions %>%
    mutate(date_long = ymd(date_long),
           latutudelongitude = paste(as.character(latitude), as.character(longitude), sep = ":"))



# plotly visualization
p <- nuclear_explosions %>% plot_mapbox(lat = ~latitude, lon = ~longitude, size = ~magnitude_body, mode = 'scattermapbox', frame = ~country)



#  Gvis visualization
p <- gvisGeoChart(nuclear_explosions, locationvar = "latutudelongitude")

# Leaflet visualization
g <- leaflet(nuclear_explosions) %>% addTiles() %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(), icon = icons)
getColor <- function(nuclear_explosions) {
    sapply(quakes$country, function(country) {
    if(country == "USA") {
    "green"
    } else if(country == "USSR") {
    "orange"
    } else {
    "red"
    } })
    }
