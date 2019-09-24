library(dplyr)
library(lubridate)
library(plotly)
library(leaflet)

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
g <- leaflet(nuclear_explosions) %>% 
    addTiles() %>% 
    addMarkers(lng = ~longitude, lat = ~latitude, 
                      clusterOptions = markerClusterOptions(), icon = icon_nuc)




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
icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'white',
    library = 'ion'
)

icon_nuc <- leaflet::icons(
    iconUrl = "explosions/Dangerous-Radiation.png",
    iconWidth =  40, iconHeight = 40,
    iconAnchorX = 20, iconAnchorY = 20)

# Нужно сдалть красивые попапы, позможно немного изменить значок, хотя может 
# и не нужно, сделать название, легенду и т.д.
# сделать разные значки для разных типов испытаний, возможно как-то по времени их разделить