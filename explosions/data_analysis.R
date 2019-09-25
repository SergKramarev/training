library(dplyr)
library(lubridate)
library(plotly)
library(leaflet)

# reading data
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# Fakel
nuclear_explosions[nuclear_explosions$id_no == 72015, c("latitude", "longitude")] <- c(49.33, 35.31)
# Klivazh
nuclear_explosions[nuclear_explosions$id_no == 79059, c("latitude", "longitude")] <- c(48.13, 38.16)
# Mururoa changed to mean latitude and longitude of explosions in the same region
nuclear_explosions[nuclear_explosions$region == "MURUROA" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "MURUROA" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                               colMeans(nuclear_explosions[nuclear_explosions$region == "MURUROA" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
# NTS site
nuclear_explosions[nuclear_explosions$region == "NTS" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "NTS" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                           colMeans(nuclear_explosions[nuclear_explosions$region == "NTS" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
# SEMI KAZAKH site
nuclear_explosions[nuclear_explosions$region == "SEMI KAZAKH" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "SEMI KAZAKH" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                                   colMeans(nuclear_explosions[nuclear_explosions$region == "SEMI KAZAKH" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
# JAKUTS RUSS site
nuclear_explosions[nuclear_explosions$region == "JAKUTS RUSS" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "JAKUTS RUSS" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                                   colMeans(nuclear_explosions[nuclear_explosions$region == "JAKUTS RUSS" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
# BASHKIR RUSS site
nuclear_explosions[nuclear_explosions$region == "BASHKIR RUSS" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "BASHKIR RUSS" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                                    colMeans(nuclear_explosions[nuclear_explosions$region == "BASHKIR RUSS" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
nuclear_explosions[nuclear_explosions$region == "BASHKI RUSS" & nuclear_explosions$latitude == 0, c("latitude", "longitude")] <- c(colMeans(nuclear_explosions[nuclear_explosions$region == "BASHKIR RUSS" & nuclear_explosions$latitude !=  0, "latitude"], na.rm = TRUE),
                                                                                                                                   colMeans(nuclear_explosions[nuclear_explosions$region == "BASHKIR RUSS" & nuclear_explosions$latitude !=  0, "longitude"], na.rm = TRUE))
# TYUMEN RUSS
nuclear_explosions[nuclear_explosions$id_no == 85034, c("latitude", "longitude")] <- c(60.45, 72.47)

# PAMUK UZBEK
nuclear_explosions[nuclear_explosions$id_no == 68022, c("latitude", "longitude")] <- c(39.01, 65.05)





nuclear_explosions <- nuclear_explosions %>%
    mutate(date_long = ymd(date_long),
           purpose_grouped = ifelse(grepl("WR|ME|COMBAT", nuclear_explosions$purpose), "Weapons",
                            ifelse(grepl("PNE", nuclear_explosions$purpose), "Peaceful",
                                   ifelse(grepl("TRANSP|FMS|WE|SE|SAM", nuclear_explosions$purpose), "Research", "Undefined"))),
           depth = ifelse(depth > 0, depth*1000, depth),
           world_part = ifelse(latitude > 0 & longitude > 0,  "NE",
                               ifelse(latitude > 0 & longitude < 0, "NW",
                                      ifelse(latitude < 0 & longitude > 0, "SE",
                                             ifelse(latitude < 0 & longitude < 0, "SW", "strange")))),
           yield_avg = round((yield_lower + yield_upper)/2, digits = 1), 
           popup_nuc = paste("Country deployed: ", country, "<br/>",
                         "Date of explosion: ", as.character(date_long), "<br/>",
                         "Purpose of explosion: ", purpose_grouped, "<br/>",
                         "TNT equivalent: ", yield_avg, " kilotons", sep = ""))

nuclearIcons <- iconList(
    Weapons = makeIcon(iconUrl = "explosions/nuclear-bomb.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 0, iconAnchorY = 30),
    Research = makeIcon(iconUrl = "explosions/light.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 15),
    Peaceful = makeIcon(iconUrl = "explosions/nuclear-energy.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 30),
    Undefined = makeIcon(iconUrl = "explosions/question-mark-button.png", iconWidth = 30, iconHeight = 30, iconAnchorX = 15, iconAnchorY = 15)
)

# Leaflet visualization
g <- leaflet(nuclear_explosions) %>% 
    addTiles() %>% 
    addMarkers(lng = ~longitude, lat = ~latitude, 
               clusterOptions = markerClusterOptions(), icon = ~nuclearIcons[purpose_grouped],
               popup = ~popup_nuc, label = ~country)




# Добавить возможность отключить/включить по странам
# Добавить дополнительных фишек
# разобраться с широтами и долготоми по взрывам произошедшим в областях с неизвестными координатами

# Data Analysis

country_year_purpose <- nuclear_explosions %>%
    dplyr::group_by(year, country, purpose_grouped) %>%
    summarise(number_of_explosions = n(), total_mass_exploded = sum(yield_avg, na.rm = TRUE))


country_year <- nuclear_explosions %>%
    dplyr::group_by(year, country) %>%
    summarise(number_of_explosions = n(), total_mass_exploded = sum(yield_avg, na.rm = TRUE))


g <- ggplot(nuclear_explosions, aes(year))
g + geom_histogram(aes(fill = country), col = "black", bins = 54) 

h <- ggplot(nuclear_explosions, aes(year))
h + geom_histogram(aes(fill = purpose_grouped), col = "black", bins = 54)

nuc_table <- table(nuclear_explosions$country, nuclear_explosions$purpose_grouped)

hemi_explosions <- table(nuclear_explosions$world_part)

f <- ggplot(country_year, aes(year, total_mass_exploded))

f + geom_line(aes(col = country), size = 1) + geom_point(aes(col = country), col= "black")


p <- plot_ly(
    type = 'scatter',
    x = country_year$year,
    y = country_year$total_mass_exploded,
    mode = 'lines',
    transforms = list(
        list(
            type = 'groupby',
            groups = country_year$country,
            styles = list(
                list(target = "USA", value = list(line =list(color = 'blue'))),
                list(target = "USSR", value = list(line =list(color = 'red'))),
                list(target = "FRANCE", value = list(line =list(color = 'green'))),
                list(target = "CHINA", value = list(line =list(color = 'magenta'))),
                list(target = "INDIA", value = list(line =list(color = 'black'))),
                list(target = "PAKIST", value = list(line =list(color = 'yellow'))),
                list(target = "UK", value = list(line =list(color = 'purple')))
            )
        )
    )
)















# plotly visualization
p <- nuclear_explosions %>% 
    plot_mapbox(lat = ~latitude, lon = ~longitude, size = ~magnitude_body, 
                mode = 'scattermapbox', frame = ~country)



#  Gvis visualization
p <- gvisGeoChart(nuclear_explosions, locationvar = "latutudelongitude")






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


