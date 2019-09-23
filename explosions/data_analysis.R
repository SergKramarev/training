library(dplyr)
library(lubridate)


# reading data
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear_explosions <- nuclear_explosions %>%
    mutate(date_long = ymd(date_long),
           latutudelongitude = paste(as.character(latitude), as.character(longitude), sep = ":"))