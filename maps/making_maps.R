library(tidyverse)
library(ggplot2)
library(data.table)
library(leaflet)
library(ggmap)
library(lubridate)
library(stringi)


vietnam <- fread("thor_data_vietnam.csv")
vietnam$MSNDATE <-
  vietnam$MSNDATE %>%
  as.Date()
vietnam$MONTH <-
  floor_date(vietnam$MSNDATE, unit = "month")
vietnam$MONTH <-
  floor_date(vietnam$MSNDATE, unit = "month")

vietnam <-
  vietnam %>%
  select(-THOR_DATA_VIET_ID, -SOURCEID, -SOURCERECORD,
         -TGTORIGCOORDS, -TGTORIGCOORDSFORMAT, -ADDITIONALINFO,
         -GEOZONE, -ID, -CALLSIGN, -AIRCRAFT_ORIGINAL, -AIRFORCEGROUP, -AIRFORCESQDN,
         -UNIT, -TGTID)
colnames(vietnam)[6] <- "TGTLAT"
colnames(vietnam)[7] <- "TGTLON"


vietnam.bob <-
  vietnam %>%
  filter(NUMWEAPONSDELIVERED > 0)

# Obtain an API key to use in register_google(key = ""). This will give access to Google Maps.
register_google(key = "")
map1 <-
  get_map(location = "Vietnam",
          source = "google",
          maptype = "terrain",
          zoom = 5,
          crop = FALSE)
airforce1 <-
  vietnam.bob %>%
  filter(COUNTRYFLYINGMISSION == "UNITED STATES OF AMERICA") %>%
  filter(year(MSNDATE) == 1973)
ggmap(map1) +
  geom_point(aes(x = TGTLON, y = TGTLAT),
             data = airforce1,
             alpha = .1,
             color = "darkred",
             size = .05) +
  coord_fixed(xlim = c(100, 111),
              ylim = c(7.5, 23.5)) +
  labs(title = "U.S. Bombing Missions in 1973",
       x = "Longitude",
       y = "Latitude")
