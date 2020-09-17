library(tidyverse)
library(ggplot2)
library(data.table)
library(leaflet)
library(ggmap)
library(lubridate)
library(stringi)
install.packages("stringi", dependencies = FALSE)
install.packages("yaml", dependencies = TRUE)

setwd("~/Documents/Documents/College/Spring 2018/Data Science/Vietnam War/datamil-vietnam-war-thor-data/original")

vietnam <- fread("thor_data_vietnam.csv")
vietnam$MSNDATE <-
  vietnam$MSNDATE %>%
  as.Date()
vietnam <-
  vietnam %>%
  select(-THOR_DATA_VIET_ID, -SOURCEID, -SOURCERECORD,
         -TGTORIGCOORDS, -TGTORIGCOORDSFORMAT, -ADDITIONALINFO,
         -GEOZONE, -ID, -CALLSIGN, -AIRCRAFT_ORIGINAL, -AIRFORCEGROUP, -AIRFORCESQDN,
         -UNIT, -TGTID)
colnames(vietnam)[6] <- "TGTLAT"
colnames(vietnam)[7] <- "TGTLON"

a<-year(vietnam$MSNDATE)

b<-month(vietnam$MSNDATE)

vietnam$MONTH <-
  floor_date(vietnam$MSNDATE, unit = "month")

vietnam.bob <-
  vietnam %>%
  filter(NUMWEAPONSDELIVERED > 0)

small <- vietnam.bob %>%
  sample_n(100)

sum(vietnam.bob$WEAPONSLOADEDWEIGHT)/2000
sum(vietnam.bob$WEAPONTYPEWEIGHT * vietnam.bob$NUMWEAPONSDELIVERED * vietnam.bob$NUMOFACFT)/2000

sum(vietnam$WEAPONSLOADEDWEIGHT)/2000

vietnam %>%
  ggplot(aes(x = COUNTRYFLYINGMISSION)) +
  geom_bar(stat = "count")

m <-
  vietnam.bob %>%
  count(TGTTYPE)
m$pct <- prop.table(m$n) * 100
 #bob <-
m %>%
  arrange(desc(pct)) %>%
  filter(pct > 3) %>%
  filter(TGTTYPE != "")

vietnam %>%
  ggplot(mapping = aes(x = MSNDATE,
                       y = WEAPONSLOADEDWEIGHT)) +
  geom_area() +
  scale_x_date() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Pounds of explosive per day",
       x = "Date",
       y = "Weightload")

vietnam.bob %>%
  filter(COUNTRYFLYINGMISSION != "") %>%
  ggplot(mapping = aes(x = MONTH,
                       fill = COUNTRYFLYINGMISSION)) +
  geom_area(stat = "count", position = "fill") +
  scale_x_date() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Daily bombings by country",
       x = "Date",
       y = "Instances")

vietnam.bob %>%
  filter(TGTCOUNTRY == "SOUTH VIETNAM" | TGTCOUNTRY == "NORTH VIETNAM") %>%
  filter(COUNTRYFLYINGMISSION == "UNITED STATES OF AMERICA") %>%
  ggplot(aes(x=MSNDATE, y=NUMWEAPONSDELIVERED*WEAPONTYPEWEIGHT/2000)) +
  geom_bar(stat = "identity") +
  scale_x_date() +
  facet_wrap(~TGTCOUNTRY) +
  theme_minimal() +
  labs(title = "American Bombs by Target Country",
       x = "Date",
       y = "Approximate Tonnage of Bombs Dropped")

vietnam.bob %>%
  ggplot(mapping = aes(x = MONTH)) +
  geom_area(stat = "count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Total strikes per month",
       x = "Date",
       y = "Strikes")

vietnam.bob %>%
  filter(PERIODOFDAY == "D" | PERIODOFDAY == "N") %>%
  ggplot(mapping = aes(x = MSNDATE,
                       fill = PERIODOFDAY)) +
  geom_area(stat = "count", position = "fill") +
  scale_x_date() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Bombings in day vs. at night",
       x = "Date",
       y = "Proportion") +
  scale_fill_discrete(name = "Period of day",
                      labels = c("Day", "Night"))

vietnam.bob %>%
  filter(PERIODOFDAY == "D" | PERIODOFDAY == "N") %>%
  filter(COUNTRYFLYINGMISSION != "") %>%
  ggplot(mapping = aes(x = COUNTRYFLYINGMISSION,
                       fill = PERIODOFDAY)) +
  geom_bar(stat = "count", position = "fill") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Period of day by country",
       x = "Country",
       y = "Proportion") +
  scale_fill_discrete(name = "Period of day",
                      labels = c("Day", "Night"))

vietnam.bob %>%
  filter(VALID_AIRCRAFT_ROOT == "B-52") %>%
  filter(MILSERVICE == "USAF") %>%
  ggplot(mapping = aes(x = MONTH)) +
  geom_area(stat = "count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Use of B-52 aircraft",
       x = "Date",
       y = "Instances")

vietnam.bob %>%
  mutate(B52 = ifelse(VALID_AIRCRAFT_ROOT == "B-52",
                      "B-52",
                      "Other")) %>%
  filter(TGTCOUNTRY == "NORTH VIETNAM" |
           TGTCOUNTRY == "SOUTH VIETNAM") %>%
  filter(MILSERVICE == "USAF") %>%
  ggplot(mapping = aes(x = MONTH, fill = B52)) +
  geom_area(stat = "count", position = "fill") +
  facet_wrap(~TGTCOUNTRY) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Use of B-52 aircraft by country",
       x = "Date",
       y = "Instances")

vietnam.bob %>%
  filter(TGTCOUNTRY == "SOUTH VIETNAM" | TGTCOUNTRY == "NORTH VIETNAM") %>%
  filter(COUNTRYFLYINGMISSION == "UNITED STATES OF AMERICA") %>%
  filter(MSNDATE >= "1972-12-1" & MSNDATE <= "1973-1-1") %>%
  ggplot(aes(x = MSNDATE,
             y = NUMWEAPONSDELIVERED*WEAPONTYPEWEIGHT/2000)) +
  geom_bar(stat = "identity",
           fill = "darkred") +
  facet_wrap(~TGTCOUNTRY) +
  theme_minimal() +
  labs(title = "American Bombs by Target Country",
       x = "Day in December 1972",
       y = "Tonnage of Strike")

vietnam %>%
  filter(year(MSNDATE) > 1973) %>%
  ggplot(mapping = aes(x = MSNDATE,
                       fill = COUNTRYFLYINGMISSION)) +
  geom_area(stat = "count") +
  scale_x_date() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Spectral")

vietnam %>%
  ggplot(mapping = aes(x = MSNDATE,
                       fill = ))

vietnam %>%
  count(OPERATIONSUPPORTED) %>%
  filter(n > 10000)
# 1,920,049 unassociated bombs - 1/2 of all bombs?

#vietnam %>%
#  leaflet() %>%
#  addProviderTiles(providers$Esri.WorldTopoMap) %>%
#  addCircles(lng = ~TGTLON,
#             lat = ~TGTLAT,
#             color = "#262834",
#             radius = .025,
#             stroke = FALSE)

register_google(key = "AIzaSyCzEGHzEAkVx6YwDH6rsg2hMAreuLWepPQ")
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