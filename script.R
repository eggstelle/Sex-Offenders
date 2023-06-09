library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(leaflet)
library(sp)
library(tigris)
library(sf)

rm(list=ls())


#getting census key

api_key <- "93b425b7f7548fbb33f5f8a5171adbd2b78668eb"
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")




#uploading dataframes 
#location of sex offenders was originally txt file turned csv 

schools <- read.csv("C:/Users/sophi/Documents/Sex-Offenders/Public_Schools.csv")
location <- read.csv("C:/Users/sophi/Documents/Sex-Offenders/publicsor/Locations.csv")

#filter for Chatham

schools <- filter(schools,county == "Chatham")
location <- filter(location, CountyName == "CHATHAM")

#remove the un-locatable people

location <- filter(location, Zip != 0)
location <- location[!grepl("HOMELESS|NC DEPT|UNKNOWN", location$AddressLine1),]

#make a full address for offenders

address = location %>% 
  unite(FullAddress, c(AddressLine1, City, State, Zip), sep = ", ", remove = FALSE)

#add state to school to create a full address for schools

schools$state <- "NC"

schoolloc = schools %>% 
  unite(FullAddress, c(phys_addr, phys_city, state, phys_zip), sep = ", ", remove = FALSE)

#find latitude and longitudes of each full address

offender_lat_longs <- address %>%
  geocode(FullAddress, method = 'arcgis', lat = latitude , long = longitude)

schools_lat_longs <- schoolloc %>%
  geocode(FullAddress, method = 'arcgis', lat = latitude , long = longitude)

#create chatham county

chatham <- tracts(
  state = 'NC',
  county = 'Chatham',
  cb = TRUE
)


#transorm the coordinates of chatham county

chatham <- chatham %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

#plot to check

plot(chatham$geometry)


#get only the latitude and longitude of both data sets

offender_lat_longs <- offender_lat_longs %>% select(latitude, longitude)
schools_lat_longs <- schools_lat_longs %>% select(latitude, longitude)


#convert the coordinates of both data sets using standard to make them SF

offenders_sf <- offender_lat_longs %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

schools_sf <- schools_lat_longs %>%
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)


#create buffer (this is measured in meters 304.8 meters = 1000 feet)

schools_feet<- st_buffer(schools_sf,dist = 304.8)


#create and check map

map <- ggplot(chatham$geometry) +
  geom_sf() +
  geom_sf(data = schools_feet, color=alpha("red", 0.5)) +
  geom_sf(data = schools_sf, color="red", size=1) +
  geom_sf(data = offenders_sf, color = "blue", size = 1)

map

#check for sex offenders
union = st_join(schools_feet, offenders_sf, join=st_intersects)
st_filter(schools_feet, offenders_sf, .predicate = st_intersects)
#no sex offenders near schools


#checking with leaflet
leaflet() %>%
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  addProviderTiles(
    "Stamen.Toner",
    group = "Stamen.Toner"
  ) %>%
  addProviderTiles(
    "Stamen.Terrain",
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "Esri.WorldStreetMap",
    group = "Esri.WorldStreetMap"
  ) %>%
  addProviderTiles(
    "Wikimedia",
    group = "Wikimedia"
  ) %>%
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "OpenStreetMap", "Stamen.Toner",
      "Stamen.Terrain", "Esri.WorldStreetMap",
      "Wikimedia", "CartoDB.Positron", "Esri.WorldImagery"
    )) %>%
  addPolygons(
    data = chatham,
    weight = 1
    ) %>%
  addPolygons(
    data = schools_feet,
    color = "red"
  ) %>%
  addMarkers(
    data = schools_lat_longs,
  ) %>%
  addCircleMarkers (
    data = offender_lat_longs,
    color = "blue", 
    stroke = FALSE, 
    radius = 3
  )



