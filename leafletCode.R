#for each map let's do a facetwrap / position dodge graph to show big picture layout by borough!
 # split maps into 2 separate tabs
#intro page, current map is starting map
#add info box topline
#improve aesthetics of ui box and graphs
#optimize speed (reactive?)
#clean up code to upload to github
# in map, add more text info to both markers and popups. i.e. rank or just total bike users.
# my name in sidebarpanel has its own CSS.
# lots of bragraphs already, switch to line graph fo rtime and date?
# add data table of the leaflet, dataframe we are using! so people can visualise!


# comments show count of riders by gender per day?
# top 5 based on infobox
#hange marker to citibike or fontawesome bike?
# check box for show all or show top 25!


# do top graph for gender and age range!
#finally add data table?

#Maps Code
statsionsgrouped= jul17dfClean %>% group_by(sex, ageRange, stations, lng, lat) %>% summarise( count =n())

neighborhoodTop10_43

library(leaflet)
library(tigris)
library(dplyr)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(geojson)
library(geojsonio)

#create neighborhoods
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)


#graph points
stationsdf = jul17dfClean %>% group_by(stations, lng, lat) %>% summarise( count =n())
leaflet(stationsdf) %>% addTiles() %>% addMarkers(~lng, ~lat) %>%
  setView(-73.98, 40.75, zoom = 13)


#graph neighborhoods
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)

#integrate points to neighborhoods
stationsdf = jul17dfClean %>% group_by(stations, lng, lat) %>% summarise( count =n())
points <- as.data.frame(cbind(stationsdf$lng, stationsdf$lat))
colnames(points)=c("lng","lat")
points_spdf=points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
#new df with long, lat, by neighborhood!
points = cbind(points, matches)
#join original stations df to new neighborhood sp df by long and lat.
stationbyreg= inner_join(jul17dfClean, points, by=c("lng", "lat"))

#code to make my dataframe into sp data(polygon data)

age_neighborhood = stationbyreg %>% group_by(neighborhood, ageRange) %>% summarise(count=n())
sex_neighborhood = stationbyreg %>% group_by(neighborhood, sex) %>% summarise(count=n())
agesex_neighborhood = stationbyreg %>% group_by(neighborhood, ageRange, sex) %>% summarise(count=n())
age_neighborhood_25to30 = stationbyreg %>% group_by(neighborhood, ageRange, lng, lat) %>% filter(ageRange=="25 to 30") %>% summarise(count=n())
age_neighborhood_25to30prac = stationbyreg %>% group_by(neighborhood, ageRange, lng, lat) %>% filter(ageRange=="25 to 30") %>% summarise(count=n()) %>% arrange(desc(count)) %>% filter( count > 2000)

################
map_data_age = geo_join(nyc_neighborhoods, age_neighborhood_25to30, "neighborhood","neighborhood")

#graph points over neighborhoods
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons() %>%
  addMarkers(~lng, ~lat, data = stationsdf, clusterOptions=markerClusterOptions()) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)



# chlorpleth/opacity code
max(agesex_neighborhood$count)
min(agesex_neighborhood$count)


min(age_neighborhood$count)

max(sex_neighborhood$count)
min(sex_neighborhood$count)

bins=c(0, 2500, 5000, 7500, 10000,Inf)
pal = colorBin("Blues", domain = age_neighborhood_25to30$count, bins=bins)

################ 
# practice code that worked for 25 to 30 age group
bins=c(0, 2500, 5000, 7500, 10000,Inf)
pal = colorBin("Blues", domain = age_neighborhood_25to30$count, bins=bins)

age_neighborhood_25to30 = stationbyreg %>% group_by(neighborhood, ageRange, lng, lat) %>% filter(ageRange=="25 to 30") %>% summarise(count=n())
age_neighborhood_25to30prac = stationbyreg %>% group_by(neighborhood, ageRange, lng, lat) %>% filter(ageRange=="25 to 30") %>% summarise(count=n()) %>% arrange(desc(count)) %>% filter( count > 2000)

map_data_age = geo_join(nyc_neighborhoods, age_neighborhood_25to30, "neighborhood","neighborhood")

summary(map_data_age$count)

leaflet(map_data_age) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13) %>%
  addMarkers( ~lng, ~lat, data = age_neighborhood_25to30prac) %>%
  addPolygons(    fillColor = ~pal(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)
    )




##1st male practice
bins_male=seq(min(neighborhood_male$count), max(neighborhood_male$count), length.out = 9)
pal_male = colorBin("Blues", domain = neighborhood_male$count, bins=bins_male)


neighborhood_male = stationbyreg %>% group_by(neighborhood, sex) %>% filter(sex=="Male") %>% summarise(count=n())
neighborhood_maleprac = stationbyreg %>% group_by(neighborhood, sex, lng, lat) %>% filter(sex=="Male") %>% summarise(count=n()) %>% arrange(desc(count)) %>% filter(count >=5000)
summary(neighborhood_male$count)
max(neighborhood_male$count)



map_data_Male = geo_join(nyc_neighborhoods, neighborhood_male, "neighborhood","neighborhood")



leaflet(map_data_Male) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
 addMarkers( ~lng, ~lat, data = neighborhood_maleprac) %>%
  addPolygons(    fillColor = ~pal_male(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

#Sex leaflets
neighborhood_Both = stationbyreg %>% group_by(neighborhood) %>% summarise(count=n())
neighborhood_Male = stationbyreg %>% group_by(neighborhood, sex) %>% filter(sex=="Male") %>% summarise(count=n())
neighborhood_Female = stationbyreg %>% group_by(neighborhood, sex) %>% filter(sex=="Female") %>% summarise(count=n())

neighborhoodTop10_Both = stationbyreg %>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_Male = stationbyreg %>% filter(sex=="Male")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_Female = stationbyreg %>% filter(sex=="Female")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% ungroup() %>% top_n(10, count)

bins_Both=seq(min(neighborhood_Both$count), max(neighborhood_Both$count), length.out = 9)
bins_Male=seq(min(neighborhood_Male$count), max(neighborhood_Male$count), length.out = 9)
bins_Female=seq(min(neighborhood_Female$count), max(neighborhood_Female$count), length.out = 9)

pal_Both = colorBin("Purples", domain = neighborhood_Both$count, bins=bins_Both)
pal_Male = colorBin("Blues", domain = neighborhood_Male$count, bins=bins_male)
pal_Female = colorBin("Reds", domain = neighborhood_Female$count, bins=bins_Female)

map_data_Both = geo_join(nyc_neighborhoods, neighborhood_Both, "neighborhood","neighborhood")
map_data_Male = geo_join(nyc_neighborhoods, neighborhood_Male, "neighborhood","neighborhood")
map_data_Female = geo_join(nyc_neighborhoods, neighborhood_Female, "neighborhood","neighborhood")

leaflet_Both= leaflet(map_data_Both) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_Both) %>%
  addPolygons(    fillColor = ~pal_Both(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_Male= leaflet(map_data_Male) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_Male) %>%
  addPolygons(    fillColor = ~pal_Male(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )


leaflet_Female = leaflet_Female= leaflet(map_data_Female) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_Female) %>%
  addPolygons(    fillColor = ~pal_Female(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

#age leaflets
neighborhood_All = stationbyreg %>% group_by(neighborhood) %>% summarise(count=n())
neighborhood_13 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="13 to 18") %>% summarise(count=n())
neighborhood_19 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="19 to 24") %>% summarise(count=n())
neighborhood_25 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="25 to 30") %>% summarise(count=n())
neighborhood_31 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="31 to 36") %>% summarise(count=n())
neighborhood_37 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="37 to 42") %>% summarise(count=n())
neighborhood_43 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="43 to 48") %>% summarise(count=n())
neighborhood_49 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="49 to 54") %>% summarise(count=n())
neighborhood_55 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="55 to 60") %>% summarise(count=n())
neighborhood_61 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="61 to 66") %>% summarise(count=n())
neighborhood_67 = stationbyreg %>% group_by(neighborhood, ageRange) %>% filter(ageRange=="67 and over") %>% summarise(count=n())


neighborhoodTop10_All = stationbyreg %>% group_by(stations, lng, lat) %>% summarise(count=n()) %>%arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_13 = stationbyreg %>% filter(ageRange=="13 to 18")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_19 = stationbyreg %>% filter(ageRange=="19 to 24")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_25 = stationbyreg %>% filter(ageRange=="25 to 30")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_31 = stationbyreg %>% filter(ageRange=="31 to 36")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_37 = stationbyreg %>% filter(ageRange=="37 to 42")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_43 = stationbyreg %>% filter(ageRange=="43 to 48")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_49 = stationbyreg %>% filter(ageRange=="49 to 54")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_55 = stationbyreg %>% filter(ageRange=="55 to 60")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_61 = stationbyreg %>% filter(ageRange=="61 to 66")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)
neighborhoodTop10_67 = stationbyreg %>% filter(ageRange=="67 and over")%>% group_by(stations, lng, lat) %>% summarise(count=n()) %>% arrange(desc(count)) %>% ungroup() %>% top_n(10, count)

View(neighborhoodTop10_13)

bins_All=seq(min(neighborhood_All$count), max(neighborhood_All$count), length.out = 9)
bins_13=seq(min(neighborhood_13$count), max(neighborhood_13$count), length.out = 9)
bins_19=seq(min(neighborhood_19$count), max(neighborhood_19$count), length.out = 9)
bins_25=seq(min(neighborhood_25$count), max(neighborhood_25$count), length.out = 9)
bins_31=seq(min(neighborhood_31$count), max(neighborhood_31$count), length.out = 9)
bins_37=seq(min(neighborhood_37$count), max(neighborhood_37$count), length.out = 9)
bins_43=seq(min(neighborhood_43$count), max(neighborhood_43$count), length.out = 9)
bins_49=seq(min(neighborhood_49$count), max(neighborhood_49$count), length.out = 9)
bins_55=seq(min(neighborhood_55$count), max(neighborhood_55$count), length.out = 9)
bins_61=seq(min(neighborhood_61$count), max(neighborhood_61$count), length.out = 9)
bins_67=seq(min(neighborhood_67$count), max(neighborhood_67$count), length.out = 9)


pal_All = colorBin("Greens", domain = neighborhood_All$count, bins=bins_All)
pal_13 = colorBin("Oranges", domain = neighborhood_13$count, bins=bins_13)
pal_19 = colorBin("Purples", domain = neighborhood_19$count, bins=bins_19)
pal_25 = colorBin("Reds", domain = neighborhood_25$count, bins=bins_25)
pal_31 = colorBin("Blues", domain = neighborhood_31$count, bins=bins_31)
pal_37 = colorBin("Greens", domain = neighborhood_37$count, bins=bins_37)
pal_43 = colorBin("Oranges", domain = neighborhood_43$count, bins=bins_43)
pal_49 = colorBin("Purples", domain = neighborhood_49$count, bins=bins_49)
pal_55 = colorBin("Reds", domain = neighborhood_55$count, bins=bins_55)
pal_61 = colorBin("Blues", domain = neighborhood_61$count, bins=bins_61)
pal_67 = colorBin("Greens", domain = neighborhood_67$count, bins=bins_67)

map_data_All = geo_join(nyc_neighborhoods, neighborhood_All, "neighborhood","neighborhood")
map_data_13 = geo_join(nyc_neighborhoods, neighborhood_13, "neighborhood","neighborhood")
map_data_19 = geo_join(nyc_neighborhoods, neighborhood_19, "neighborhood","neighborhood")
map_data_25 = geo_join(nyc_neighborhoods, neighborhood_25, "neighborhood","neighborhood")
map_data_31 = geo_join(nyc_neighborhoods, neighborhood_31, "neighborhood","neighborhood")
map_data_37 = geo_join(nyc_neighborhoods, neighborhood_37, "neighborhood","neighborhood")
map_data_43 = geo_join(nyc_neighborhoods, neighborhood_43, "neighborhood","neighborhood")
map_data_49 = geo_join(nyc_neighborhoods, neighborhood_49, "neighborhood","neighborhood")
map_data_55 = geo_join(nyc_neighborhoods, neighborhood_55, "neighborhood","neighborhood")
map_data_61 = geo_join(nyc_neighborhoods, neighborhood_61, "neighborhood","neighborhood")
map_data_67 = geo_join(nyc_neighborhoods, neighborhood_67, "neighborhood","neighborhood")

leaflet_All= leaflet(map_data_All) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_All) %>%
  addPolygons(    fillColor = ~pal_All(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_13= leaflet(map_data_13) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_13) %>%
  addPolygons(    fillColor = ~pal_13(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_19= leaflet(map_data_19) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_19) %>%
  addPolygons(    fillColor = ~pal_19(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_25= leaflet(map_data_25) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_25) %>%
  addPolygons(    fillColor = ~pal_25(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_31= leaflet(map_data_31) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_31) %>%
  addPolygons(    fillColor = ~pal_31(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_37= leaflet(map_data_37) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_37) %>%
  addPolygons(    fillColor = ~pal_37(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_43= leaflet(map_data_43) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_43) %>%
  addPolygons(    fillColor = ~pal_43(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_49= leaflet(map_data_49) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_49) %>%
  addPolygons(    fillColor = ~pal_49(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_55= leaflet(map_data_55) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_55) %>%
  addPolygons(    fillColor = ~pal_55(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_61= leaflet(map_data_61) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_61) %>%
  addPolygons(    fillColor = ~pal_61(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

leaflet_67= leaflet(map_data_67) %>%
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 12) %>%
  addMarkers( ~lng, ~lat, data = neighborhoodTop10_67) %>%
  addPolygons(    fillColor = ~pal_67(count),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = .9,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~neighborhood
  )

