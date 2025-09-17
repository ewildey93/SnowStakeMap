library(leaflet)
library(sf)
library(ggplot2)
library(ggmap)

register_google()
sites <- read.csv("./PotentialSnowStakeDistributionLocations(Sheet1).csv", nrows = 40)
siteSF <- st_as_sf(x=sites, coords=c("Longitude", "Latitude"), crs=4326)
Wisconsin <- st_read("C:/Users/wildeefb/Documents/GeoSpatial/VectorLayers/Wisconsin_State_Boundary_24K.shp")
Wisconsin3857 <- st_transform(Wisconsin, 3857)
WisconsinMain <- Wisconsin4326[c(119,134),]
plot(st_geometry(WisconsinMain))
plot(st_geometry(Wisconsin))

leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=st_transform(Wisconsin, 4326), fill = NA, color="white")%>%
  addCircleMarkers(data=siteSF, fillColor = "black", fillOpacity = 1,   stroke=F, radius=5, popup=paste(siteSF$Organization,siteSF$Open.hours, sep="<br>"))


map <- get_map(location = c(st_coordinates(st_centroid(WisconsinMain))[1,1], st_coordinates(st_centroid(WisconsinMain))[1,2]),
               maptype="hybrid", source="google", zoom=7)
map2 <- get_map(location = c(st_coordinates(st_centroid(WisconsinMain))[1,1], st_coordinates(st_centroid(WisconsinMain))[1,2]),
               maptype="roadmap", source="google", zoom=7)

ggmap(map2) +
  geom_sf(data=siteSF,inherit.aes=FALSE, show.legend = "point") #+
  geom_sf_text(data=siteSF,inherit.aes = FALSE, aes(label=Organization), show.legend=FALSE, color="white", size=3)
