library(leaflet)
library(sf)


sites <- read.csv("./PotentialSnowStakeDistributionLocations(Sheet1).csv", nrows = 40)
siteSF <- st_as_sf(x=sites, coords=c("Longitude", "Latitude"), crs=4326)
Wisconsin <- st_read("C:/Users/wildeefb/Documents/GeoSpatial/VectorLayers/Wisconsin_State_Boundary_24K.shp")


leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=st_transform(Wisconsin, 4326), fill = NA, color="white")%>%
  addCircleMarkers(data=siteSF, fillColor = "black", fillOpacity = 1,   stroke=F, radius=5, popup=paste(siteSF$Organization,siteSF$Open.hours, sep="<br>"))
