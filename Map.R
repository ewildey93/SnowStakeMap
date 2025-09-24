library(leaflet)
library(sf)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(patchwork)
library(sswids)
library(dplyr)
library(terra)
library(tidyterra)


sites <- read.csv("./ConfirmedSites.csv")%>%arrange(desc(Latitude))
sitestab <- sites[,c("Pickup.Site.Name", "Pickup.Address","Pickup.Hours")]
siteSF <- st_as_sf(x=sites, coords=c("Longitude", "Latitude"), crs=4326)
Wisconsin <- st_read("C:/Users/wildeefb/Documents/GeoSpatial/VectorLayers/Wisconsin_State_Boundary_24K.shp")
Wisconsin4326 <- st_transform(Wisconsin, 4326)
#Wisconsin without all the tiny islands that throw off centroid calculation
WisconsinMain <- Wisconsin4326[c(119,134),]
Counties <- get_spatial_data("counties")
Counties4326 <- st_transform(Counties, 4326)
MajorRoads <- get_spatial_data("major_roads")
MajorRoads4326 <- st_transform(MajorRoads, 4326)
plot(st_geometry(MajorRoads))

#download basemap
map2 <- get_map(location = c(st_coordinates(st_centroid(WisconsinMain))[1,1], st_coordinates(st_centroid(WisconsinMain))[1,2]),
                maptype="stamen_terrain_background", source="stadia", zoom=7)

#convert map to raster
map2rast <- rast(map2)
#clip to state of Wisconsin make values outside mask NA so they are transparent
WisconsinClip <- terra::crop(map2rast, Wisconsin4326)
WisconsinClip <- terra::mask(map2rast, Wisconsin4326, updatevalue=NA)

sitemap <- ggplot() +
  geom_spatraster_rgb(data=WisconsinClip) + 
  geom_sf(data=MajorRoads, inherit.aes = FALSE, color="gray") +
  geom_sf(data=Counties4326, inherit.aes=FALSE, fill=NA, color="black") +
  geom_sf_label(data=siteSF,inherit.aes = FALSE, aes(label=1:nrow(siteSF)),
                show.legend=FALSE, color="black", size=4) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())




#make theme for table, only thing set is font size
mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 0.6)), #fg_params = foreground parameters
  colhead = list(fg_params=list(cex = 0.7)),
  rowhead = list(fg_params=list(cex = 0.7)))
#turn table into a tableGrob (whatever that is) for input into ggplot object
sitestableGrob <- tableGrob(sitestab, theme=mytheme, cols = c("Pick Up Site", "Address", "Hours"))

#make table a ggplot object
tableplot <- ggplot() + theme_void() + 
  annotation_custom(sitestableGrob)

#combine into 1 figure
sitemap + tableplot #+ plot_layout(ncol = 1) - for vertical layout



###################################################################################################
####                  scrap paper (old code) you can ignore everything below this line         ####
###################################################################################################
#move labels on map
#https://stackoverflow.com/questions/78529290/position-dodge-doesnt-dodge-geom-sf-label
library(sf)
library(viridis)
library(dplyr)
library(ggplot2)

# Get US sf
us_states <- states(cb = TRUE, resolution = "20m")

# Shift non-contiguous states, add centroid lon/lat values for ggrepel,
# address label placement issue with Mississippi and Alabama
us_states_shifted <- shift_geometry(us_states) %>%
  mutate(lon = st_coordinates(st_centroid(.))[,1],
         lat = st_coordinates(st_centroid(.))[,2],
         lat = case_when(NAME == "Mississippi" ~ lat -0.5e5,
                         NAME == "Alabama" ~ lat + 0.5e5,
                         .default = lat))

# Create vector of remaining problem labels
move_labels <- c("Connecticut", "Delaware", "District of Columbia",
                 "Maryland", "Massachusetts", "New Hampshire", "New Jersey",
                 "Rhode Island", "Vermont")

# Subset states and assign offset values for labels and segments
move_states <- us_states_shifted %>%
  filter(NAME %in% move_labels) %>%
  arrange(lat) %>%
  mutate(xend = 2.2e6,
         yend = seq(min(lat)-2.5e5, max(lat), length.out = n()))

# Plot
us_states_shifted %>% 
  ggplot(aes(fill = AWATER)) +
  geom_sf()+
  scale_fill_viridis(option = "virdis") +
  geom_label(data = filter(us_states_shifted, !NAME %in% move_labels),
             aes(x = lon, y = lat,
                 label = NAME),
             fill = "white",
             size = 2) +
  geom_label(data = move_states,
             aes(x = xend, y = yend,
                 label = NAME),
             fill = "white",
             size = 2,
             hjust = 0) +
  geom_segment(data = move_states, 
               aes(lon, lat, xend = xend, yend = yend),
               colour = "grey60",
               linewidth = 0.3) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("Amount of Water In Each State")



ggmap(map2, baselayer=Wisconsin4326, maprange=TRUE)
sitemap <- ggmap(map2, baselayer=Wisconsin4326, maprange=TRUE) +
  geom_sf(data=Wisconsin4326, inherit.aes=FALSE, fill=NA) +
  #geom_sf(data=siteSF,inherit.aes=FALSE, show.legend = "point")+
  geom_sf(data=Counties4326, inherit.aes=FALSE, fill=NA) +
  geom_sf_label(data=siteSF,inherit.aes = FALSE, aes(label=1:nrow(siteSF)),
                show.legend=FALSE, color="black", size=4)
sitemap

halfsitemap <- ggmap(map2) +
  geom_sf(data=Wisconsin4326, inherit.aes=FALSE, fill=NA) +
  geom_sf(data=Counties4326, inherit.aes=FALSE, fill=NA) +
  #geom_sf(data=siteSF,inherit.aes=FALSE, show.legend = "point")+
  geom_sf_label(data=halfsiteSF,inherit.aes = FALSE, aes(label=rownames(halfsiteSF)),
                show.legend=FALSE, color="black", size=4, alpha = 0.75)

ggplot() +
  geom_sf(data=Wisconsin4326,inherit.aes=FALSE, fill=NA) +
  geom_sf(data=Counties4326, inherit.aes=FALSE, fill=NA) +
  #geom_sf(data=MajorRoads, inherit.aes=FALSE) +
  #geom_sf(data=siteSF,inherit.aes=FALSE, show.legend = "point")+
  geom_sf_label(data=halfsiteSF,inherit.aes = FALSE, aes(label=rownames(halfsiteSF)),
                show.legend=FALSE, color="black", size=4, alpha = 0.75) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data=st_transform(Wisconsin, 4326), fill = NA, color="white")%>%
  addCircleMarkers(data=siteSF, fillColor = "black", fillOpacity = 1,   stroke=F, radius=5, popup=paste(siteSF$Organization,siteSF$Open.hours, sep="<br>"))


map <- get_map(location = c(st_coordinates(st_centroid(WisconsinMain))[1,1], st_coordinates(st_centroid(WisconsinMain))[1,2]),
               maptype="roadmap", source="google", zoom=7)



  
  
rows <- sample(x = nrow(sitestab), size = 20, replace = FALSE)
halfsitestab <- sitestab[rows,]%>%arrange()
halfsiteSF <- siteSF[rows,]

browseVignettes("gridExtra")



#function to convert ggmap to raster
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- ext(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- rast(.extent, nrow= nrow(map2), ncol = ncol(map2))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  c(red,green,blue)
}

map2rast <- ggmap_rast(map2)
names(map2rast) <- c("red","green", "blue")

values <- terra::values(WisconsinClip)
xy <- terra::xyFromCell(WisconsinClip, 1:(dim(WisconsinClip)[1]*dim(WisconsinClip)[2]))
Pointsdf <- cbind.data.frame(xy, values)
Pointsdf2 <- Pointsdf[!rowSums(is.na(Pointsdf[3:5])),]
sitemap <- ggplot(Pointsdf2) +
  geom_point(aes(x=x, y=y, col=rgb(red/255, green/255, blue/255))) + 
  geom_sf(data=MajorRoads, inherit.aes = FALSE, color="gray") +
  geom_sf(data=Counties4326, inherit.aes=FALSE, fill=NA, color="black") +
  geom_sf_label(data=siteSF,inherit.aes = FALSE, aes(label=1:nrow(siteSF)),
                show.legend=FALSE, color="black", size=4) +
  scale_color_identity() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())