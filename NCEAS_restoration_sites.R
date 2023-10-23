library(tidyverse)
library(tidycensus)
library(sf)

site <- read.csv("C:/Users/eholmes/Documents/Spatial/NCEAS_restoration/data/spatial/ecoatlas_demo/ecoatlas_demo/ecoatlas_9522/ecoatlas_9522/sites/site.csv")

site_sf <- st_as_sf(site, wkt = "geom")

plot(site_sf)

frp <- read_sf("C:/Users/eholmes/Documents/Spatial/NCEAS_restoration/data/spatial/Vector/Eco_restore_polygons.shp")

# Removing Yolo Bypass
frp <- frp[frp$Total_Site < 17000,] 

## Bring in Census variables

delta_tracts <- get_acs(
  state = "CA",
  county = c("Alameda", "Contra Costa", "Sacramento", "San Joaquin", "Solano", "Yolo"),
  geography = "tract",
  variables = c("B19013_001","B01002_001", "B25071_001", "B25064_001"),
  geometry = TRUE,
  year = 2020
)

delta_tracts_wide <- get_acs(
  state = "CA",
  county = c("Alameda", "Contra Costa", "Sacramento", "San Joaquin", "Solano", "Yolo"),
  geography = "tract",
  variables = c("B19013_001","B01002_001", "B25071_001", "B25064_001"),
  geometry = TRUE,
  year = 2020,
  output = "wide"
)

delta_tracts_utm <- st_transform(delta_tracts, crs = st_crs(frp))

cowplot::plot_grid(
  ggplot(delta_tracts_utm[delta_tracts_utm$variable == "B01002_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median age") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts_utm[delta_tracts_utm$variable == "B19013_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median income") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts_utm[delta_tracts_utm$variable == "B25064_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median gross rent") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts_utm[delta_tracts_utm$variable == "B25071_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median gross rent as a proportion of income") + scale_fill_viridis_c() + theme_bw(),
  nrow = 2)

st_crs(frp)
st_crs(delta_tracts_utm)

## spatial join frp and census data

ggplot(delta_tracts_utm[delta_tracts_utm$variable == "B01002_001",]) + geom_sf(aes(fill = estimate)) +
  labs(title = "Median age") + scale_fill_viridis_c() + theme_bw() + geom_sf(data = frp, fill = "red")

frp_join <- st_join(frp, delta_tracts_utm)

ggplot(frp_join) + geom_sf(aes(fill = estimate))

library(mapview)

m1<-mapview(frp_join[frp_join$variable == "B01002_001",], z = "estimate")
m2<-mapview(frp_join[frp_join$variable == "B19013_001",], z = "estimate")
m3<-mapview(frp_join[frp_join$variable == "B25064_001",], z = "estimate")
m4<-mapview(frp_join[frp_join$variable == "B25071_001",], z = "estimate")

variables_acs <- load_variables(2021, "acs5")

# PCA ---------------------------------------------------------------------
library(leaflet)

leaflet() %>% addPolygons(frp_wide_join)

delta_tracts_wide_utm <- st_transform(delta_tracts_wide, crs = st_crs(frp))
frp_wide_join <- st_join(frp, delta_tracts_wide_utm, largest = T)

ggplot(frp_wide_join, aes(B01002_001E,B19013_001E)) + geom_point()

frp_num <- sf::st_drop_geometry(frp_wide_join[,c(26,28,30,32)])
frp_num <- frp_num[complete.cases(frp_num),]

frp_pca <- prcomp(frp_num, scale. = TRUE)

summary(frp_pca)
plot(frp_pca)
biplot(frp_pca)

corrplot::corrplot(cor(frp_num), type = "upper", method = "number")
frp_wide_join_wgs <- st_transform(frp_wide_join, crs = '+init=EPSG:4326')

# Leaflet map -------------------------------------------------------------

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles("Esri.WorldImagery", group = "Imagery") %>% 
  addPolygons(data = frp_wide_join_wgs, group = "FRP", label = ~B19013_001E,
             labelOptions = labelOptions(noHide = T)) %>% 
  addPolygons(data = delta_tracts_wide, group = "Census_tracts", color = "yellow") %>% 
  addLayersControl(
    baseGroups = c("Toner", "Imagery"),
    overlayGroups = c("FRP", "census_tracts"),
    options = layersControlOptions(collapsed = FALSE))
