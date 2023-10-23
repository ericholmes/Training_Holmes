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
