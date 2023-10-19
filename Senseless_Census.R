library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

pop_2020 <- get_decennial(geography = "state",
                          variable = "P1_001N",
                          year = 2020)

table_p2_2020 <- get_decennial(geography = "state",
                               table = "P2",
                               year = 2020)
unique(table_p2_2020$variable)

var_pl_2020 <- load_variables(2020, "pl")

var_dhc_2020 <- load_variables(2020, "dhc")

delta_hisp <- get_decennial(
  geography = "county",
  state = "CA",
  county = c("Alameda", "Contra Costa", "Sacramento", "San Joaquin", "Solano", "Yolo"),
  variable = c("P2_002N", "P2_003N"),
  year = 2020
)

options(tigris_use_cache = TRUE)

all_vars_acs5 <- 
  load_variables(year = 2020, dataset = "acs5")
# B25035_001 Estimate!!Median year structure built
# medinc = "B19013_001", 
# medage = "B01002_001"
# B25064_001 Estimate!!Median gross rent
# B25071_001 gross rent as a 
# "B15012_011" Field of bachelor degree

delta_tracts <- get_acs(
  state = "CA",
  county = c("Alameda", "Contra Costa", "Sacramento", "San Joaquin", "Solano", "Yolo"),
  geography = "tract",
  variables = c("B19013_001","B01002_001", "B25071_001", "B25064_001"),
  geometry = TRUE,
  year = 2020
)

cowplot::plot_grid(
  ggplot(delta_tracts[delta_tracts$variable == "B01002_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median age") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B19013_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median income") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B25064_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median gross rent") + scale_fill_viridis_c() + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B25071_001",], aes(fill = estimate)) + geom_sf() +
    labs(title = "Median gross rent as a proportion of income") + scale_fill_viridis_c() + theme_bw(),
  nrow = 2)

cowplot::plot_grid(
  ggplot(delta_tracts[delta_tracts$variable == "B01002_001",], aes(x = estimate)) + geom_histogram() +
    labs(title = "Median age") + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B19013_001",], aes(x = estimate)) + geom_histogram() +
    labs(title = "Median income") + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B25064_001",], aes(x = estimate)) + geom_histogram() +
    labs(title = "Median gross rent") + theme_bw(),
  ggplot(delta_tracts[delta_tracts$variable == "B25071_001",], aes(x = estimate)) + geom_histogram() +
    labs(title = "Median gross rent as a proportion of income") + theme_bw(),
  nrow = 2)

delta_tracts_wide <- delta_tracts %>% 
  pivot_wider(id_cols = c("GEOID", "NAME"), names_from = "variable", values_from = "estimate") %>% 
  data.frame()

ggplot(delta_tracts_wide, aes(x = B01002_001, y = B19013_001)) + geom_point()
ggplot(delta_tracts_wide, aes(x = B25064_001, y = B19013_001)) + geom_point()

ggplot(delta_tracts_wide, aes(x = B01002_001, y = B25071_001)) + geom_point()

library(segregation)

delta_tracts_seg <- delta_tracts %>% 
  mutual_local(
    group = "variable",
    unit = "GEOID",
    weight = "estimate", 
    wide = TRUE
  )

delta_tracts_segregated <- merge(delta_tracts, delta_tracts_seg, by = "GEOID", all.x = T)

ggplot(delta_tracts_segregated[delta_tracts_segregated$variable == "B01002_001",], aes(fill = sqrt(ls))) + geom_sf() +
  labs(title = "Segregation index") + scale_fill_viridis_c() + theme_bw()
  
library(tidyverse)
delta_blocks <- get_decennial(
  state = "CA",
  county = c("Alameda", "Contra Costa", "Sacramento", "San Joaquin", "Solano", "Yolo"),
  geography = "block",
  variables = "P2_002N",
  geometry = TRUE,
  year = 2020
)

ggplot(delta_blocks, aes(fill = value)) + geom_sf()

# st_write(delta_blocks, "data/delta_census_blocks.shp")