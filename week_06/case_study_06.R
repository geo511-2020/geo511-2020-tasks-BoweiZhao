library(raster)
library(sp)
library(spData)
library(tidyverse)
library(sf)

data(world)
tmax_monthly <- getData(name = "worldclim", var = "tmax", res=10)
plot(tmax_monthly)

world_new <- world %>% filter(continent != "Antarctica")
as(world_new,"Spatial")

gain(tmax_monthly) <- 0.1
tmax_annual <- max(tmax_monthly)
names(tmax_annual) <- "tmax"
plot(tmax_annual)

maxtem_country <- raster::extract(tmax_annual,world_new, na.rm=T, small=T, sp = T, fun = max)
maxtem_country_sf <- st_as_sf(maxtem_country)

ggplot(maxtem_country_sf) + geom_sf(aes(fill = tmax)) + scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)") +
  theme(legend.position = 'bottom')
ggsave("Annual Maximum Temperature.png")

hottest_country <- maxtem_country_sf %>% select(name_long, continent, tmax) %>% st_set_geometry(NULL) %>%
  group_by(continent) %>% arrange(desc(tmax)) %>% top_n(1)
hottest_country

