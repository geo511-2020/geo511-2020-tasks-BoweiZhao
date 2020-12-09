library(sf)
library(spData)
library(tidyverse)
library(units)
library(rgeos)

data(world)
data(us_states)
plot(world[2])
plot(us_states[2])

albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +
ellps=GRS80 +datum=NAD83 +units=m +no_defs"
transform_world <- st_transform(world, albers)
plot(transform_world[2])
transform_us_states <- st_transform(us_states, albers)
plot(transform_us_states[2])

canada_border <- filter(transform_world, name_long == "Canada")
canada_buffer <- st_buffer(canada_border, 10000)  
new_york_state <- filter(transform_us_states, NAME == "New York")

final_polygon <- st_intersection(canada_buffer, new_york_state) 
ggplot(new_york_state) + geom_sf() + geom_sf(data = final_polygon[2], fill = "red") + 
  xlab("Longitude") + ylab("Latitude") + ggtitle("New York Land Within 10KM") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

area <- st_area(final_polygon)
area_km <- set_units(area, km^2)                                               
area_km


