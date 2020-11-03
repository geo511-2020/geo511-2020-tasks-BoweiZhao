library(sf)
library(tidyverse)
library(ggmap)
library(rnoaa)
library(spData)
data(world)
data(us_states)

# 2020 update - it appears NOAA changed the URL which broke the R function.  Use the following instead of storm_shp().
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.NA.list.v04r00.points.zip"
tdir=tempdir()
download.file(dataurl,destfile=file.path(tdir,"temp.zip"))
unzip(file.path(tdir,"temp.zip"),exdir = tdir)
list.files(tdir)

storm_data <- read_sf(list.files(tdir,pattern=".shp",full.names = T))
storm_nobasin = storm_data %>% filter(BASIN == "NA")

storm_after1950 = storm_nobasin %>% filter(SEASON == "1950" | SEASON > "1950")
storm_NA = storm_after1950 %>% mutate_if(is.numeric, function(x) ifelse(x==-999.0,NA,x)) %>%
  mutate(decade=(floor(year/10)*10))
region = st_bbox(storm_NA)

ggplot(world) + geom_sf(colour = "grey4") + facet_wrap(~decade) +
  stat_bin2d(data=storm_NA, aes(y=st_coordinates(storm_NA)[,2], x=st_coordinates(storm_NA)[,1]),bins=100) +
  scale_fill_distiller(palette="YlOrRd", trans="log", direction=-1, breaks = c(1,10,100,1000)) +
  coord_sf(ylim=region[c(2,4)], xlim=region[c(1,3)]) + theme(axis.title=element_blank())

us_transform = st_transform(us_states, crs = st_crs(storm_NA))
us_rename = us_transform %>% select(state=NAME)
storm_states <- st_join(storm_NA, us_rename, join = st_intersects,left = F)
most_five_states =  storm_states %>% group_by(state) %>% summarize(storms=length(unique(NAME))) %>%
  arrange(desc(storms)) %>% slice(1:5)
most_five_states
