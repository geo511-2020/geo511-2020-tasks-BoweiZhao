library(tidyverse)
library(nycflights13)

airports = nycflights13::airports

#According to my group classmates(Collin, Marko, and Jay) suggestion, the next line of code
#has been turned into a more professional way
flights = nycflights13::flights %>% filter(origin =="JFK"|origin == "EWR" |origin == "LGA")
ordered_distance = arrange(flights, desc(distance))
farthest_airport = ordered_distance[1,c(13,14,16)]
farthest_airport
names(airports)[1] = "dest"
farthest_fullname = farthest_airport %>% left_join(airports, by = "dest") %>% select(name)
farthest_fullname 

