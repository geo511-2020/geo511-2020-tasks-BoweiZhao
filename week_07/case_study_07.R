library(tidyverse)
library(reprex)
library(sf)
library(spData)

data(world)
ggplot(world,aes(x=gdpPercap, y=continent, color=continent))+
  geom_density(alpha=0.5,color=F)

#fixed the code
result <- world %>% group_by(continent) %>% 
  ggplot(aes(x=gdpPercap, fill = continent))+geom_density(alpha=0.5, color = NA) +
  theme(legend.position = "bottom") + labs(x = "GDP Per Capita", y = "Density")
result

