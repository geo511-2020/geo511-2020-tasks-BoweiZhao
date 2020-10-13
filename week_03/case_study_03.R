library(ggplot2)
library(gapminder)
library(dplyr)

summary(gapminder)
gapminder_new = filter(gapminder, country != "Kuwait")
ggplot(data = gapminder_new, aes(x = lifeExp, y = gdpPercap)) +
  geom_point(aes(size=pop/100000, color = continent)) + facet_wrap(~year,nrow=1) +
  scale_y_continuous(trans = "sqrt") + theme_bw() +
  xlab("Life Expectancy") + ylab("GDP per capita") + scale_size_continuous("Population (100k)") +
  scale_color_discrete("Continent")
ggsave("Life Expectancy vs GDP per capita.png", width = 15)  


gapminder_continent <- gapminder_new %>% group_by(continent, year) %>% 
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),
            pop = sum(as.numeric(pop)))
ggplot(data = gapminder_new, aes(x = year, y = gdpPercap,group = country, color = continent)) +
  geom_line() + geom_point() + 
  geom_line(data = gapminder_continent, aes(x = year, y = gdpPercapweighted), inherit.aes = FALSE) +
  geom_point(data = gapminder_continent, aes(x = year, y = gdpPercapweighted,size=pop/100000), inherit.aes = FALSE) +
  facet_wrap(~continent,nrow=1) + theme_bw() +
  labs(x = "Year", y = "GDP per capita", size = "Population(100K)")
ggsave("Year vs GDP per capita.png", width = 15)  

  