#first case study
#use hist()function
data(iris)
petal_length <- iris$Petal.Length
petal_length_mean <- mean(petal_length)
hist(petal_length, main = "Histogram for petal length", xlab = "length", 
     border = "blue", col = "green", las = 1, breaks = 7)

#use ggplot2 package
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))
qplot(petal_length, geom = "histogram", binwidth = 1, main = "Histogram for petal length", 
      xlab = "length", ylab = "count", fill = I("blue"), col = I("red"), alpha = I(0.2), ylim = c(0,40))

ggplot(data = iris, aes(petal_length))+geom_histogram(breaks=seq(1,7,by=0.5), col="red",
                                                      fill="green", alpha=0.2) +
  labs(title = "Histogram for petal length", x = "length", y = "count") + ylim(c(0,40))

