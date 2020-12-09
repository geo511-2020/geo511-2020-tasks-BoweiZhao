library(tidyverse)
library(ggplot2)

# define the link to the data - you can try this in your browser too.  Note that the URL ends in .csv.
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_USW00014733_14_0_1/station.csv"

temp=read_csv(dataurl,
              skip=1, #skip the first line which has column names
              na="999.90", # tell R that 999.90 means missing in this dataset
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary becuase they used dashes ("-")
# in the column names and R doesn't like that.

view(temp)
ggplot(temp, aes(YEAR, JJA)) + geom_path() + geom_smooth(color = "red") +
  xlab("Year") + ylab("Mean Summer Temperature(C)") +
  labs(title = "Mean Summer Temperatures in Buffalo, NY", 
  subtitle = "Summer includes June, July,and August
Data from the Global Historical Climate Network
Red line is a LOESS smooth")

ggsave("Mean Summer Temperatures in Buffalo, NY.png")  
