
# want to write a function that calculates the standard deviation of the geometric mean based on the previous function and creates a new column in the data frame with those values
library(stats)
library(dplyr)

geo_sd <- function(x) {
  geosd <- exp(sd(log(1+x)-1))-1
  return(geosd)
}
