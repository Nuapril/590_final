geo_mean <- function(x) {  
  #need to put if statements into function
  # if x is not a vector/df return x is not a vector/data frame
  #if(is.data.frame(df) != TRUE) {
  #stop("Argument df is not a data frame")
  #}
# if a value of x is a negative return negative value in df
# want to write a function in which the geometric mean is calculated for every three rows in a column, then creates a new column with those values
  
  geomean <- exp(sum(log(x[x > 0]), na.rm = TRUE) / length(x))
  #geosd <- exp(sd(log(1+x)-1))-1
  #return(c(geomean, geosd))
  return(geomean)
}

geo_sd <- function(x) {
  geosd <- exp(sd(log(1+x)-1))-1
  return(geosd)

}

#geo_mean_append <- function(df, i, j, k, l){
  #df is dataframe, all other values are column in df
  #print(seq_along(df[,2]-1) %/% 3)
  #b <-  unname(tapply(df[,2], (seq_along(df[,2])-1) %/% 3, geo_mean))
  #df[,l] <- b
  #return (df)
#}
#geo_mean_append(test2, "Sample", "E", "Ct", "Geomean")





