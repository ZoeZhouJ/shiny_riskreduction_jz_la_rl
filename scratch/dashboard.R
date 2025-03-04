
library(jsonlite)
map <- fromJSON('raw-data/usgs.json', flatten = TRUE)
head(map)