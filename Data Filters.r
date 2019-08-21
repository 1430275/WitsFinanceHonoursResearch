library(dplyr)


PriceData <- subset.data.frame(Pricedf, c(Pricedf[, 1:449]) >100)