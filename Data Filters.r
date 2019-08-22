library(dplyr)
library(naniar)

        

PriceData <- replace_with_na_all(Pricedf, ~.x < 100)

PriceData2 <-  remove_empty(PriceData, "cols")