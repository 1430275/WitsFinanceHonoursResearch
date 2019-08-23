rm(list = ls())

library(googledrive)
library(readxl)
library(janitor)
library(naniar)
library(xts)
library(lubridate)

#settings
lkbk <- 5 

id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)

PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)") 
VolumeData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Volume (D)") 

NaFunction <-  function(column){
  !all(is.na(column))
}

exclP <-  sapply(PriceData, NaFunction)
exclV <-  sapply(VolumeData, NaFunction)

Pricedf <-  PriceData[exclP]
Pricedf$Date <-  as.Date(Pricedf$Date)
Volumedf <-  VolumeData[exclV]
#Volumedf$Date <-  as.Date(Volumedf$Date)

rm(PriceData, VolumeData, id, exclP, exclV)

maxDD <-  function(column, lb){
  dd <- vector(mode = "double", length = (length(column) - lb))
  for (i in (lb+1):length(column)){
    if(is.na(max(column[(i-lb):i]))){
      dd[i] <- NA
    } else{
      dd[i] <- (column[i] - max(column[(i-lb):i], na.rm = T))/max(column[(i-lb):i], na.rm = T)
    }
  }
  return(dd)
}

ddDf <-  as.data.frame(sapply(Pricedf[-1], maxDD, lb = lkbk))

d <- Pricedf[[1]]

d[ddDf[[1]] < -0.2]


test <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
maxDD(test, lkbk)
