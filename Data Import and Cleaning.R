rm(list = ls())

library(googledrive)
library(readxl)
library(janitor)
library(naniar)
library(xts)
library(lubridate)

id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)

PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)") 
PriceData <- replace_with_na_all(PriceData, ~.x < 100)
VolumeData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Volume (D)") 
VolumeData <- replace_with_na_all(VolumeData, ~.x < 1)

Pricedf <-  remove_empty(PriceData, "cols")
Volumedf <-  remove_empty(VolumeData, "cols")

rm(PriceData, VolumeData, id)

Pricedf$Date <-  as.Date(Pricedf$Date)

Returns <- xts(Pricedf[,-1], order.by= Pricedf$Date)
Returns <- diff(Returns, arithmetic=FALSE ) - 1
