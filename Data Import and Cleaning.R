rm(list = ls())

library(googledrive)
library(readxl)
library(janitor)

id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)

PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)") 
VolumeData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Volume (D)") 
MCapData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Mcap (D)") 

Pricedf <-  remove_empty(PriceData, "cols")
Volumedf <-  remove_empty(VolumeData, "cols")
MCapdf <-  remove_empty(MCapData, "cols")

rm(PriceData, VolumeData, MCapData, id)