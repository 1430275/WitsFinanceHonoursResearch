rm(list = ls())

library(googledrive)
library(readxl)
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

#file.remove("Price-Volume-MarketCap.xlsx")


###Function Declaration
NaFunction <-  function(column){
  !all(is.na(column))
}

ZAr100Filter <-  function(column){
  PriceData<- vector(mode = "double", length = (length(column)))
  for (i in 1:length(column)){
    if(column[i] < 100){
      PriceData[i] <- NA
    } else{
      PriceData[i]
    }
  }
  return(PriceData)
}

#test <- as.data.frame(sapply(PriceData[-1], ZAr100Filter))

exclP <-  sapply(PriceData, NaFunction)
exclV <-  sapply(VolumeData, NaFunction)

Pricedf <-  PriceData[exclP]
Pricedf$Date <-  as.Date(Pricedf$Date)
Volumedf <-  VolumeData[exclV]
Volumedf$Date <-  as.Date(Volumedf$Date)

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

minDU <-  function(column, lb){
  du <- vector(mode = "double", length = (length(column) - lb))
  for (i in (lb+1):length(column)){
    if(is.na(min(column[(i-lb):i]))){
      du[i] <- NA
    } else{
      du[i] <- (column[i] - min(column[(i-lb):i], na.rm = T))/min(column[(i-lb):i], na.rm = T)
    }
  }
 return(du)
}

DDdf <-  as.data.frame(sapply(Pricedf[-1], maxDD, lb = lkbk))
DDdf <-  cbind(Pricedf$Date, DDdf)
names(DDdf)[names(DDdf) == "Pricedf$Date"] <- "Date"

DUdf <-  as.data.frame(sapply(Pricedf[-1], minDU, lb = lkbk))
DUdf <-  cbind(Pricedf$Date, DUdf)
names(DUdf)[names(DUdf) == "Pricedf$Date"] <- "Date"

dateDD <- DDdf$Date
dateDU <- DUdf$Date

DDtrig <- lapply(2:ncol(DDdf), function(i) dateDD[DDdf[[i]] < - 0.15])
DUtrig <- lapply(2:ncol(DUdf), function(i) dateDU[DUdf[[i]] < - 0.15])

rm(list=setdiff(ls(), c("DDtrig", "DUtrig")))
