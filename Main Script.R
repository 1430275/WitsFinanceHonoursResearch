rm(list = ls())

library(googledrive)
library(readxl)
library(xts)
library(lubridate)
library(dplyr)

id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)

PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)") 
VolumeData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Volume (D)") 

#file.remove("Price-Volume-MarketCap.xlsx")


###Function Declaration
NaFunction <-  function(column){
  !all(is.na(column) | column == 0)
}

colClean1 <- function(x){
  gsub(" SJ Equity", "", names(x), fixed = TRUE)
}

colClean2 <- function(x){
  gsub(".SJ.Equity", "", names(x), fixed = TRUE)
}

#ZAr100Filter <-  function(column){
#  PriceData<- vector(mode = "double", length = (length(column)))
#  for (i in 1:length(column)){
#    if(column[i] < 100){
#      PriceData[i] <- NA
#    } else{
#      PriceData[i]
#    }
#  }
#  return(PriceData)
#}

#test <- as.data.frame(sapply(PriceData[-1], ZAr100Filter))

exclP <-  sapply(PriceData, NaFunction)
exclV <-  sapply(VolumeData, NaFunction)

PriceData <-  PriceData[exclP]
PriceData$Date <-  as.Date(PriceData$Date)
VolumeData <-  VolumeData[exclV]
VolumeData$Date <-  as.Date(VolumeData$Date)

names(PriceData) <- colClean1(PriceData)
names(VolumeData) <- colClean1(VolumeData)

#settings
lookback <- 5 
window <- 5

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

DDdf <-  as.data.frame(sapply(PriceData[-1], maxDD, lb = lookback))
DDdf <-  cbind(PriceData$Date, DDdf)
names(DDdf)[names(DDdf) == "PriceData$Date"] <- "Date"

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

DUdf <-  as.data.frame(sapply(PriceData[-1], minDU, lb = lookback))
DUdf <-  cbind(PriceData$Date, DUdf)
names(DUdf)[names(DUdf) == "PriceData$Date"] <- "Date"



###############################



DDdf <- read.xlsx(xlsxFile = "DDdf.xlsx", detectDates = TRUE, skipEmptyCols = FALSE, skipEmptyRows = FALSE)
DUdf <- read.xlsx(xlsxFile = "DUdf.xlsx", detectDates = TRUE, skipEmptyCols = FALSE, skipEmptyRows = FALSE)

trigIndexDD <- lapply(DDdf[-1], function(i){
  trigDD <- vector(mode = "integer", length = 1)
  triggerDD <- -0.15
  l = length(i)
  s = 0
  pos = 1
  while(s <= l){
    x = which(i[(s+1):l] <= -triggerDD)[1]+s
    if(is.na(x)){
      break
    } else{
      trigDD[pos] = x
    }
    s = trigDD[pos] + window
    pos = pos + 1
  }
  return(trigDD)
})

dateIndexDD <- lapply(trigIndexDD, function(x){
  DDdf$Date[x]
})

excldiDD <-  sapply(dateIndexDD, NaFunction)
dateIndexDD <- dateIndexDD[excldiDD]



trigIndexDU <- lapply(DUdf[-1], function(i){
  trigDU <- vector(mode = "integer", length = 1)
  triggerDU <- 0.15
  l = length(i)
  s = 0
  pos = 1
  while(s <= l){
    x = which(i[(s+1):l] <= triggerDU)[1]+s
    if(is.na(x)){
      break
    } else{
      trigDU[pos] = x
    }
    s = trigDU[pos] + window
    pos = pos + 1
  }
  return(trigDU)
})

dateIndexDU <- lapply(trigIndexDU, function(x){
  DUdf$Date[x]
})


excldiDU <-  sapply(dateIndexDU, NaFunction)
dateIndexDU <- dateIndexDU[excldiDU]

names(dateIndexDD) <- colClean2(dateIndexDD)
names(dateIndexDU) <- colClean2(dateIndexDU)

