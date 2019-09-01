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
triggerDD <- -0.15
triggerDU <- 0.15

maxDD <-  function(column, lb){
  dd <- vector(mode = "double", length = (length(column) - lb))
  for (i in (lb+1):length(column)){
    if(is.na(max(column[(i-lb):i]))){
      dd[i] <- 0
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
      du[i] <- 0
    } else{
      du[i] <- (column[i] - min(column[(i-lb):i], na.rm = T))/min(column[(i-lb):i], na.rm = T)
    }
  }
 return(du)
}

DUdf <-  as.data.frame(sapply(PriceData[-1], minDU, lb = lookback))
DUdf <-  cbind(PriceData$Date, DUdf)
names(DUdf)[names(DUdf) == "PriceData$Date"] <- "Date"

#DDdf <-  DDdf[-c(1:5), ]
#DUdf <-  DUdf[-c(1:5), ]

#exclDD <-  sapply(DDdf, NaFunction)
#DDdf <- DDdf[exclDD]

#exclDU <-  sapply(DUdf, NaFunction)
#DUdf <- DUdf[exclDU]




trigIndexDD <- lapply(DDdf[-1], function(i){
  trigDD <- vector(mode = "integer", length = 1)
  l = length(i)
  s = 0
  pos = 1
  while(s <= l){
    x = which(i[(s+1):l] <= triggerDD)[1]+s
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

trigIndexDU <- lapply(DUdf[-1], function(i){
  trigDU <- vector(mode = "integer", length = 1)
  l = length(i)
  s = 0
  pos = 1
  while(s <= l){
    x = which(i[(s+1):l] >= triggerDU)[1]+s
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

#valuesDD <-  sapply(PriceData[-1], function(i){
#  PriceData[[i]][match(unlist(trigIndexDD[,i]), PriceData[[i]])]
#})

valueDD <-  PriceData[[2]][trigIndexDD[[1]]]



##########################
#dateIndexDD <- lapply(trigIndexDD, function(x){
#    PriceData[[x]]
#})

#excldiDD <-  sapply(dateIndexDD, NaFunction)
#dateIndexDD <- dateIndexDD[excldiDD]

#dateIndexDU <- lapply(trigIndexDU, function(x){
#    PriceData[[x]]
#})

#excldiDU <-  sapply(dateIndexDU, NaFunction)
#dateIndexDU <- dateIndexDU[excldiDU]

#names(dateIndexDD) <- colClean2(dateIndexDD)
#names(dateIndexDU) <- colClean2(dateIndexDU)

#rowIndex <-  function(columnP, columnD){
#  rowDD <- vector(mode = "integer", length = length(columnD))
#  for ( i in 1:length(column)){
#    rowIndex[i] <-  which(grepl())
    
#  }
#}

#rowIndexDD <-  lapply(dateIndexDD, function(i){
#  for (i in 1:278){
#    grepl()
#  }
#})