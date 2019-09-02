rm(list = ls())

library(googledrive)
library(readxl)
library(lubridate)
library(qrmtools)
library(dplyr)


id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)
PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)") 
#file.remove("Price-Volume-MarketCap.xlsx")


###Function Declaration
NaFunction <-  function(column){
  !all(is.na(column) | column == 0)
}


colClean1 <- function(x){
  gsub(" SJ Equity", "", names(x), fixed = TRUE)
}

exclP <-  sapply(PriceData, NaFunction)

PriceData <-  PriceData[exclP]
PriceData$Date <-  as.Date(PriceData$Date)
names(PriceData) <- colClean1(PriceData)

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

DDdf <-  DDdf[-c(1:5), ]
DUdf <-  DUdf[-c(1:5), ]

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

PriceDataNew <-  PriceData[, -1]

PriceDataNew <-  sapply(PriceDataNew, function(x){
  as.numeric(x)
})

returnsDf <- as.data.frame(returns(PriceDataNew, method = "simple"))
returnsDf <-  rbind(seq(from = 0, to = 0, length.out = 448), returnsDf)

ultrigDD <- unlist(trigIndexDD)
ultrigDD <- t(ultrigDD)

# valuesDD <- mapply(function(x,y) x[sub("_3.*", "", colnames(y))], as.data.frame(returnsDf), trigIndexDD)


