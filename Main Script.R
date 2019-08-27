rm(list = ls())

###################################
#Please see line 86 and beyond for reprex data and functions we need help with
###################################

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
  !all(is.na(column) | column == 0)
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

DDdf <-  as.data.frame(sapply(PriceData[-1], maxDD, lb = lkbk))
DDdf <-  cbind(PriceData$Date, DDdf)
names(DDdf)[names(DDdf) == "PriceData$Date"] <- "Date"

DUdf <-  as.data.frame(sapply(PriceData[-1], minDU, lb = lkbk))
DUdf <-  cbind(PriceData$Date, DUdf)
names(DUdf)[names(DUdf) == "PriceData$Date"] <- "Date"


####Reprex Data

datesEg <- seq(as.Date("2014-09-04"), by = "day", length.out = 6)
#DDsub <-DDdf[c(1:15),c(2:6)]
#DUsub <-DUdf[c(1:15),c(2:6)]

DDsub <-  data.frame("v1" = c(0, -0.0012, -0.1612, -0.1953, -0.2432, -0.1122), "v2" = c(0, 0, 0, 0, 0, 0), "v3" = c(0, -0.0021, -0.1233, -0.1242, -0.0043, -0.0033),"v4" = c(0, -0.0031, -0.3094, -0.1023, -0.0984, -0.01235),"v5" = c(0, NA, NA, NA, NA, NA))

DUsub <-  abs(DDsub)
reprexDD <- data.frame(datesEg, DDsub)  
reprexDU <- data.frame(datesEg, DUsub) 

##################################################
#We're Trying to get this to work
###This pulls dates and forms a list of lists of dates that fall under the 15% trigger.  

#dateDD <- DDdf$Date
#dateDU <- DUdf$Date

#DDtrig <- lapply(2:ncol(DDdf), function(i) dateDD[DDdf[[i]] < - 0.15])
#DUtrig <- lapply(2:ncol(DUdf), function(i) dateDU[DUdf[[i]]  > 0.15])


#This removes all NA columns - (may not be necessary)

#exclDD <-  sapply(DDtrig, NaFunction)
#DDtrig <-  DDtrig[exclDD]

#exclDU <-  sapply(DUtrig, NaFunction)
#DUtrig <-  DUtrig[exclDU]

#################################################