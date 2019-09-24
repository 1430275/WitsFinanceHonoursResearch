rm(list = ls())

library(googledrive)
library(readxl)
library(qrmtools)


# Data Import -------------------------------------------------------------


#Download from Google Drive 
#Reads excel file and places data in dataframe

id <-  "1SGigXMnzubpP15Y1W18GqnlVQFiv95jN"
try(drive_download(as_id(id), overwrite = FALSE), silent = TRUE)
PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)")
#file.remove("Price-Volume-MarketCap.xlsx")


# Function Declarations ---------------------------------------------------

#Create index of Columns that do not contain only NA/0
NaFunction <-  function(column){
  !all(is.na(column) | column == 0)
}

#Removes SJ Equity from Column Names
colClean <- function(x){
  gsub(" SJ Equity", "", names(x), fixed = TRUE)
}

# Data Cleaning -----------------------------------------------------------

#Store Index of NaFunction as Dataframe and select them from original dataframe
exclP <-  sapply(PriceData, NaFunction)
PriceData <-  PriceData[exclP]

#Convert any text dates from data to date format
PriceData$Date <-  as.Date(PriceData$Date)

#Run colClean Function
names(PriceData) <- colClean(PriceData)



# Data Manipulation and Creation ------------------------------------------

#Create new matrix without dates and store values as numeric 

PriceDataNew <-  PriceData[, -1]

PriceDataNew <-  sapply(PriceDataNew, function(x){
  as.numeric(x)
})

#Create a new dataframe of returns using the returns function of qrmtools and clean out
returnsDf <- as.data.frame(returns(PriceDataNew, method = "simple"))
returnsDf <-  rbind(seq(from = 0, to = 0, length.out = 448), returnsDf)

#settings for functions
lookback <- 5 
window <- 5
triggerDD <- -0.15
triggerDU <- 0.15

#functions to calculate maximum drawdowns and minimum drawups
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

#functions run over the data and stored in a new dataframe
DDdf <-  as.data.frame(sapply(PriceData[-1], maxDD, lb = lookback))
DDdf <-  cbind(PriceData$Date, DDdf)
names(DDdf)[names(DDdf) == "PriceData$Date"] <- "Date"

DUdf <-  as.data.frame(sapply(PriceData[-1], minDU, lb = lookback))
DUdf <-  cbind(PriceData$Date, DUdf)
names(DUdf)[names(DUdf) == "PriceData$Date"] <- "Date"

#function that returns the index of the first time the drawdown/up breaches the trigger value as defined
#in the settings above

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

#delisting the lists created above to 1 column for each trigger value to be used to calculate CAARs and Buy and Hold Returns

ultrigDD <- unlist(trigIndexDD)
ultrigDD <- as.data.frame(t(ultrigDD))

ultrigDU <- unlist(trigIndexDU)
ultrigDU <- as.data.frame(t(ultrigDU))
