rm(list = ls())

library(readxl)
library(ggplot2)
library(qrmtools)
library(moments)

PriceData <- read_excel("Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)",
                        na = c("", "NA", "#N/A", "#N/A #N/A", "#N/A Invalid Security"))
# DD5 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD5", col_types = c("blank", "numeric")))
# DD10 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD10", col_types = c("blank", "numeric")))
# DD21 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD21", col_types = c("blank", "numeric")))
# DU5 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU5", col_types = c("blank", "numeric")))
# DU10 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU10", col_types = c("blank", "numeric")))
# DU21 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU21", col_types = c("blank", "numeric")))

#Create index of Columns that do not contain only NA/0
NaFunction <-  function(column){
  !all(is.na(column) | column == 0)
}

#Removes SJ Equity from Column Names
# edited to remove ...num from duplicated column
colClean <- function(x){
  
  gsub(" SJ Equity", "", names(x), fixed = TRUE)
}

exclP <-  sapply(PriceData, NaFunction)
PriceData <-  PriceData[exclP]
PriceData <-  as.data.frame(PriceData)
PriceData$Date <-  as.Date(PriceData$Date)

PriceData <-  PriceData[,-1]

ret <- as.data.frame(sapply(PriceData, function(x){
  returns(x, "simple")
}))

means <- sapply(ret, function(x){
  mean(x, na.rm = T)
})

medians <- sapply(ret, function(x){
  median(x, na.rm = T)
})

stds <- sapply(ret, function(x){
  sd(x, na.rm = T)
})

variances <- sapply(ret, function(x){
  var(x, na.rm = T)
})

skewnesses <- sapply(ret, function(x){
  skewness(x, na.rm = T)
  
})

kurtosises <- sapply(ret, function(x){
  kurtosis(x, na.rm = T)
})
sum <-  summary(PriceData)

# png("Descriptive Stats Original.png")
# qplot(PriceData,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# 
# dev.off()

# png("DD5.png")
# qplot(DD5$DD_CAR_5,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
# png("DD10.png")
# qplot(DD10$DD_CAR_10,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
# png("DD21.png")
# qplot(DD21$DD_CAR_21,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
# png("DU5.png")
# qplot(DU5$DU_CAR_5,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
# png("DU10.png")
# qplot(DU10$DU_CAR_10,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
# png("DU21.png")
# qplot(DU21$DU_CAR_21,
#       geom="histogram",
#       binwidth=50,
#       xlab="CAR",
#       ylab="",
#       fill=I("blue"),
#       col=I("red"),
#       alpha=I(.2),
#       ....=c(20,50))
# dev.off()
#
#
