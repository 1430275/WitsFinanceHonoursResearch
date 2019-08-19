rm(list=ls())


library(readxl)
library(janitor)
PriceData <- read_excel("C:/Users/shane/University/2019/Research Report/Data/Price-Volume-MarketCap.xlsx", 
                        sheet = "Price (D)", 
                        na = ""
                        )

typeof(PriceData)
df <-  as.data.frame(PriceData)



df1 <-  remove_empty(df, "cols")