rm(list = ls())

library(readxl)
library(ggplot2)

DD5 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD5", col_types = c("blank", "numeric")))
DD10 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD10", col_types = c("blank", "numeric")))
DD21 <- as.numeric(read_excel("CARS.xlsx", sheet = "DD21", col_types = c("blank", "numeric")))
DU5 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU5", col_types = c("blank", "numeric")))
DU10 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU10", col_types = c("blank", "numeric")))
DU21 <- as.numeric(read_excel("CARS.xlsx", sheet = "DU21", col_types = c("blank", "numeric")))

png("DD5.png")
# hDD5 <-hist(DD5[[1]], breaks=20, col="blue",
#         main="DD5", xlab = "", ylab = "")

qplot(DD5$DD_CAR_5,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()

png("DD10.png")
qplot(DD10$DD_CAR_10,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()

png("DD21.png")
qplot(DD21$DD_CAR_21,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()

png("DU5.png")
qplot(DU5$DU_CAR_5,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()

png("DU10.png")
qplot(DU10$DU_CAR_10,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()

png("DU21.png")
qplot(DU21$DU_CAR_21,
      geom="histogram",
      binwidth=50, 
      xlab="CAR",
      ylab="",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))
dev.off()


