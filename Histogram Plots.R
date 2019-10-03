rm(list = ls())

DD5 <- read_excel("CARS.xlsx", sheet = "DD5", col_types = c("blank", "numeric", "blank","blank"))
DD10 <- read_excel("CARS.xlsx", sheet = "DD10", col_types = c("blank", "numeric"))
DD21 <- read_excel("CARS.xlsx", sheet = "DD21", col_types = c("blank", "numeric"))
DU5 <- read_excel("CARS.xlsx", sheet = "DU5", col_types = c("blank", "numeric"))
DU10 <- read_excel("CARS.xlsx", sheet = "DU10", col_types = c("blank", "numeric"))
DU21 <- read_excel("CARS.xlsx", sheet = "DU21", col_types = c("blank", "numeric"))

png("DD5.png")
hDD5 <-hist(DD5[[1]], breaks=20, col="blue",
        main="DD5", xlab = "", ylab = "")
dev.off()

png("DD10.png")
hDD10 <-hist(DD10[[1]], breaks=20, col="blue",
            main="DD10", xlab = "", ylab = "")
dev.off()

png("DD21.png")
hDD21 <-hist(DD21[[1]], breaks=20, col="blue",
            main="DD21", xlab = "", ylab = "")
dev.off()

png("DU5.png")
hDU5 <-hist(DU5[[1]], breaks=20, col="blue",
            main="DU5", xlab = "", ylab = "")
dev.off()

png("DU10.png")
hDU10 <-hist(DU10[[1]], breaks=20, col="blue",
            main="DU10", xlab = "", ylab = "")
dev.off()

png("DU21.png")
hDU21 <-hist(DU21[[1]], breaks=20, col="blue",
            main="DU21", xlab = "", ylab = "")
dev.off()


