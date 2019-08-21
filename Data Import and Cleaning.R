rm(list = ls())

library(googledrive)
library(readr)
library(janitor)

wd <-  getwd()

idP <- "1FWCX68QO9VUT5HVPP2rMuA5GI5XH2JVl" # google file ID
idV <- "1tfeAcTXbJT-Md-Wsae0LfLJJ8M7k7uw5"
idM <- "1SxQtzVoAN8HczZLI8LZzsWDYmYDItdl8"



PriceFile <- drive_download(as_id(idP), overwrite = TRUE)
VolumeFile <-drive_download(as_id(idV), overwrite = TRUE)
MCapFile <- drive_download(as_id(idM), overwrite = TRUE)

PriceData <-  read.csv(paste0(wd, "/" ,PriceFile[1,1], sep=""))
VolumeData <-  read.csv(paste(wd, "/" , VolumeFile[1,1], sep=""))
MCapData <-  read.csv(paste(wd, "/",MCapFile[1,1], sep=""))

PricedfTemp <-  as.data.frame(PriceData)
VolumedfTemp <-  as.data.frame(VolumeData)
MCapdfTemp <-  as.data.frame(MCapData)

Pricedf <-  remove_empty(PricedfTemp, "cols")
Volumedf <-  remove_empty(VolumedfTemp, "cols")
MCapdf <-  remove_empty(MCapdfTemp, "cols")

rm(PriceFile, PriceData, PricedfTemp, VolumeFile, VolumeData, VolumedfTemp, MCapFile, MCapData, MCapdfTemp, idP, idV, idM, wd)