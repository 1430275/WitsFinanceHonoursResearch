rm(list = ls())

library(googledrive)
library(readr)
library(janitor)


tempdirectory <- tempdir()

tempP <- tempfile(tmpdir = tempdirectory, fileext = ".zip")
dlP <- drive_download(as_id("1FWCX68QO9VUT5HVPP2rMuA5GI5XH2JVl"), 
                      path = tempP, 
                      overwrite = TRUE)
outP <- unzip(tempP, exdir = tempdirectory)
PriceData <- read.csv(outP[1], sep=",")

tempV <- tempfile(tmpdir = tempdirectory, fileext = ".zip")
dlV <- drive_download(as_id("1tfeAcTXbJT-Md-Wsae0LfLJJ8M7k7uw5"),
                      path = tempV,
                      overwrite = TRUE)
outV <- unzip(tempV, exdir = tempdirectory)
VolumeData <- read.csv(outV[1], sep=",")

tempM <- tempfile(tmpdir = tempdirectory, fileext = ".zip")
dlM <- drive_download(as_id("1SxQtzVoAN8HczZLI8LZzsWDYmYDItdl8"), 
                      path = tempM,
                      overwrite = TRUE)
outM <- unzip(tempM, exdir = tempdirectory )
MCapData<- read.csv(outM[1], sep=",")

PricedfTemp <-  as.data.frame(PriceData)
VolumedfTemp <-  as.data.frame(VolumeData)
MCapdfTemp <-  as.data.frame(MCapData)

Pricedf <-  remove_empty(PricedfTemp, "cols")
Volumedf <-  remove_empty(VolumedfTemp, "cols")
MCapdf <-  remove_empty(MCapdfTemp, "cols")