library(tidyverse)
library(sf)

library(terra)
library(luna)
library(geodata)
modis <- getProducts("^MOD04")
product <- "MOD04_3K"

start <- "2019-12-15"
end <- "2019-12-31"
CRC <- gadm(country = "Costa Rica",level = 3,path = ".",)
CRC_sC <- CRC[CRC$NAME_3!="Isla del Coco"]

mf <- getModis(product, start, end, aoi=CRC_sC, download = FALSE,version = "6",
               server = "LAADS")

mf2 <- getModis(product, start, end, aoi=UU, download = FALSE,version = "6",
               server = "LAADS")

dir_desc <- "./Data/MOD04_3K"

mf <- getModis(product, start, end, aoi=CRC_sC, download = TRUE,version = "6",
               server = "LAADS",path = dir_desc,username = "luis_barboza",
               password = "rdSURNd7ggJQz6*")

# https://rspatial.org/modis/3-explore.html

r <- rast(mf[1])
r <- rast(x = './Data/descarga_pba/MOD04_L2.A2023001.1525.061.2023002020829.hdf')
