library(chirps)
library(sf)
library(raster)
library(ggplot2)
library(tidyverse)
library(lubridate)

aut <- getData('GADM', country = 'CR', level = 1)
aut <- st_as_sf(aut)
ggplot() + 
  geom_sf(data = aut)

grid <- aut %>% 
  st_make_grid(cellsize = 0.1, what = "centers") %>% # grid of points
  st_intersection(aut)  

ubicaciones <- unlist(grid)
lon_CR <- ubicaciones[ubicaciones<0]
lat_CR <- ubicaciones[ubicaciones>0]
lonlat <- data.frame(lon = lon_CR,
                     lat = lat_CR)

dates <- c("1981-01-02", "2022-02-28")
save(dates,lonlat,file = 'localizacionesCHIRPS.RData')

dat <- get_chirps(lonlat, dates, server = "CHC")

save(dat,file='datosPrecCHIRPS.RData')