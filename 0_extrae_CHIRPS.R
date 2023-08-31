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

dat_max <- get_chirps(lonlat, dates, server = "CHC",operation=0)
dat_min <- get_chirps(lonlat, dates, server = "CHC",operation=1)
dat_median <- get_chirps(lonlat, dates, server = "CHC",operation=2)
dat_sum <- get_chirps(lonlat, dates, server = "CHC",operation=4)
dat_average <- get_chirps(lonlat, dates, server = "CHC",operation=5)

head(dat_max)
dat_max <- dat_max %>% rename(prep_max=chirps)
dat_min <- dat_min %>% rename(prep_min=chirps)
dat_median <- dat_median %>% rename(prep_median=chirps)
dat_sum <- dat_sum %>% rename(prep_sum=chirps)
dat_average <- dat_average %>% rename(prep_average=chirps)
  
datos <- dat_max %>% left_join(dat_min, by=c("id","lon","lat","date"))  %>% 
                      left_join(dat_min, by=c("id","lon","lat","date"))  %>% 
                      left_join(dat_median, by=c("id","lon","lat","date"))  %>% 
                      left_join(dat_sum, by=c("id","lon","lat","date"))  %>% 
                      left_join(dat_average, by=c("id","lon","lat","date"))
head(datos)


save(datos,file='.Data/datosPrecCHIRPS.RData')
