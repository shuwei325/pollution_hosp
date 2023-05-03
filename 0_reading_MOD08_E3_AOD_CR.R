library(ncdf4)
library(fields)
library(maps)
library(sp)
library(mapproj)
library(RNetCDF)
library(ggmap)
library(akima)
library(reshape)
library(dplyr)
library("stringr")
library(rlist)

dirbase <- "./Data/Modis/MOD08_E3/"
listfilesg <- list.files(path = dirbase,pattern = paste0("*.hdf"), recursive = TRUE)

VARNAME = "Aerosol_Optical_Depth_Land_Ocean_Mean_Mean"

lon_lim <- c(-87, -82)
lat_lim <- c(8, 11) 
data_list <- list()

for (i in 20:length(listfilesg)) {
  show(paste0('Construccion datos - ', i))
  data_nc <- ncdf4::nc_open(paste0(dirbase, listfilesg[i]))
  var_array <- ncdf4::ncvar_get(data_nc, VARNAME)
  
  year <- str_sub(listfilesg[i], 1, 4)
  day <- str_sub(listfilesg[i], 6, 8)
  #dim(var_array) 
  
  lon <- ncdf4::ncvar_get(data_nc, "XDim")
  lat <- ncdf4::ncvar_get(data_nc, "YDim")
  # range(lon)
  # range(lat)
  
  nc_close(data_nc) 
  
  dimnames(var_array) <- list(lon, lat)
  var_prep <- melt(var_array)
  colnames(var_prep) <- c('lon', 'lat', "aerosol")
  
  data<-var_prep %>% dplyr::filter( between(lon ,lon_lim[1],lon_lim[2]) &
                                      between(lat ,lat_lim[1],lat_lim[2]))
  data <- data %>% mutate(year = as.numeric(year) , day = as.numeric(day))
  
  data_list[[i]] <- data
}  

datos_aerosol <- list.rbind(data_list)

scale <- 0.00100000004749745

datos_aerosol$aerosol <- datos_aerosol$aerosol *scale
datos_aerosol <- datos_aerosol %>% mutate(date= parse_date_time(x = paste(year, day),orders="yj"))

save(datos_aerosol,file= "./Data/aerosol_MOD08_E3.RData")




