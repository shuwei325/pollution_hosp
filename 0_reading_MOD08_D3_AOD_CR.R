#https://rspatial.github.io/terra/reference/terra-package.html
#https://cran.r-project.org/web/packages/terra/terra.pdf


#library(ncdf4)
library(fields)
library(maps)
library(sp)
library(mapproj)
library(RNetCDF)
library(ggmap)
library(reshape)
library(dplyr)
library(stringr)
library(rlist)
library(terra)
library(lubridate)

dirbase <- "./Data/MOD08_D3/2001/"
year <- 2001
listfilesg <- list.files(path = paste0(dirbase),pattern = paste0("*.hdf"), recursive = TRUE)

lon_lim <- c(-87, -82)
lat_lim <- c(8, 11) 
CR_ext <- ext(-87, -82, 8, 11 )

data_list <- list()

for (i in 1:length(listfilesg)) {
  show(paste0('Construccion datos - ', i))
  data_nc <- sds(paste0(dirbase, listfilesg[i]))
  
  var<-data_nc$Aerosol_Optical_Depth_Land_Ocean_Mean
  var_CR <- crop(var, CR_ext)
  var_array<- as.array(var_CR)[,,1]
  
  #year <- 2001 #str_sub(listfilesg[i], 1, 4)
  day <- str_sub(listfilesg[i], 1, 3)
  
  lon <- xFromCol(var_CR)
  lat <- yFromRow(var_CR)
  
  rm(data_nc,var)
  
  dimnames(var_array) <- list(lat, lon)
  var_prep <- melt(var_array)
  colnames(var_prep) <- c('lon', 'lat', "aerosol")
  
  # data<-var_prep %>% dplyr::filter( between(lon ,lon_lim[1],lon_lim[2]) &
  #                                     between(lat ,lat_lim[1],lat_lim[2]))
  data <- var_prep %>% mutate(year = as.numeric(year) , day = as.numeric(day))
  
  data_list[[i]] <- data
}  

datos_aerosol <- list.rbind(data_list)

scale <- 0.001

datos_aerosol$aerosol <- datos_aerosol$aerosol *scale
datos_aerosol <- datos_aerosol %>% mutate(date= parse_date_time(x = paste(year, day),orders="yj"))

save(datos_aerosol,file= paste0("./Data/aerosol_MOD08_D3_",year,".RData"))




