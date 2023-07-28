#https://lpdaac.usgs.gov/products/mcd19a2v006/

# Download CR data -----------------------------------------------------------------

library(RGISTools)

CR_geo<-extent(c(-86,-82,7.96,11.2))

sres <- modSearch(product = "MCD19A2", 
                  startDate = as.Date("01-01-2015", "%d-%m-%Y"), 
                  endDate = as.Date("10-01-2015", "%d-%m-%Y"), 
                  collection = 6, extent = CR_geo)


# download the first image in sres 
wdir <- file.path("./Data/") 
print(wdir)                                   

wdir.mod <- file.path(wdir,"Modis","MCD19A2") 

modDownload(sres, 
            username = "shuwei.ucr", 
            password = "shuUCRwei20", 
            AppRoot = wdir.mod)



# Test --------------------------------------------------------------------

dirbase <- "./Data/Modis/MCD19A2/hdf/"
file <- "MCD19A2.A2015001.h09v07.006.2018101205839.hdf"
data_nc <- ncdf4::nc_open(paste0(dirbase, file))

print(data_nc)
{
  sink('MCD19A2_metadata.txt')
  print(data_nc)
  sink()
}

names(data_nc)

#User's guide:
#https://lpdaac.usgs.gov/documents/110/MCD19_User_Guide_V6.pdf

#metadata:
#https://atmosphere-imager.gsfc.nasa.gov/sites/default/files/ModAtmo/documents/MOD08_E3_CDL_fs.txt
VARNAME = "Optical_Depth_047"
VARNAME = "Optical_Depth_055"
var_array <- ncdf4::ncvar_get(data_nc, VARNAME)
dim(var_array) 

#nc_close(data_nc) 
var_array<-var_array[,,2] #Aquí no encontré la documentación de las 5 dimensiones.
lon <- 1:1200
lat <- 1:1200

sum(is.na(var_array))/(1200^2)

dimnames(var_array) <- list(lon, lat)

var_prep <- melt(var_array)
colnames(var_prep) <- c('lon', 'lat', "AOD")

ggplot(var_prep) +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = AOD),           # attribute color
             size = 2.5)+                # make all points larger
  #col_scale(name = "degF") +
  #xlab("Longitude (deg)") +             # x-axis label
  #ylab("Latitude (deg)") +              # y-axis label
  theme_bw()     

test<-var_prep[1:50000,]
test %>% ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = AOD),           # attribute color
             size = 2.5)+                # make all points larger
  #col_scale(name = "degF") +
  #xlab("Longitude (deg)") +             # x-axis label
  #ylab("Latitude (deg)") +              # y-axis label
  theme_bw()     
