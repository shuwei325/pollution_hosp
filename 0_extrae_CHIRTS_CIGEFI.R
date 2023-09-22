#https://ropensci.org/blog/2019/11/05/tidync/
library(dplyr)
library(tidync)
library(lubridate)

TMAX <- tidync("./Data/CHIRTS/tmax_estimated_1950-2020.nc")
#year 1950-2020
#time 1:25915
TMAX <- TMAX %>% hyper_tibble() 
head(TMAX)

TMIN <- tidync("./Data/CHIRTS/tmin_estimated_1950-2020.nc")
TMIN <- TMIN %>% hyper_tibble() 
head(TMIN)

datos <- TMAX %>% left_join(TMIN,by=c("longitude","latitude","time"))
rm(TMAX,TMIN)


datos <- datos %>% group_by(longitude,latitude) %>%
  mutate(tt = 1:n()) %>% ungroup()


datos <- datos %>% mutate(year= floor(1950 + (tt-1)/365))
datos <- datos %>% group_by(longitude,latitude,year) %>% 
  mutate(day=1:n()) %>% ungroup()
datos<- datos %>% mutate(date = parse_date_time(x = paste(year, day), orders = "yj") )
datos<- datos %>% mutate(week = epiweek(date),
                         amplitude = Tmax-Tmin)

#quantile(Tmax,0.75)
datos <- datos %>% filter(year>=2000)

quantile(datos$Tmax,0.75)
quantile(datos$Tmin,0.25)
quantile(datos$amplitude,c(0.75,0.9))

datos<- datos %>% group_by(longitude,latitude,year,week) %>%
  summarise(Tmax_max=max(Tmax),
            Tmax_mean=mean(Tmax),
            Tmax_min=min(Tmax),
            n_Tmax_Q3= sum(Tmax > 32.82),
            Tmin_min=min(Tmin),
            Tmin_mean=mean(Tmin),
            Tmin_max=max(Tmin),
            n_Tmin_Q1= sum(Tmin < 22.67),
            amplitude_max=max(amplitude),
            amplitude_mean=mean(amplitude),  
            amplitude_min=min(amplitude),
            n_amplitude_Q3=sum(amplitude > 7.16),
            n_amplitude_P90=sum(amplitude > 8.496))

datos<- datos %>% mutate(date = aweek::get_date(week=week,year=year))

# Resumen por región ------------------------------------------------------


#load(file="./Data/CHIRTS_CIGEFI.RData")
load("0_construye_mapaSalud.R")

datos_space <- datos %>% dplyr::filter(date==datos$date[1]) %>% 
  dplyr::select(longitude,latitude)

puntos <- st_as_sf(datos_space,remove=FALSE, coords =c("longitude","latitude"), 
                   crs = 4326, agr = "constant")

puntos_sf_joined <- 
  st_join(puntos, union_sf) # spatial join to get intersection of points and poly

datos_space_joined <- as.data.frame(puntos_sf_joined) %>% dplyr::select(longitude, latitude, Región)

datos <- datos %>% 
  left_join(datos_space_joined, by=c("longitude","latitude")) %>% 
  na.omit()

datos_CHIRTS <- datos %>% group_by(year,week,date,Región) %>% 
  summarise(Tmax_max = max(Tmax_max),
            Tmax_mean = mean(Tmax_mean),
            Tmax_min = min(Tmax_min),
            n_Tmax_Q3 = mean(n_Tmax_Q3),
            Tmin_min=min(Tmin_min),
            Tmin_mean=mean(Tmin_mean),
            Tmin_max=max(Tmin_max),
            n_Tmin_Q1 = mean(n_Tmin_Q1),
            amplitude_max_max=max(amplitude_max),
            amplitude_max_mean=mean(amplitude_max),  
            amplitude_max_min=min(amplitude_max), 
            amplitude_min_max=max(amplitude_min),
            amplitude_min_mean=mean(amplitude_min),  
            amplitude_min_min=min(amplitude_min), 
            n_amplitude_Q3=mean(n_amplitude_Q3),
            n_amplitude_P90=mean(n_amplitude_P90)            
  ) %>%
  ungroup()

save(datos_CHIRTS,file="./Data/CHIRTS_CIGEFI_week_Region.RData")
