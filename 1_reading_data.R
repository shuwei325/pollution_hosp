library(stringr)
library(ncdf4)
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(sf)

# Extraccion de casos observados en Costa Rica ----
CR_datos <-
  read_xlsx('./Data/CCSS Serie de egresos hospitalarios debido a enfermedades respiratorias por distrito.xlsx',
            range = "A3:RT9134",
            col_names = T,
            .name_repair = "minimal")

head(CR_datos)
egresos_ts <- rowSums(CR_datos[,-1])

egresos_ts_sin_desconocidos <- rowSums(CR_datos[,-c(1,2)])

fechas <- CR_datos$Fecha
distritos <- names(CR_datos)
range(fechas)
fechas_true <- seq(as.Date("1997/1/1"), as.Date("2021/12/31"), by = "day")
length(fechas_true);length(fechas)

data <- data.frame(fechas,egresos_ts)
data %>% ggplot( aes(x=fechas, y=egresos_ts)) +
  geom_line() +
  geom_point()

# Excluir los dos cantones nuevos.
#Rio cuarto de Grecia, cod_canton==203
#Monte verde de Puntarenas, cod_canton==601


list_distritos <- read_xlsx("./Data/Lista_distritos_area_resumen.xlsx",
                                 range = "A1:J487")

all.equal(distritos[-c(1,2)],list_distritos$distrito)

colnames(CR_datos)[-c(1,2)]<- list_distritos$cod_distrito

egresos_region_dia <- CR_datos  %>%
  pivot_longer(cols = 'Desconocido':'7065', 
               names_to = "cod_distrito", 
               values_to = "egreso")%>% 
  left_join(list_distritos, by = "cod_distrito") %>% 
  group_by(Fecha,Región) %>% 
  summarise(egreso_region=sum(egreso)) %>% ungroup() %>% na.omit()

egresos_region_dia <- egresos_region_dia %>% mutate(year=epiyear(Fecha),
                                                    epi.week=epiweek(Fecha),
                                                    month= month(Fecha)) 

egresos_region_semana <- egresos_region_dia %>% 
  group_by(Región, year, epi.week) %>%
  summarise(egreso = sum(egreso_region)) %>% ungroup() %>%
  mutate(date=aweek::get_date(week=epi.week,year=year))

# CHRITS CIGEFI -----------------------------------------------------------

load(file="./Data/CHIRTS_CIGEFI_week_Region.RData")

# Precipitación -----------------------------------------------------------

load(file='./Data/datosPrecCHIRPS.RData')


quantile(datos$precip_max,c(0.5,0.6,0.75,0.9))
#quantile(datos1$precip_average,c(0.75,0.9))
datos_CHIRPS <- datos %>% 
            mutate(year = year(date),
                   week = epiweek(date)) %>%
            group_by(lon,lat,year,week) %>% 
            summarise(precip_max = max(precip_max),
                      precip_min = min(precip_min),
                      precip_mean = mean(precip_average),
                      n_precip_max_Q3= sum(precip_max > 11.95),
                      n_precip_max_P90= sum(precip_max > 23.87),
            ) %>%
            ungroup()

datos_space <- datos_CHIRPS %>% filter(year==datos_CHIRPS$year[1],week==datos_CHIRPS$week[1]) %>% select(lon,lat)

puntos <- st_as_sf(datos_space,remove=FALSE, coords =c("lon","lat"), 
                   crs = 4326, agr = "constant")


puntos_sf_joined <- st_join(puntos, union_sf)

datos_space_joined <- as.data.frame(puntos_sf_joined) %>% select(lon, lat, Región)

datos_CHIRPS <- datos_CHIRPS %>% 
  left_join(datos_space_joined, by=c("lon","lat")) %>% 
  na.omit()

datos_CHIRPS <- datos_CHIRPS %>%  mutate(date=aweek::get_date(week=week,year=year))

datos_CHIRPS <- datos_CHIRPS %>% group_by(year,week,date,Región) %>% 
  summarise(precip_max_max = max(precip_max),
            precip_max_mean = mean(precip_max),
            precip_max_min = min(precip_max),
            precip_mean_mean = mean(precip_mean),
            n_precip_max_Q3 =mean(n_precip_max_Q3),
            n_precip_max_P90 =mean(n_precip_max_P90)            
  ) %>%
  ungroup()


# AOD D3 ---------------------------------------------------------------------


dirbase <- "./Data/MOD08_D3/"
listfilesg <- list.files(path = dirbase,pattern = paste0("*.RData"), recursive = TRUE)

aerosol<-numeric()
for (i in 1:length(listfilesg)) {
  load(file = paste0(dirbase, listfilesg[i]))
  aerosol<- rbind(aerosol,datos_aerosol)
}       

head(aerosol)

#un error en la descarga de datos
names(aerosol)<- c("lat", "lon", "aerosol", "year", "day", "date")

datos_aerosol <- aerosol %>% 
  mutate(week = epiweek(date)) %>%
  group_by(year,week) %>% summarise(aerosol = mean(aerosol,na.rm=TRUE)) %>%
  ungroup()

datos_aerosol <- datos_aerosol %>% mutate(date=aweek::get_date(week=week,year=year))

# Joint data --------------------------------------------------------------

range(datos_CHIRTS$date)
range(egresos_region_semana$date)

datos_finales <- egresos_region_semana %>% filter(year>=2000 & year<=2019) %>%
                            left_join(datos_CHIRTS, by=c("Región"="Región",
                                                         "date"="date",
                                                         "year"="year",
                                                         "epi.week"="week")) %>%
                            left_join(datos_CHIRPS, by=c("Región"="Región",
                                                         "date"="date",
                                                         "year"="year",
                                                         "epi.week"="week"))  %>%
                            left_join(datos_aerosol, by=c("date"="date",
                                                          "year"="year",
                                                          "epi.week"="week")) 




save(datos_finales,file="./Data/datos_finales.RData")





