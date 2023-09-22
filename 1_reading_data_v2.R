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


list_distritos <- read_xlsx("./Data/Lista_distritos_area_resumen2.xlsx",
                                 range = "A1:J487")

all.equal(distritos[-c(1,2)],list_distritos$distrito)

colnames(CR_datos)[-c(1,2)]<- list_distritos$cod_distrito

# egresos_region_dia <- CR_datos  %>%
#   pivot_longer(cols = 'Desconocido':'7065', 
#                names_to = "cod_distrito", 
#                values_to = "egreso")%>% 
#   left_join(list_distritos, by = "cod_distrito") %>% 
#   group_by(Fecha,Región) %>% 
#   summarise(egreso_region=sum(egreso)) %>% ungroup() %>% na.omit()

egresos_region_dia <- CR_datos  %>%
  pivot_longer(cols = 'Desconocido':'7062',
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
load(file = "./Data/regiones_geom.RData")


head(dat)
range(dat$date)

unique(dat$date)


datos_space <- dat %>% filter(date==dat$date[1]) %>% select(lon,lat)

puntos <- st_as_sf(datos_space,remove=FALSE, coords =c("lon","lat"), 
                   crs = 4326, agr = "constant")


puntos_sf_joined <- st_join(puntos, union_sf)

datos_space_joined <- as.data.frame(puntos_sf_joined) %>% select(lon, lat, Región)

dat1 <- dat %>% 
  left_join(datos_space_joined, by=c("lon","lat")) %>% 
  na.omit()

datos_CHIRPS <- dat1 %>% 
            mutate(year = year(date),
                   week = epiweek(date)) %>%
            group_by(year,week,Región) %>% summarise(precip = sum(chirps)) %>%
  ungroup()
datos_CHIRPS <- datos_CHIRPS %>%  mutate(date=aweek::get_date(week=week,year=year))


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

points_aerosol <- st_as_sf(data.frame(unique(aerosol[,c(2,1)])),
                           coords = c("lon","lat"),crs=4326)

distancias <- st_distance(centroides_sf,points_aerosol)

coordenadas_puntos <- data.frame(st_coordinates(points_aerosol)) %>% 
  mutate(codigo_grilla = paste(X,Y,sep = ',')) 

distancias <- data.frame(distancias)
colnames(distancias) <- coordenadas_puntos$codigo_grilla
rownames(distancias) <- centroides_sf$Región
distancias <- data.frame(t(distancias)) %>% mutate(codigo_grilla = coordenadas_puntos$codigo_grilla)
colnames(distancias)[-8]<- centroides_sf$Región
distancias_long <-  distancias %>% pivot_longer(cols = Brunca:`Pacífico Central`,names_to = 'Region',
                                                  values_to = 'distancia') 


datos_aerosol <- aerosol %>% 
  mutate(week = epiweek(date)) %>%
  group_by(year,week,lat,lon) %>% summarise(aerosol = mean(aerosol,na.rm=TRUE)) %>%
  ungroup()

# datos_aerosol <- aerosol %>% 
#   mutate(week = epiweek(date)) %>%
#   group_by(year,week) %>% summarise(aerosol = mean(aerosol,na.rm=TRUE)) %>%
#   ungroup()

datos_aerosol <- datos_aerosol %>% mutate(date=aweek::get_date(week=week,year=year))
datos_aerosol <- datos_aerosol %>% mutate(codigo_grilla = paste(lon,lat,sep = ','))



Region <- unique(distancias_long$Region)

cambia_puntos_regiones <- function(bloque){
  bloque_p <- NULL
  for(i in 1:7){
    distancias_long_pba <- distancias_long %>% filter(Region == Region[i])
    bloque_c <- bloque %>% left_join(distancias_long_pba,by = 'codigo_grilla') %>% 
      mutate(distancia2 = aerosol*distancia/sum(distancia,na.rm = T)) %>% 
      dplyr::select(year, week,Region,distancia2) %>% group_by(Region) %>%
      mutate(distancia2 = sum(distancia2,na.rm = T))%>%
      ungroup() %>% distinct()
    bloque_p <- bloque_p %>% bind_rows(bloque_c)
  }
  return(bloque_p)
}

# summarise(distancia2 = sum(distancia2,na.rm = T),.groups = "drop")

fechas_unicas <- datos_aerosol %>% select(year,week) %>% distinct()
bloques_regiones <- NULL
for(i in 1:(dim(fechas_unicas)[1])){
  show(i)
  datos_aerosol_t <- datos_aerosol %>% 
    filter(year == as.numeric(fechas_unicas[i,1]) & 
             week == as.numeric(fechas_unicas[i,2])) 
  bloques_regiones_t <- cambia_puntos_regiones(datos_aerosol_t)
  bloques_regiones <- bloques_regiones %>% bind_rows(bloques_regiones_t)
}

datos_aerosol <- bloques_regiones



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
                            left_join(datos_aerosol, by=c("year"="year",
                                                          "epi.week"="week",
                                                          "Región"="Region")) 




datos_finales <- datos_finales %>% rename(aerosol = distancia2)

save(datos_finales,file="Data/datos_finales2.RData")





