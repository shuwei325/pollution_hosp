library(stringr)
library(ncdf4)
library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

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

data <- data.frame(fechas,egresos_ts)
data %>% ggplot( aes(x=fechas, y=egresos_ts)) +
  geom_line() +
  geom_point()

# Excluir los dos cantones nuevos.
#Rio cuarto de Grecia, cod_canton==203
#Monte verde de Puntarenas, cod_canton==601


list_distritos <- read_xlsx("./Data/Lista_distritos.xlsx")
all.equal(distritos[-c(1,2)],list_distritos$distrito)

colnames(CR_datos)[-c(1,2)]<- list_distritos$cod_distrito

egresos_canton_dia <- CR_datos  %>%
  pivot_longer(cols = 'Desconocido':'7065', 
               names_to = "cod_distrito", 
               values_to = "egreso")%>% 
  left_join(list_distritos, by = "cod_distrito") %>% 
  group_by(Fecha,canton,cod_canton) %>% 
  summarise(egreso_canton=sum(egreso)) %>% ungroup()

egresos_canton_dia <- egresos_canton_dia %>% mutate(year=epiyear(Fecha),
                                                    epi.week=epiweek(Fecha),
                                                    month= month(Fecha)) 

save(egresos_canton_dia,file = "./Data/egresos_canton_dia.Rdata")


