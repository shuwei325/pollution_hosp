library(lubridate)
library(imputeTS)
library(ggplot2)
library(dplyr)
library(readxl)

MOD08_D3
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



#datos faltantes
colSums(is.na(aerosol))/dim(aerosol)[1]

table(aerosol$lon,aerosol$lat)

# 1 localizacion

test1 <- aerosol %>% dplyr::filter(lon == -84.5 & lat == 9.5)

table(test1$year,test1$day)
table(test1$year)

ggplot(test1, aes(x = date, y = aerosol)) + 
  geom_line() +
  theme_minimal()

test1 <- test1 %>% filter(year >=2017)

# 2. Resumen de datos

aerosol_sum <- aerosol %>% group_by(year,day,date) %>% 
  summarise(aerosol_mean=mean(aerosol,na.rm=TRUE)) %>% ungroup()

ggplot(aerosol_sum, aes(x = date, y = aerosol_mean)) + 
  geom_line() +
  theme_minimal()


aerosol_sum <- aerosol_sum %>% filter(year >=2017)


# 3. Imputation
aerosol_sum <- aerosol_sum %>%
  mutate(
    aerosol_impute = na_interpolation(aerosol_mean, option = "linear")
  ) %>% ungroup()

ggplot(aerosol_sum, aes(x = date, y = aerosol_impute)) + 
  geom_line() +
  theme_minimal()

acf(aerosol_sum$aerosol_mean,lag.max=500, na.action=na.pass)

range(aerosol_sum$date)



# Extraccion de casos observados en Costa Rica ----
PI01 <-
  read_xlsx('./Data/Base de Datos_PM_2017-21.xlsx',
            sheet = "PI-024",
            range = "A1:C419",
            col_names = T,
            .name_repair = "minimal")

names(PI01)<-c("t","fecha","PM10")
str(PI01)
dim(PI01)

datos_join <- aerosol_sum %>% left_join(PI01, by=c("date"="fecha")) %>% 
                        select(year,date,aerosol_mean,aerosol_impute,PM10)

datos_join
colSums(is.na(datos_join))


ggplot(datos_join, aes(x=aerosol_mean, y=PM10)) +
  geom_point(size=2, shape=23)

with(datos_join, cor(aerosol_mean,PM10,use="pairwise.complete.obs"))



# map ---------------------------------------------------------------------

#Costa Rica
aerosol %>% filter(year==2001 & day== 1) %>% ggplot() +             # plot points
  geom_point(aes(x = lon,y = lat,       # lon and lat
                 colour = aerosol),           # attribute color
             size = 2.5)+                # make all points larger
  #col_scale(name = "degF") +
  xlab("Longitude (deg)") +             # x-axis label
  ylab("Latitude (deg)") +              # y-axis label
  geom_path(data = ggplot2::map_data("world"),   # add map
            aes(x = long, y = lat, group = group), 
            col=1) +
   facet_wrap(~day, nrow = 4) +        # facet by time
  coord_fixed(xlim = c(-86, -82),
              ylim = c(8, 11))  +      # zoom in
  theme_bw()     
