library(lubridate)
library(imputeTS)
library(ggplot2)
library(dplyr)

load(file = "./Data/aerosol_MOD08_L3.RData")

head(datos_aerosol)

datos_aerosol<-datos_aerosol %>% group_by(lon,lat,year) %>% mutate(id = row_number()) %>% ungroup()

#datos faltantes
colSums(is.na(datos_aerosol))/dim(datos_aerosol)[1]

table(datos_aerosol$lon,datos_aerosol$lat)

test1 <- datos_aerosol %>% dplyr::filter(lon == -84.5 & lat == 9.5)

table(test1$year,test1$day)
table(test1$year)

ggplot(test1, aes(x = date, y = aerosol)) + 
  geom_line() +
  theme_minimal()

test1 <- test1 %>% filter(year >=2001)

test1 %>% group_by()

ggplot(test1, aes(x = id, y = aerosol, group = year)) + 
  geom_line() +
  theme_minimal()



#Imputation
# datos_aerosol <- datos_aerosol %>% 
#   group_by(lon,lat) %>%
#   mutate(
#     aerosol_impute = na_seadec(aerosol,algorithm = "interpolation",find_frequency = TRUE)  
#   ) %>% ungroup()




# map ---------------------------------------------------------------------

#Costa Rica
datos_aerosol %>% filter(year==2001 & day<=150) %>% ggplot() +             # plot points
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
