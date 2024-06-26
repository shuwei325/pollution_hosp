library(tidyverse)
library(sf)
library(geodata)
library(readxl)

# Function to remove accents from strings and convert to lowercase
remove_accents_and_lowercase <- function(input_string) {
  # Convert string to ASCII encoding to remove accents
  cleaned_string <- iconv(input_string, to = "ASCII//TRANSLIT")
  
  # Remove any remaining special characters (optional step)
  cleaned_string <- str_remove_all(cleaned_string, "[^[:alnum:]]")
  
  # Convert the cleaned string to lowercase
  cleaned_string <- tolower(cleaned_string)
  
  # Return the cleaned string
  return(cleaned_string)
}


CRC <- gadm(country = "Costa Rica",level = 3,path = ".",)
CRC_sC <- CRC[CRC$NAME_3!="Isla del Coco"]
CRC_sf <- st_as_sf(CRC_sC)
CRC_sf <- CRC_sf %>% mutate(distrito = NAME_3,canton=NAME_2) %>%
  mutate(distrito = remove_accents_and_lowercase(distrito),
         canton=remove_accents_and_lowercase(canton))


# Carga datos de areas de salud
tabla_areas <- read_xlsx("Data/Lista_distritos_area_resumen2.xlsx")
tabla_areas <- tabla_areas %>% filter(cod_distrito != "2036") %>%
  mutate(distrito = remove_accents_and_lowercase(distrito),
         canton=remove_accents_and_lowercase(canton))

CRC_sf_areas <- CRC_sf %>% left_join(tabla_areas,by = c("distrito","canton")) 

union_sf <- CRC_sf_areas %>%
  group_by(Región) %>%
  summarize(geometry = st_union(geometry))

ggplot() +
  geom_sf(data = union_sf, aes(fill = Región)) +
  theme_minimal()+
  labs(fill = 'Region')


centroides_sf <- union_sf %>% st_centroid() 

save(union_sf,centroides_sf,file = "./Data/regiones_geom.RData")
