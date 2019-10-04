# se crea una muestra para evaluar el trabajo de BITS, esta muestra será 
# revisada por Pedro 
# Entrada: primera entrega de BITS (incompleta) y mapa con muestra entregada
# Salida: muestra de tamaño 300 estratificada por clase

library(sf)
library(raster)
library(dplyr)
library(tidyverse)

bits_2018 <- read_sf("datos_entrada/datos_bits/Validacion_Final_Mapa-2018_BITS_190211/validacion_final_2018.shp")

# muestra tamaño 300
# seleccionamos muestra a partir de entrega de BITS, estratificando por clase
set.seed(8718)
muestra_300 <- bits_2018 %>% 
    group_by(raster_m18) %>% 
    sample_n(10, replace = FALSE)
st_write(muestra_300, "datos_salida/muestras_pedro/muestra_300.shp")

# generamos también una muestra de tamaño 700 en caso de que sobre tiempo
set.seed(92718)
muestra_700 <- bits_2018 %>% 
    filter(!(identifier %in% muestra_300$identifier)) %>% 
    group_by(raster_m18) %>% 
    sample_frac(0.03)
st_write(muestra_700, "datos_salida/muestras_pedro/muestra_700.shp")
