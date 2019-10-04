# se crean datos para revisiar la muestra y para estimar
library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(srvyr)

source("R/funciones.R")

### entrega BITS
bits_2018 <- read_sf("datos_entrada/datos_bits/PUNTOS DE VALIDACION-2018/Puntos_de_Validacion_2018_Revisados.shp")
bits_2018_points <- bits_2018 %>% st_cast("POINT")

# detectamos puntos repetidos
bits_2018_points$equals <- st_equals(bits_2018_points) %>% map_int(first) 

# seleccionamos únicamente un punto cuando hay repetidos
bits_2018_unique <- bits_2018_points %>% 
    group_by(equals) %>% 
    top_n(n = 1, identifier) %>% 
    ungroup() 

# agregamos variable de mapa MADMEX para comparar y determinar coincidencia
# última versión MADMEX 2018:
raster_31 <- raster("datos_entrada/madmex_sentinel2_2018_31.tif")

# Reproyectamos raster y extraemos la variable para agregarla a sf de BITS
# agregamos también columnas para el valor de intérpretes en 17 clases 
# (solo estaba en 31 clases).
crs_lcc <- crs(raster_31) %>% as.character()
bits_2018_lcc <- st_transform(bits_2018_unique, crs = crs_lcc) 
bits_raster <- raster::extract(raster_31, y = bits_2018_lcc)

bits_2018_lcc <- bits_2018_lcc %>% 
    add_column(raster = bits_raster) %>% 
    mutate_at(vars(raster, interp1, interp2, interp3), 
        .funs = list(c31 = identity, c17 = clasifica_31_17)) %>% 
    select(-raster, -interp1, -interp2, -interp3)

# determinamos coincidencia con la primera etiqueta de los revisores (top) y 
# con alguna de las 3 asignadas
bits_2018_lcc_row <- bits_2018_lcc %>% 
    rowwise() %>% 
    mutate(
        correcto_31_top = raster_c31 == interp1_c31,
        correcto_31_top3 = raster_c31 %in% c(interp1_c31, interp2_c31, 
            interp3_c31), 
        correcto_17_top = raster_c17 == interp1_c17,
        correcto_17_top3 = raster_c17 %in% c(interp1_c17, interp2_c17, 
            interp3_c17)
    ) %>% 
    ungroup() %>% 
    select(OBJECTID, identifier, correcto_31_top:correcto_17_top3)

bits_2018_lcc <- bits_2018_lcc %>% 
    left_join(bits_2018_lcc_row)

write_rds(bits_2018_lcc, "datos_salida/bits_2018_lcc.rdata")

# para la estimación se requiere la información del marco y diseño
# leemos tamaños de diseño de muestreo
marco <- read_csv("datos_salida/tamanos_2.csv") %>% 
    dplyr::select(-p, -n_0) %>% 
    rename(classid = clase, n_planned = n)

# leemos muestra entregada a BITS y revisamos repetidos
muestra_pais <- read_rds(path = "datos_salida/muestra_pais.rds")
muestra_pais <- muestra_pais %>% 
    mutate(id_muestra = 1:n())
muestra_pais$equals <- st_equals(muestra_pais) %>% 
    map_int(first) 

# puntos con repetición (195)
muestra_eq <- muestra_pais %>% 
    group_by(equals) %>% 
    mutate(n = n()) %>% 
    filter(n > 1)
st_write(muestra_eq, "datos_salida/muestra_pais_reps.shp")

# sin repetición
muestra_pais_unique <- muestra_pais %>% 
    group_by(equals) %>% 
    top_n(n = 1, id_muestra) %>% 
    ungroup() %>% 
    dplyr::select(-equals)

# unimos puntos únicos con puntos BITS para tener variables de estratificación:
# edo y classid de datos originales

bits_2018_edo <- select(bits_2018_lcc, -equals) %>% 
    st_join(muestra_pais_unique, join = st_is_within_distance, 
        dist = 0.02, left = TRUE)

# para los ponderadores necesitamos saber el tamaño por estrato
bits_2018_w <- bits_2018_edo %>% 
    left_join(marco, by = c("classid", "edo")) %>% 
    group_by(classid, edo) %>% 
    mutate(n_obs = n()) %>% 
    ungroup() %>% 
    mutate(estrato = paste0(classid, "-", edo))

# revisamos planeados vs observados
bits_2018_w %>% 
    st_drop_geometry() %>% 
    dplyr::select(classid, edo, n_planned, n_obs) %>% 
    dplyr::distinct() %>% 
    mutate(diff = n_planned - n_obs) %>% 
    filter(diff > 0) %>% 
    arrange(-diff)

glimpse(bits_2018_w)

write_rds(bits_2018_w, path = "datos_salida/bits_2018_weights.rdata")

# creamos datos con diseño para estimación con survey y srvyr
library(srvyr)

bits_design <- bits_2018_w %>% 
    as_survey_design(ids = identifier, strata = estrato, fpc = N)
write_rds(bits_design, path = "datos_salida/bits_2018_design.rdata")


# entrega Pedro
# dos etiquetas: etiqueta pixel y etiqueta hectárea, resultan de evaluar 
# únicamente el pixel seleccionado y de evaluar una hectárea alrededor
 
pedro <- read_sf("datos_salida/muestras_pedro_etiquetada/muestra300_etiq_pedro.shp")

bits_2018_lcc_df <- st_drop_geometry(bits_2018_lcc)

# agregamos clasificación 17 clases y etiquetas de BITS, determinamos 
# coincidencias
bits_pedro <- pedro %>% 
    st_drop_geometry() %>% 
    mutate(
        pedro_c17 = clasifica_31_17(pedro31cl),
        pedro1ha_c17 = clasifica_31_17(pedro1ha), 
        pedro_c31 = pedro31cl,
        pedro1ha_c31 = pedro1ha
    ) %>% 
    dplyr::select(-interp1, -interp2, -interp3, -pedro31cl, 
        -pedro1ha) %>% 
    left_join(bits_2018_lcc_df, by = "identifier") %>% 
    rowwise() %>% 
    mutate(
        p_correcto_31_pix = (pedro_c31 == raster_c31),
        p_correcto_31_ha = (pedro1ha_c31 == raster_c31),
        p_correcto_31 = p_correcto_31_pix | p_correcto_31_ha,
        
        p_bits_correcto_31_pix = pedro_c31 %in% 
            c(interp1_c31, interp2_c31, interp3_c31),
        p_bits_correcto_31_1ha = pedro1ha_c31 %in% 
            c(interp1_c31, interp2_c31, interp3_c31),
        p_bits_correcto_31 = p_bits_correcto_31_pix | p_bits_correcto_31_1ha,
        
        p_correcto_17_pix = (pedro_c17 == raster_c17),
        p_correcto_17_ha = (pedro1ha_c17 == raster_c17),
        p_correcto_17 = p_correcto_17_pix | p_correcto_17_ha,
        
        p_bits_correcto_17_pix = pedro_c17 %in% c(interp1_c17, interp2_c17, 
            interp3_c17), 
        p_bits_correcto_17_1ha = pedro1ha_c17 %in% c(interp1_c17, interp2_c17, 
            interp3_c17), 
        p_bits_correcto_17 = p_bits_correcto_17_pix | p_bits_correcto_17_1ha
    ) %>% 
    ungroup()

write_rds(bits_pedro, path = "datos_salida/bits_pedro.rds")

# usando diseño de muestreo (pob. es muestra BITS)
# datos con los que se creó la muestra Pedro (no finales)
bits_2018_sample <- read_sf("datos_entrada/datos_bits/Validacion_Final_Mapa-2018_BITS_190211/validacion_final_2018.shp")
bits_2018_sample_n <- bits_2018_sample %>% 
    st_drop_geometry() %>% 
    group_by(raster_m18) %>% 
    summarise(N = n())

bits_pedro_w <- bits_pedro %>% 
    left_join(bits_2018_sample_n, by = "raster_m18") 
bits_pedro_design <- bits_pedro_w %>% 
    as_survey_design(ids = identifier, strata = raster_m18, fpc = N)
write_rds(bits_pedro_design, path = "datos_salida/bits_pedro_design.rds")
