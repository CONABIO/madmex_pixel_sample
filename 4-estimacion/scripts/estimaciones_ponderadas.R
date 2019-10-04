# genera csv's con estimaciones que se almacenan en datos_salida
# estimaciones a total y por clase, a nivel nacional y por estado

library(srvyr)
library(tidyverse)
library(sf)
source("R/funciones.R")

### Porcentajes crudos
bits_2018_lcc <- read_rds("datos_salida/bits_2018_lcc.rdata")
bits_2018_lcc_df <- bits_2018_lcc %>% st_drop_geometry()

## A total
# Nacional
tab_crudos <- bits_2018_lcc_df %>% 
    summarise(
        porcent_31_top = imprime_porcent(correcto_31_top), 
        porcent_31_top3 = imprime_porcent(correcto_31_top3), 
        porcent_17_top = imprime_porcent(correcto_17_top),
        porcent_17_top3 = imprime_porcent(correcto_17_top3)
    ) %>% 
    gather(clasif, porcentaje) %>% 
    mutate(clasif = str_remove(clasif, "porcent_")) %>% 
    separate(clasif, into = c("n_clases", "tipo")) %>% 
    spread(n_clases, porcentaje)
usethis::use_directory("datos_salida/porcentajes_crudos")
write_csv(tab_crudos, "datos_salida/porcentajes_crudos/porcentajes_nacional_17_32_top_top3.csv")

### Por clase
# Nacional
prop_clase_pais_17_top <- prop_clase_crudo(bits_2018_lcc_df,
    var_clase_madmex = raster_c17, var_clase_interp = interp1_c17, 
    var_y = correcto_17_top) 

prop_clase_pais_17_top3 <- prop_clase_crudo(bits_2018_lcc_df,
    var_clase_madmex = raster_c17, var_y = correcto_17_top3, top = FALSE, 
    n_clases = 17)
write_csv(prop_clase_pais_17_top, "datos_salida/porcentajes_crudos/porcentajes_clase_Nacional_17_top.csv")
write_csv(prop_clase_pais_17_top3, "datos_salida/porcentajes_crudos/porcentajes_clase_Nacional_17_top3.csv")



### Estimaciones ponderadas

bits_2018_w <- read_rds(path = "datos_salida/bits_2018_weights.rdata")
bits_design <- bits_2018_w %>% 
    as_survey_design(ids = identifier, strata = estrato, fpc = N)

## A total
# Nacional
est_pais_17_top3 <- estima_total(bits_design, correcto_17_top3)
est_pais_17_top <- estima_total(bits_design, correcto_17_top)

est_pais_31_top3 <- estima_total(bits_design, correcto_31_top3)
est_pais_31_top <- estima_total(bits_design, correcto_31_top)

usethis::use_directory("datos_salida/estimaciones")
write_csv(est_pais_17_top3, "datos_salida/estimaciones/estimaciones_nacional_17_top3.csv")
write_csv(est_pais_17_top, "datos_salida/estimaciones/estimaciones_nacional_17_top.csv")

write_csv(est_pais_31_top3, "datos_salida/estimaciones/estimaciones_nacional_31_top3.csv")
write_csv(est_pais_31_top, "datos_salida/estimaciones/estimaciones_nacional_31_top.csv")

# Por estado 17 clases
est_edo_17_top3 <- estima_total(bits_design, correcto_17_top3, 
    var_group = edo)
est_edo_17_top <- estima_total(bits_design, correcto_17_top, 
    var_group = edo)

# Por estado 31 clases
est_edo_31_top3 <- estima_total(bits_design, correcto_31_top3, 
    var_group = edo)
est_edo_31_top <- estima_total(bits_design, correcto_31_top, 
    var_group = edo)

write_csv(est_edo_17_top3, "datos_salida/estimaciones/estimaciones_edo_17_top3.csv")
write_csv(est_edo_17_top, "datos_salida/estimaciones/estimaciones_edo_17_top.csv")

write_csv(est_edo_31_top3, "datos_salida/estimaciones/estimaciones_edo_31_top3.csv")
write_csv(est_edo_31_top, "datos_salida/estimaciones/estimaciones_edo_31_top.csv")

### Por clase
# Nacional
est_clase_pais_17_top <- estima_clase(bits_design, 
    var_clase_madmex = raster_c17, var_clase_interp = interp1_c17, 
    var_y = correcto_17_top)
est_clase_pais_17_top3 <- estima_clase(bits_design, 
    var_clase_madmex = raster_c17, var_y = correcto_17_top3, top = FALSE)

write_csv(est_clase_pais_17_top, "datos_salida/estimaciones/estimaciones_clase_Nacional_17_top.csv")
write_csv(est_clase_pais_17_top3, "datos_salida/estimaciones/estimaciones_clase_Nacional_17_top3.csv")

est_clase_pais_31_top <- estima_clase(bits_design, 
    var_clase_madmex = raster_c31, var_clase_interp = interp1_c31, 
    var_y = correcto_31_top, n_clases = 31)
est_clase_pais_31_top3 <- estima_clase(bits_design, 
    var_clase_madmex = raster_c31,  
    var_y = correcto_31_top3, top = FALSE, n_clases = 31)

write_csv(est_clase_pais_31_top, "datos_salida/estimaciones/estimaciones_clase_Nacional_31_top.csv")
write_csv(est_clase_pais_31_top3, "datos_salida/estimaciones/estimaciones_clase_Nacional_31_top3.csv")

# Por estado
edos <- unique(bits_2018_w$edo)
est_clase_edos_17_top <- map(set_names(edos), ~ estima_clase(bits_design, 
    var_clase_madmex = raster_c17, var_y = correcto_17_top, 
    var_filter = edo, var_clase_interp = interp1_c17,
    value_filter = ., top = TRUE))

usethis::use_directory("datos_salida/estimaciones/estimaciones_clase_edo_top")

walk2(est_clase_edos_17_top, names(est_clase_edos_17_top), ~write_csv(.x, 
    path = paste0("datos_salida/estimaciones/estimaciones_clase_edo_top/estimaciones_clase_", 
        .y, "_17_top.csv")))
est_clase_edos_17_top3 <- map(set_names(edos), ~ estima_clase(bits_design, 
    var_clase_madmex = raster_c17, var_y = correcto_17_top3, 
    var_filter = edo, var_clase_interp = interp1_c17,
    value_filter = ., top = TRUE))

usethis::use_directory("datos_salida/estimaciones/estimaciones_clase_edo_top3")
walk2(est_clase_edos_17_top3, names(est_clase_edos_17_top3), ~write_csv(.x, 
    path = paste0("datos_salida/estimaciones/estimaciones_clase_edo_top3/estimaciones_clase_", 
        .y, "_17_top3.csv")))

