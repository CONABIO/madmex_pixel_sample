library(tidyverse)

bits_2018_w <- read_rds(path = "../../datos_salida/bits_2018_weights.rdata")
edos <- c("Nacional", sort(unique(bits_2018_w$edo)))

leer_tabs_clase <- function(mi_edo) {
    mi_edo_top_file <- list.files("../../datos_salida/estimaciones/estimaciones_clase_edo_top", 
        pattern = paste0(mi_edo, "_17"), full.names = TRUE, ignore.case = TRUE)
    mi_edo_top <- read_csv(mi_edo_top_file)
    mi_edo_top3_file <- list.files("../../datos_salida/estimaciones/estimaciones_clase_edo_top3", 
        pattern = paste0(mi_edo, "_17"), full.names = TRUE, ignore.case = TRUE)
    mi_edo_top3 <- read_csv(mi_edo_top3_file)
    list(top = mi_edo_top, top3 = mi_edo_top3)
}

nal <- leer_tabs_clase("Nacional")

leer_tabs_total <- function(agregacion){
    agregacion_files <- list.files("../../datos_salida/estimaciones", 
        pattern = str_c(agregacion, ".*csv$"), full.names = TRUE, 
        ignore.case = TRUE, include.dirs = FALSE)
    map_dfc(agregacion_files, read_csv) %>% 
        select(ends_with("edo"), contains("correcto")) %>% 
        gather(var, valor, correcto_17_top:correcto_31_top3_se) %>% 
        mutate(
            tipo = ifelse(str_detect(var, "top3"), "Top3", "Top"), 
            tipo_valor = ifelse(str_detect(var, "_se"), "ee", "est"), 
            n_clases = ifelse(str_detect(var, "31"), as.integer(31), 
                as.integer(17))
        ) %>% 
        select(-var) %>% 
        spread(tipo_valor, valor) %>% 
        arrange(n_clases) %>% 
        select(tipo, n_clases, contains("edo"), est, ee)
}    
            

tab_clases_17 <- read_csv("../../datos_salida/tablas_indice/clases_17.csv")
tab_clases_31 <- read_csv("../../datos_salida/tablas_indice/clases_31.csv")