# funciones para el cálculo de porcentajes y estimación
# documentadas con docstring

library(tidyverse)
library(docstring)
library(survey)
library(srvyr)

# tablas con nombres y claves
tab_clases_17 <- tibble(clase = 1:17, descrip = c(
    "Bosques de aciculifolias y escuamifolias", 
    "Bosques latifoliados",
    "Bosque Húmedo de Montaña", 
    "Manglar y petén", 
    "Selvas húmedas", 
    "Selvas secas", 
    "Matorral alto denso", 
    "Matorral mésico",
    "Matorral bajo abierto", 
    "Vegetación acuática menor",
    "Vegetación de suelos arenosos", 
    "Vegetación halófila",
    "Pastizales y otra vegetación herbácea", 
    "Tierras agrícolas",
    "Urbano y Construido", 
    "Suelo desnudo", 
    "Agua"))

tab_clases_31 <- tibble(clase = 1:31, descrip = c(
    "Bosque de Coniferas de Oyamel Ayarin Cedro",
    "Bosque de Coniferas de Pino y Tascate",
    "Bosque de Encino y Bosque de Galeria",
    "Chaparral",
    "Mezquital y Submontano",
    "Selva Baja Perennifolia y Bosque Mesofilo",
    "Selva Baja y Mediana Subperennifolia Galeria y Palmar Natural",
    "Manglar y Peten",
    "Selva Mediana y Alta Perennifolia",
    "Selva Alta Subperennifolia",
    "Selva Baja Caducifolia Subcaducifolia y Matorral Subtropical",
    "Selva Mediana Caducifolia y Subcaducifolia",
    "Mezquital Xerofilo Galeria y Desertico Microfilo",
    "Matorral Crasicaule",
    "Matorral Espinoso Tamaulipeco",
    "Matorral Sarco-Crasicaule",
    "Matorral Sarcocaule",
    "Matorral Sarco-Crasicaule de Neblina",
    "Matorral Rosetofilo Costero",
    "Matorral Desertico Rosetofilo",
    "Popal",
    "Tular",
    "Vegetacion de Dunas Costeras",
    "Vegetacion de Desiertos Arenosos",
    "Vegetacion Halofila Hidrofila",
    "Vegetacion Halofila Xerofila y Gipsofila",
    "Pastizales",
    "Tierras Agricolas",
    "Urbano y Construido",
    "Suelo Desnudo",
    "Agua"))

clasifica_31_17 <- function(x){
    #' Convierte 31 a 17 clases
    #'
    #' Convierte de clasificación en 31 clases Mad-Mex a clasificación en 17 
    #' clases CSI.
    #' @param x Vector numérico a clasificar
    case_when(
        x == 0 ~ 0,
        x <= 2 ~ 1,
        x == 3 ~ 2,
        x == 6 ~ 3,
        x == 8 ~ 4,
        x %in% c(7, 9, 10) ~ 5,
        x %in% 11:12 ~ 6,
        x %in% c(14, 15, 18) ~ 7,
        x %in%  4:5 ~ 8,
        x %in% c(13, 16, 17, 19, 20) ~ 9,
        x %in%  21:22 ~ 10,
        x %in%  23:24 ~ 11,
        x %in%  25:26 ~ 12,
        x ==  27 ~ 13,
        x ==  28 ~ 14,
        x ==  29 ~ 15,
        x == 30 ~ 16,
        x == 31 ~17
    )
}

# funciones sencillas para salidas
imprime_porcent <- function(x) round(100 * mean(x), 1)
escribe_tab <- function(x){write.table(x, row.names = FALSE, sep = ",")}


# funciones para estimar con diseño muestral
estima_total <- function(data_design, var_y, var_group) {
    #' Estimaciones de porcentaje clasificado correcto
    #' 
    #' Utiliza el diseño de muestreo para estimar el porcentaje de área 
    #' clasificada correctamente. Puede utilizarse para estimaciones a total 
    #' o por grupo (por ejemplo por estado).
    #' @param data_design
    #' @param var_y nombre de la variable en data_design (sin entrecomillar)
    #'   que indica si la clasificación es correcta.
    #' @param var_group  opcional, nombre de la variabla en data_design (sin 
    #'   entrecomillar que indica el grupo).
    #' @return data.frame con las estimaciones y errores estándar
    var_y_eq <- enquo(var_y)
    var_name <- quo_name(var_y_eq)
    if(missing(var_group)) {
        ests <- data_design %>% 
            srvyr::summarise(!!var_name := survey_mean(x = !!var_y_eq, 
                proportion = TRUE)) %>% 
            dplyr::mutate_all(list(~round(100 * ., 1))) 
    } else {
        var_group <- enquo(var_group)
        ests <- data_design %>% 
            srvyr::group_by(!!var_group) %>% 
            srvyr::summarise(!!var_name := survey_mean(x = !!var_y_eq, 
                proportion = TRUE)) %>% 
            dplyr::mutate_if(is.numeric, list(~round(100 * ., 1))) 
    }
    return(ests)
}

#' estima_total <- function(data_design, var_y, ...) {
#'     #' Estimaciones de porcentaje clasificado correcto
#'     #' 
#'     #' Utiliza el diseño de muestreo para estimar el porcentaje de área 
#'     #' clasificada correctamente. Puede utilizarse para estimaciones a total 
#'     #' o por grupo (por ejemplo por estado).
#'     #' @param data_design
#'     #' @param var_y nombre de la variable en data_design (sin entrecomillar)
#'     #'   que indica si la clasificación es correcta.
#'     #' @param ...  opcional, nombre de las variables en data_design (sin 
#'     #'   entrecomillar que indica el grupo).
#'     #' @return data.frame con las estimaciones y errores estándar
#'     var_y_eq <- enquo(var_y)
#'     var_name <- quo_name(var_y_eq)
#'     if(missing(var_group)) {
#'         ests <- data_design %>% 
#'             srvyr::summarise(!!var_name := survey_mean(x = !!var_y_eq)) %>% 
#'             dplyr::mutate_all(list(~round(100 * ., 1))) 
#'     } else {
#'         group_var <- enquos(...)
#'         ests <- data_design %>% 
#'             srvyr::group_by(!!!var_group) %>% 
#'             srvyr::summarise(!!var_name := survey_mean(x = !!var_y_eq)) %>% 
#'             dplyr::mutate_if(is.numeric, list(~round(100 * ., 1))) 
#'     }
#'     return(ests)
#' }

estima_clase <- function(data_design, var_clase_madmex, 
    var_clase_interp, var_y, var_filter, value_filter, top = TRUE, 
    n_clases = 17) {
    #' Estimaciones de ususario y productor para porcentaje clasificado correcto
    #' 
    #' Utiliza el diseño de muestreo para estimar el porcentaje de área 
    #' clasificada correctamente.
    #' @param data_design
    #' @param var_clase_madmex nombre de la variable en data (sin entrecomillar)
    #'   que correponde a la clasificación del algoritmo
    #' @param var_clase_interp nombre de la variable en data (sin 
    #'   entrecomillar) que correponde a la clasificación del revisor.
    #' @param var_y nombre de la variable en data_design (sin entrecomillar)
    #'   que indica si la clasificación es correcta.
    #' @param top variable lógica que indica si las exactitudes deben tomar en 
    #'   cuenta la primera etiqueta del revisor o las tres asignadas.
    #' @param var_filter sí se desea aplicar un filtro en la base es la variable
    #'   variable que se usará para filtrar (sin entrecomillar) y el valor 
    #' @return data.frame con las estimaciones y errores estándar.
    #' @details En el caso de las exactitudes de usuario top3 se estima la 
    #'   probabilidad de que un pixel etiquetado como una clase dada por el 
    #'   revisor en alguna de las tres etiquetas asignadas esté correctamente 
    #'   etiquetado, notar que la clase que asigna el algoritmo MAD-Mex puede 
    #'   estar bien incluso cuando no coincide con la etiqueta que se está 
    #'   evaluando.
    var_clase_madmex_eq <- enquo(var_clase_madmex)
    var_y_eq <- enquo(var_y)
    
    if(!missing(var_filter)){
        var_filter_eq <- enquo(var_filter)
        data_design <- srvyr::filter(data_design, 
            !!var_filter_eq == value_filter)
    }
    usuario <- data_design %>% 
        srvyr::group_by(!!var_clase_madmex_eq) %>% 
        srvyr::summarise(est_usuario = survey_mean(x = !!var_y_eq, 
            proportion = FALSE)) %>% 
        srvyr::rename(clase = !!var_clase_madmex_eq)
    conteos_usuario <- data_design %>% 
        as.data.frame() %>% 
        dplyr::group_by(!!var_clase_madmex_eq) %>% 
        dplyr::summarise(n_usuario = n()) %>% 
        rename(clase = !!var_clase_madmex_eq)
    usuario <- usuario %>% 
        left_join(conteos_usuario, by = "clase")
    
    if(top){
        var_clase_interp_eq <- enquo(var_clase_interp)
        productor <- data_design %>% 
            srvyr::group_by(!!var_clase_interp_eq) %>% 
            srvyr::summarise(est_productor = survey_mean(x = !!var_y_eq, 
                proportion = FALSE)) %>% 
            rename(clase = !!var_clase_interp_eq)
        conteos_productor <- data_design %>% 
            as.data.frame() %>% 
            dplyr::group_by(!!var_clase_interp_eq) %>% 
            dplyr::summarise(n_productor = n()) %>% 
            rename(clase = !!var_clase_interp_eq)
        productor <- productor %>% 
            left_join(conteos_productor, by = "clase")
    } else {
        productor <- map_df(set_names(1:n_clases), 
            ~estima_productor_clase(data_design, clase_num = ., 
                var_clase_madmex = !!var_clase_madmex_eq, 
                var_y = !!var_y_eq, n_clases = n_clases), 
            .id = "clase") %>% 
            mutate(clase = as.numeric(clase))
    }
    estimaciones <- usuario %>% 
        left_join(productor, by = "clase") %>% 
        mutate_at(vars(est_usuario, est_usuario_se, est_productor, 
            est_productor_se), list(~round(100 * .)), digits = 1)
    
    return(estimaciones)
}

estima_productor_clase <- function(data_design, clase_num, var_clase_madmex, 
    var_y, n_clases = 17) {
    #' Función auxiliar de estima_clase
    var_clase_madmex_eq <- enquo(var_clase_madmex)
    var_y_eq <- enquo(var_y)
    bd_df <- as.data.frame(data_design)
    if(n_clases == 17){
        clase_obj <- clase_num == bd_df$interp1_c17 | 
            clase_num == bd_df$interp2_c17 | clase_num == bd_df$interp3_c17
    } else {
        clase_obj <- clase_num == bd_df$interp1_c31 | 
            clase_num == bd_df$interp2_c31 | clase_num == bd_df$interp3_c31
    } 
    n_casos <- sum(clase_obj)
    data_design %>% 
        srvyr::mutate(clase_obj = clase_obj) %>% 
        srvyr::filter(clase_obj) %>% 
        srvyr::summarise(est_productor = survey_mean(x = !!var_y_eq, 
            proportion = TRUE)) %>% 
        mutate(n_productor = n_casos)
}


prop_clase_crudo <- function(data, var_clase_madmex, 
    var_clase_interp = NULL, var_y, top = TRUE, n_clases = NULL) {
    #' Porcentaje correcto por clase
    #' 
    #' Calcula exactitudes de usuario y productor ignorando diseños de muestreo.
    #' @param data data.frame con variables etiquetadas por algoritmo y por
    #'   expertos y con variable que indique si la clasificación fue correcta.
    #' @param var_clase_madmex nombre de la variable en data (sin entrecomillar)
    #'   que correponde a la clasificación del algoritmo
    #' @param var_clase_interp nombre de la variable en data (sin 
    #'   entrecomillar) que correponde a la clasificación del revisor.
    #' @param var_y nombre de la variable en data (sin entrecomillar)
    #'   que indica si la clasificación es correcta.
    #' @return data.frame con proporciones correctas de usuario y productor y 
    #'   con tamaños de muestra con que se calcularon las proporciones en cada 
    #'   clase.
    #' @details En el caso de prop_usuario se dividen los grupos por clase del 
    #'   algoritmo var_clase_madmex y en cada subconjunto de los datos se 
    #'   calcula el porcentaje correcto de acuerdo a la variable var_y. 
    #'   Para prop_productor, en el caso top = TRUE se hace algo 
    #'   análogo con los subconjuntos determinados por la clase asignada por el 
    #'   revisor. En el caso top3 (top = FALSE) no es claro como definir 
    #'   exactitud de productor, se reportan dos cantidades: 1. De todo lo que
    #'   tiene clase 1 en alguna de las tres clases, ¿qué porcentaje está 
    #'   correctamente clasificado? y 2. De todo lo que tiene clase 1 en alguna 
    #'   de las tres clases, ¿qué porcentaje fue clasificado como clase 1 por el 
    #'   algoritmo?
    var_clase_madmex_eq <- enquo(var_clase_madmex)
    var_y_eq <- enquo(var_y)
    
    usuario <- data %>% 
        group_by(!!var_clase_madmex_eq) %>% 
        summarise(
            prop_usuario = mean(x = !!var_y_eq), 
            n_usuario = n()
            ) %>% 
        rename(clase = !!var_clase_madmex_eq)
    
    if(top){
        var_clase_interp_eq <- enquo(var_clase_interp)
        productor <- data %>% 
            srvyr::group_by(!!var_clase_interp_eq) %>% 
            srvyr::summarise(
                prop_productor = mean(x = !!var_y_eq), 
                n_productor = n()
                ) %>% 
            rename(clase = !!var_clase_interp_eq)
    } else {
        productor <- map_df(set_names(1:n_clases), 
            ~prop_productor_clase_crudo(data, ., 
                var_clase_madmex = !!var_clase_madmex_eq, var_y = !!var_y_eq, 
                n_clases = n_clases), .id = "clase") %>% 
            mutate(clase = as.numeric(clase))
    }
    estimaciones <- usuario %>% 
        left_join(productor, by = "clase") %>% 
        mutate_at(vars(contains("prop")), list(~round(100 * .)), 
            digits = 1)
    return(estimaciones)
}

prop_productor_clase_crudo <- function(data, clase, var_clase_madmex, var_y, 
    n_clases = 17) {
    #' Auxiliar de prop_clase_crudo
    #' 
    #' Calcula exactitudes de productor cuando se utiliza más de una etiqueta 
    #' (top3).
    var_clase_madmex_eq <- enquo(var_clase_madmex)
    var_y_eq <- enquo(var_y)
    data_clase <- data %>% 
        rowwise() %>% 
        mutate(
            clase_obj = ifelse(n_clases == 17, clase %in% c(interp1_c17, 
                interp2_c17, interp3_c17), 
                clase %in% c(interp1_c31, interp2_c31, interp3_c31))
        ) %>% 
        ungroup() %>% 
        filter(clase_obj) %>% 
        summarise(
            prop_productor = mean(x = !!var_y_eq), 
            prop_productor_2 = mean(clase == !!var_clase_madmex_eq),
            n_productor = n()
            ) 
    data_clase
}

calcula_productor_clase_n <- function(data, clase, var_clase_madmex, var_y, 
    n_clases = 17) {
    var_clase_madmex_eq <- enquo(var_clase_madmex)
    var_y_eq <- enquo(var_y)
    data_clase <- data %>% 
        rowwise() %>% 
        mutate(
            clase_obj = ifelse(n_clases == 17, clase %in% c(interp1_c17, 
                interp2_c17, interp3_c17), 
                clase %in% c(interp1_c31, interp2_c31, interp3_c31)), 
            raster_f = factor(!!var_clase_madmex_eq, levels = 1:n_clases)
        ) %>% 
        ungroup() %>% 
        filter(clase_obj, !(!!var_y_eq)) %>% 
        pull(raster_f) %>% 
        table()
}

