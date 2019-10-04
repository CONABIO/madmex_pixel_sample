library(ggplot2)
library(DT)
theme_set(theme_minimal())
source("../scripts/crear_datos.R")

navbarPage("Validación MAD-Mex 2018",
    tabPanel("Metodología",
        fluidRow(
            column(width = 6, 
                h2("Diseño de la muestra"),  
                p("Utilizamos muestreo probabilístico, en particular muestreo 
                    estratificado unietápico, dentro de cada estrato se 
                    seleccionaron los pixeles mediante muestreo aleatorio 
                    simple."),
                p(strong("Marco muestral"), ": Mapa MADMEX 2018 (raster)."), 
                p(strong("Estratificación"), ": Se definieron los estratos 
                    como el cruce de estado y clase (CONABIO/SEMARNAT/INEGI) 
                    dando lugar a 450 estratos."), 
                h2("Protocolo de validación"), 
                p("Los evaluadores no sabrán la clase que MADMEX ha asignado."), 
                p("En cada pixel el evaluador deberá etiquetar la case 
                    prevalente observada en el pixel y tomando en cuenta el 
                    entorno, en caso de duda, es decir, si el evaluador no tiene 
                    certeza de la clase, podrá elegir dos o tres opciones."),
                p("Consideramos 2 criterios para determinar que un píxel está
                    correctamente clasificado:"),
                p(strong("Top"), "si la etiqueta principal del evaluador
                    coincide con la etiqueta asignada por el algoritmo."), 
                p(strong("Top3"), "si alguna de las 3 etiquetas del evaluador 
                    coincide con la etiqueta asignada por el algoritmo.")
                ),
            column(width = 6, h3("Descripción de clases"),
                selectInput("N_clases", label = "# clases", 
                    choices = c("17", "31"), selected = "17"), 
                tableOutput("tab_clases"))
            )
    ),
    tabPanel("Total",
        fluidRow(
            column(width = 3, 
                selectInput("agregacion", label = "Clasificación", 
                    choices = c("Nacional", "Edo"), selected = "Nacional"), 
                selectInput("tipo_est_total", label = "Tipo", 
                    choices = c("Top", "Top3", "Ambas"), selected = "Top"),
                p("Estimaciones a total (sin dividir por clase) de porcentaje de 
                    área correctamente clasificada. En el caso de estados se 
                    grafican las estimaciones +- un error estándar.")),
            column(width = 4, dataTableOutput("tab_ests_total")),
            column(width = 5, plotOutput("plot_ests_total"))
            )
    ),
    tabPanel("Por clase",
        fluidRow(
            column(width = 2, 
                selectInput("edo", label = "Estado", choices = edos, 
                    selected = "Nacional"), 
                selectInput("tipo_est", label = "Tipo", 
                    choices = c("Top", "Top3"), selected = "Top"), 
                p("Estimaciones por clase, para la clasificación de 17 clases")),
            column(width = 3, plotOutput("int")),
            column(width = 7, dataTableOutput("tab_est_clase"))
        )
    )
)
