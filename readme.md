Diseño muestral y análisis para validación de mapa MAD-Mex 2018.

## Diseño de la muestra

Utilizamos muestreo probabilístico, en particular muestreo estratificado 
unietápico, dentro de cada estrato se seleccionan los pixeles mediante muestreo 
aleatorio simple.

* Marco muestral: Mapa MADMEX 2018 (raster).

* Estratos: definidos por estado $\times$ clase (17 clases 
CONABIO/SEMARNAT/INEGI) dando lugar a 450 estratos.

### Descripción de scripts
Propuesta de tamaño de muestra por estrato de acuerdo a objetivo: evaluar clase x estado. 
 * Input: número de pixeles por clase x estado (calculado por Julián en python). 

3-revison_entrega

* scripts/1-crear_muestra_revision.R
Se genera una muestra de tamaño 300 que evaluó Pedro para evaluar el trabajo de Bits. Esta muestra es estratificada para representar todas las clases y se crea a partir de una entrega parcial de Bits.
 - Input: `datos_entrada/datos_bits/Validacion_Final_Mapa-2018_BITS_190211/validacion_final_2018.shp` (Bits).
 - Output: `datos_salida/muestras_pedro/muestra_300.shp`

* scripts/2-crear_datos_revision.R
Reune la entrega de Bits con el marco muestral y la muestra original con el fin de crear el insumo para la estimación. 
 - Inputs: `datos_entrada/datos_bits/PUNTOS DE VALIDACION-2018/Puntos_de_Validacion_2018_Revisados.shp` (Bits)
    `datos_entrada/madmex_sentinel2_2018_31.tif` (Thilo y Steffen, mapa final distinto al mapa con el que se diseñó la muestra) 
    `datos_salida/tamanos_2.csv` (Julián, indica los tamaños de cada estrato, es decir número de pixeles por clase y edo)
    `datos_salida/muestra_pais.rds` (Julián, muestra entregada a los revisores, sirve para identificar estratos del diseño)
 - Outputs: `datos_salida/bits_2018_weights.rdata` (datos con variables de diseño muestral y evaluaciones de expertos)
    `datos_salida/bits_2018_design.rdata` (datos de tipo survey para hacer estimación con paquete de R)
 Además crea los datos para evaluar la entrega de Bits comparándola con los resultados de Pedro.
 - Inputs: `datos_salida/muestras_pedro_etiquetada/muestra300_etiq_pedro.shp` (Pedro, muestra etiquetada)
   `datos_entrada/datos_bits/Validacion_Final_Mapa-2018_BITS_190211/validacion_final_2018.shp` (Bits primera entrega)
 - Outputs: `datos_salida/bits_pedro.rds` (datos con variables de Pedro, Bits y Madmex)
   `datos_salida/bits_pedro_design.rds` (datos de tipo survey para hacer estimación con paquete de R)

5-resultados
app: aplicación shiny para explorar resultados, incluye tablas y gráficas con 
resultados nacionales y estatales, a total y por estado. Su insumo son 
las tablas generadas por `3-revison_entrega/scripts/estimaciones_ponderadas.R`.

scripts: código auxiliar para la app.

