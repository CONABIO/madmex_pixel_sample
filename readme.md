Diseño muestral y análisis para validación de mapa MAD-Mex 2018.

## Diseño de la muestra

Utilizamos muestreo probabilístico, en particular muestreo estratificado 
unietápico, dentro de cada estrato se seleccionan los pixeles mediante muestreo 
aleatorio simple.

* Marco muestral: Mapa MADMEX 2018 (raster).

* Estratos: definidos por estado $\times$ clase (17 clases 
CONABIO/SEMARNAT/INEGI) dando lugar a 450 estratos.

### datos
* `datos_entrada/madmex_sentinel2_2018_31.tif`: Mapa con muestra generada por 
Julián.


3-revison_entrega

* scripts/estimaciones_ponderadas.R: genera csv's con estimaciones que se 
almacenan en datos_salida estimaciones a total y por clase (17 y 31 clases), a 
nivel nacional y por estado.

5-resultados

app: aplicación shiny para explorar resultados, incluye tablas y gráficas con 
resultados nacionales y estatales, a total y por estado. Su insumo son 
las tablas generadas por `3-revison_entrega/scripts/estimaciones_ponderadas.R`.

scripts: código auxiliar para la app.


entradas:

* datos_entrada/madmex_sentinel2_2018_31.tif: mapa MADMEX a evaluar.
* datos_entrada/datos_bits/PUNTOS DE VALIDACION-2018/Puntos_de_Validacion_2018_Revisados.shp: 
datos con etiqueta BITS.
* datos_salida/muestras_pedro_etiquetada/muestra300_etiq_pedro.shp:  mapa con 
clasificacipnes de Pedro (muestra tamaño 300).

salidas: 
* datos_salida/bits_2018_lcc.rdata: mapa con columna de clasificación correcta