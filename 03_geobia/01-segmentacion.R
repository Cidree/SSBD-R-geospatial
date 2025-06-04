# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Clasificación de usos del suelo en R (GEOBIA)
#
# Imágenes descargadas (Centro Descargas CNIG):
# - PNOA-MA-OF-ETRS89-HU29-H25-0046-2.tif
# - PNOA-MA-IRG-OF-ETRS89-HU29-H25-0046-2.tif
#
# OBJETIVOS:
# - Descarga de Orfeo Toolbox
# - Segmentación de imagen con large scale meanshift
# - Segmentación imagen vs segmentación por batches

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(arrow, geoarrow, link2GI, mapview, OTBsegm, sf, terra, tidyverse)

# 2. Cargar datos --------------------------------------------------------

ortofoto_sr <- rast("00_data/03-geobia/ortofoto-downsample.tiff")

# 3. Test segmentacion ---------------------------------------------------

## Antes de aplicar la segmentación a toda la imagen, hacer pruebas
## para ver cuáles son los mejores parámetros para nuestro caso

## cortar area pequeña
sample_sr <- crop(ortofoto_sr, ext(585000, 585000 + 500, 4790000, 4790000 + 500))

## visualizar
plotRGB(sample_sr)

## localizar OTB
otblink <- linkOTB(searchLocation = "C:/OTB/")

## segmentar
segment_sf <- segm_lsms(
    image    = sample_sr,
    otb      = otblink,
    spatialr = 5,
    ranger   = 25,
    minsize  = 200
)

## visualizar resultado
plotRGB(sample_sr)
plot(sf::st_geometry(segment_sf), add = TRUE, border = "gray90")

## segmentar
segment_ms_sf <- segm_meanshift(
    image    = sample_sr,
    otb      = otblink,
    spatialr = 5,
    ranger   = 20,
    minsize  = 200
)

## visualizar resultado
plotRGB(sample_sr)
plot(sf::st_geometry(segment_ms_sf), add = TRUE, border = "gray90")

# 4. Segmentación completa -----------------------------------------------

## 4.1. OPCIÓN 1 - Aplicar a ortofoto ------------

## - Intensivo computacionalmente
## - Más lento
## - Menos efectos de borde
## - Menor complejidad

## segmentar (22min)
segment_sf <- segm_meanshift(
    image    = ortofoto_sr,
    otb      = otblink,
    spatialr = 5,
    ranger   = 20,
    minsize  = 200,
    vector_tilesize = 5000
)

## 4.2. OPCIÓN 2 - Aplicar por barches ----------

## Aplicar por batches
## - Menos instensivo para el ordenador
## - Más rápido
## - Muchos archivos de menor peso
## - Más efectos de borde
## - Necesidad de realizar más pasos

## crear tiles
tiles_sf <- st_make_grid(
    x = ext(ortofoto_sr),
    n = 5,
    crs = crs(ortofoto_sr)
  ) |> 
    st_as_sf() |> 
    rowid_to_column()

## visualizar
mapview(tiles_sf)

## función para segmentar por tile
segment_tile <- function(tile) {

    ## cortar imagen a la tile
    ortofoto_tile_sr <- crop(ortofoto_sr, tile)

    ## segmentar
    segment_sf <- segm_meanshift(
        image    = ortofoto_tile_sr,
        otb      = otblink,
        spatialr = 5,
        ranger   = 20,
        minsize  = 200,
        vector_tilesize = 5000
      )
  
    ## seleccionar columna geometria
    segment_sf <- select(segment_sf, geometry)
    return(segment_sf)
}

## aplicar función a todos los tiles
## - Duración: ~1min/tile ~ 20min
segment_list <- map(
    split(tiles_sf, tiles_sf$rowid),
    segment_tile
)

## exportar
map2(
    segment_list,
    1:length(segment_list),
    \(x, y) write_parquet(x, glue::glue("00_data/03-geobia/ms_tiles/{y}_segmentation_ms.parquet"))
)
