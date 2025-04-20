# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Clasificación de usos del suelo en R (GEOBIA)
# 
#
# OBJETIVOS:
# - Toma de datos para modelizar (QGIS)
# - Focal statistics: extraer valores de la segmentación

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(arrow, geoarrow, exactextractr, sf, terra, tidyverse)

# 2. Cargar datos --------------------------------------------------------

## ortofoto
ortofoto_sr <- rast("00_data/03-geobia/ortofoto-downsample.tiff")
names(ortofoto_sr) <- c("R", "G", "B")

## segmentacion
segment_list <- map(
  list.files("00_data/03-geobia/lsms_tiles/", full.names = TRUE),
  \(x) open_dataset(x) |> st_as_sf()
)

## puntos muestreo
points_sf <- read_sf("00_data/03-geobia/training-points.gpkg") |> 
  st_transform(crs(ortofoto_sr))


# 3. Fstats - 1 tile -----------------------------------------------------

## seleccionar 1 elementos de la lista
selected_segment <- segment_list[[1]]

## focal statistics
fstats_df <- exact_extract(
  x   = ortofoto_sr,
  y   = selected_segment,
  fun = c("median", "stdev", "count", "min", "max")
)

## añadir valores a la segmentación
selected_segment_stats_sf <- bind_cols(selected_segment, fstats_df)

## añadir clase, y filtrar solamente puntos que caen en esa tile
st_join(selected_segment_stats_sf, points_sf) |> 
  filter(!is.na(class))


# 4. Fstats - Entero -----------------------------------------------------

## 4.1. Focal statistics --------------------

## función para focal statistics
segment_fstats <- function(segment) {
  ## focal stats
  segment_stats_sf <- exact_extract(
    x   = ortofoto_sr,
    y   = segment,
    fun = c("median", "stdev", "count", "min", "max")
  )

  ## unir a segment
  bind_cols(segment, segment_stats_sf)
}

## focal statistics (duración ~ 7min)
tic()
segment_stats_list <- map(
  segment_list,
  segment_fstats,
  .progress = TRUE
)
toc()

## exportar
map2(
  segment_stats_list,
  1:length(segment_stats_list),
  \(segment_tile, id) write_parquet(
    segment_tile,
    glue::glue("00_data/03-geobia/lsms_tiles_stats/{id}_segment_stats.parquet")
  )
)

## cargar resultados anteriores
# segment_stats_list <- map(
#   list.files("00_data/03-geobia/lsms_tiles_stats/", full.names = TRUE),
#   \(file) open_dataset(file) |> st_as_sf()
# )

## 4.2. Datos modelo ----------------------

## extraer valores de la segmentación en la localización de los puntos, tile by tile
point_stats_list <- map(
  segment_stats_list,
  \(segment_stat) st_join(segment_stat, points_sf) |> 
    filter(!is.na(class))
)

## unir en un solo objeto
point_stats_sf <- bind_rows(point_stats_list)

## exportar
write_sf(point_stats_sf, "00_data/03-geobia/training-points-stats.gpkg")
