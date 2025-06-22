# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Clasificación de usos del suelo en R (GEOBIA)
# 
#
# OBJETIVOS:
# - Toma de datos para modelizar (QGIS)
# - Zonal statistics: extraer valores de la ortofoto

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(arrow, geoarrow, exactextractr, mapview, sf, terra, tidyverse)

# 2. Cargar datos --------------------------------------------------------

## ortofoto
ortofoto_sr <- rast("00_data/03-geobia/ortofoto-downsample.tiff")
names(ortofoto_sr) <- c("R", "G", "B")

## segmentacion
segment_list <- map(
  list.files("00_data/03-geobia/ms_tiles/", full.names = TRUE),
  \(x) open_dataset(x) |> st_as_sf(),
  .progress = TRUE
)

## puntos entrenamiento
points_sf <- read_sf("00_data/03-geobia/training-points.gpkg") |> 
  st_transform(crs(ortofoto_sr))


# 3. zstats - 1 tile -----------------------------------------------------

## seleccionar 1 elementos de la lista
selected_segment <- segment_list[[1]]

## zonal statistics
zstats_df <- exact_extract(
  x   = ortofoto_sr,
  y   = selected_segment,
  fun = c("median", "stdev", "count", "min", "max")
)

## añadir valores a la segmentación
selected_segment_stats_sf <- bind_cols(selected_segment, zstats_df)

## añadir clase, y filtrar solamente puntos que caen en esa tile
points_selected_stats_sf <- st_join(points_sf, selected_segment_stats_sf) |> 
  filter(!is.na(median.R))

## visualizar
mapview(selected_segment_stats_sf, color = "red", alpha.regions = 0) +
  mapview(points_selected_stats_sf, col.regions = "yellow")

# 4. Zstats - Entero -----------------------------------------------------

## 4.1. Zonal statistics --------------------

## función para Zonal statistics
segment_zstats <- function(segment) {
  ## zonal stats
  segment_stats_sf <- exact_extract(
    x   = ortofoto_sr,
    y   = segment,
    fun = c("median", "stdev", "count", "min", "max")
  )

  ## unir a segment
  bind_cols(segment, segment_stats_sf)
}

## zonal statistics (duración ~ 4min)
segment_stats_list <- map(
  segment_list,
  segment_zstats,
  .progress = TRUE
)

## exportar
map2(
  segment_stats_list,
  1:length(segment_stats_list),
  \(segment_tile, id) write_parquet(
    segment_tile,
    glue::glue("00_data/03-geobia/ms_tiles_stats/{id}_segment_stats.parquet")
  )
)

## cargar resultados anteriores
# segment_stats_list <- map(
#   list.files("00_data/03-geobia/ms_tiles_stats/", full.names = TRUE),
#   \(file) open_dataset(file) |> st_as_sf()
# )

## 4.2. Datos modelo ----------------------

## extraer valores de la segmentación en la localización de los puntos, tile by tile
point_stats_list <- map(
  segment_stats_list,
  \(segment_stat) st_join(points_sf, segment_stat) |> 
    filter(!is.na(median.R))
)

## unir en un solo objeto
point_stats_sf <- bind_rows(point_stats_list)

## exportar
write_sf(point_stats_sf, "00_data/03-geobia/training-points-stats.gpkg")
