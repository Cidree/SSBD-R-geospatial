# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Clasificación de usos del suelo en R (GEOBIA)
# 
#
# OBJETIVOS:
# - Implementar modelo elegido

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(
  arrow, geoarrow, mapview, ranger, sf, stars, terra, tidymodels, tidyterra, tidyverse
)

# 2. Cargar datos --------------------------------------------------------

## segmentacion con datos de entrada
segm_list <- map(
  list.files("00_data/03-geobia/ms_tiles_stats/", full.names = TRUE),
  \(file) open_dataset(file) |> st_as_sf()
)

## modelo
best_model <- read_rds("00_data/03-geobia/best_ranger.rds")

# 3. Predicciones - 1 objeto ---------------------------------------------

## seleccionar una segmentación
segm_sf <- segm_list[[1]]

## predicciones
predict(best_model, segm_sf)

## añadir predicciones a datos de entrada
preds_sf <- augment(best_model, segm_sf) |> 
  st_as_sf()

## Visualizar
mapview(preds_sf, zcol = ".pred_class")

## convertir a raster
preds_sr <- st_rasterize(preds_sf[".pred_class"], dx = 5, dy = 5) |> 
  rast()

## visualizar
plot(preds_sr)

## filtro para eliminar ruido
preds_filter_sr <- focal(preds_sr, w = 3, fun = "modal", expand = TRUE)

## visualizar
plot(preds_filter_sr)

# 4. Predicciones - Todo -------------------------------------------------

## crear función
pred_as_rast <- function(data) {

  ## predicciones
  preds_sf <- augment(best_model, data) |> 
    st_as_sf()

  ## rasterizar y filtrar
  st_rasterize(preds_sf[".pred_class"], dx = 1, dy = 1) |> 
    rast() |> 
    focal(w = 5, fun = "modal", expand = TRUE)

}

## aplicar función
preds_list <- map(segm_list, pred_as_rast, .progress = TRUE)

## juntar todos los rasters
preds_sr <- sprc(preds_list) |> 
  merge()

## visualizar
ggplot() +
  geom_spatraster(
    data = preds_sr
  ) +
  scale_fill_manual(
    values = c("#F7FF58", "#5FAD41", "#8ACDEA", "#76818E"),
    name = "Clase"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(r = 20)
  )

## exportar
writeRaster(preds_sr, "00_data/03-geobia/predicciones-ranger.tiff", overwrite = TRUE)
