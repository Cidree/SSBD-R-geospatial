# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 02: Sequía en Andalucía - Descarga de datos
# 
#
# OBJETIVOS:
# - Descarga de datos de precipitación
# - Programación funcional
# - Procesamiento de múltiples rásters
# - Crear mapas del número de días secos

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(giscoR, sf, terra, tidyverse, tidyterra)

# 2. Cargar datos --------------------------------------------------------

## 2.1. Área de estudio -------------------

study_area_sf <- gisco_get_nuts(country = "ES", nuts_level = 2) |> 
  filter(NAME_LATN == "Andalucía")

## 2.2. Datos de sequía -------------------

## archivos a leer
prec_paths <- list.files("00_data/02-geospatial-intermedio/prec_chirps/", full.names = TRUE)

## leer archivos
prec_list <- map(prec_paths, rast)

# 3. Preparar datos ------------------------------------------------------

## enmascarar área de estudio
prec_mask_list <- map(prec_list, \(x) crop(x, study_area_sf, mask = TRUE))

## visualizar 
plot(prec_mask_list[[2]])

## eliminar valores extremos
prec_clamp_list <- map(prec_mask_list, \(x) clamp(x, lower = 0))

## visualizar 
plot(prec_clamp_list[[2]][[1]])

## crear raster donde 1 = no lluvia; 0 = lluvia. Actualmente:
## - 0 = no precipitación
## - >0 = precipitación
prec_prep_list <- map(prec_clamp_list, \(x) ifel(x == 0, 1, 0))

## visualizar
plot(prec_prep_list[[2]][[1]])

## sumar rasters para calcular número de días secos en el mes corriente
## - Desafío: tenemos los 365 días del año en el mismo raster
## - Debemos calcular la suma mes a mes
sum(prec_prep_list[[2]])


## crear función para aplicar a cada año
calculate_dry_days <- function(year, raster) {

  ## crear secuencia de días
  days_vec <- seq.Date(
    from = as.Date(paste0(year, "-01-01")),
    to   = as.Date(paste0(year, "-12-31")),
    by   = "day"
  )

  ## iterar sobre cada mes
  result_lst <- list()
  for (mes in 1:12) {

    ## convertir mes a string de 2 caracteres rodeado de guiones (e.g. "-01-")
    mes_str <- sprintf("-%02d-", mes)

    ## obtener IDs de days_vec del mes actual
    mes_ids <- which(str_detect(days_vec, mes_str))

    ## seleccionar esos IDs del raster y sumarlos
    result_lst[[mes]] <- raster[[mes_ids]] |> sum()

  }

  ## reducir lista a un raster
  result_sr <- rast(result_lst)

  ## renombrar capas
  names(result_sr) <- paste0(month.name, " ", year)

  return(result_sr)

}

## probar función - aplicar a un solo año
calculate_dry_days(2015, prec_prep_list[[1]])

## aplicar a todos los años
dry_days_list <- map2(
  2015:2024,
  prec_prep_list,
  calculate_dry_days
)

## visualizar
plot(dry_days_list[[1]])

## función para reclasificar
classify_dry_days <- function(raster) {

  ## crear matriz de reclasificación
  mat <- matrix(
    c(
      -Inf, 16, 1,
      16, 19, 2,
      19, 22, 3,
      22, 25, 4,
      25, 28, 5,
      28, Inf, 6
    ),
    ncol = 3,
    byrow = TRUE
  )

  ## aplicar reclasificación
  raster_class <- classify(raster, mat) |> as.factor()
  return(raster_class)

}

## aplicar función de reclasificación a todos los años
dry_days_class_list <- map(dry_days_list, classify_dry_days)

# 4. Mapa ----------------------------------------------------------------

## seleccionar un año
map_sr <- dry_days_class_list[[8]] 
year <- 2022

## elegir una paleta de colores
drought_colors <- grass.colors(6, "roygbiv")
names(drought_colors) <- 1:6

## mapear
ggplot() +
  geom_spatraster(data = map_sr) +
  # scale_fill_grass_d("roygbiv") +
  scale_fill_manual(
    values = drought_colors, 
    na.value = NA, 
    na.translate = FALSE, 
    breaks = names(drought_colors),
    labels = c("Very Wet", "Wet", "Moderately Wet", "Moderately Dry", "Dry", "Very Dry")
  ) +
  facet_wrap(vars(lyr)) +
  guides(
    fill = guide_legend(
      title = NULL,
      nrow  = 1,
      label.position = "bottom",
      position = "bottom"
    )
  ) +
  labs(
    title = str_glue("Drought in Andalucía (Spain) during the year {year}")
  ) +
  theme_void() +
  theme(
    legend.key.width     = unit(40, "mm"),
    legend.key.height    = unit(3, "mm"),
    legend.key.spacing.x = unit(0, "mm"),
    plot.background      = element_rect(fill = "gray90", colour = NA),
    plot.margin          = margin(1, 1, 1, 1, "cm"),
    text                 = element_text(color = "gray30"),
    plot.title           = element_text(
      face   = "bold",
      size   = 22,
      hjust  = .5,
      margin = margin(b = 10, unit = "mm")
    )
  )
