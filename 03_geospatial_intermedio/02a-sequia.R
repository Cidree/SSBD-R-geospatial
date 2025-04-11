# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 02: Sequía en Andalucía - Descarga de datos
# 
#
# OBJETIVOS:
# - Descarga de datos de precipitación

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(chirps, giscoR, sf, terra, tidyverse)

# 2. Descarga de datos ---------------------------------------------------

## 2.1. Área de estudio -------------------

study_area_vect <- gisco_get_nuts(country = "ES", nuts_level = 2) |> 
  filter(NAME_LATN == "Andalucía") |> 
  vect()

## 2.2. Ejemplo descarga -----------------

## descargar datos de precipitación - ejemplo
muestra_sr <- get_chirps(
  object = study_area_vect,
  dates  = c("2017-01-01", "2017-01-10"),
  server = "CHC"
)

## enmascarar
muestra_sr <- clamp(muestra_sr, 0) |> 
  mask(study_area_vect)

## visualizar
plot(muestra_sr)

## 2.3. Descarga total ------------------

## función para descargar datos año a año
download_chirps <- function(year) {
  
  ## fechas de inicio y fin
  start_date <- str_glue('{year}-01-01')
  end_date <- str_glue('{year}-12-31')

  ## descargar muestra
  muestra_sr <- get_chirps(
    object = study_area_vect,
    dates  = c(start_date, end_date),
    server = "ClimateSERV"
  )

  ## exportar
  writeRaster(muestra_sr, str_glue("00_data/02-geospatial-intermedio/prec_chirps/prec_{year}.tiff"), overwrite = TRUE)
  message(str_glue("Año {year} terminado"))

}

## descargar (10 años tardan ~ 2 horas)
map(2015:2024, download_chirps, .progress = TRUE)
