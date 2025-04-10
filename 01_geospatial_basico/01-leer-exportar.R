# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# Ejercicio: 01 Leer y exportar con `sf`
# 
#
# OBJETIVOS:
# - Descarga de datos utilizando `giscoR`
# - Exportar datos vectoriales en distintos formatos
# - Importar datos vectoriales en distintos formatos
# - Visualizar datos espaciales en mapa web
# - Explorar y borrar capas de un geopackage


# 1. Cargar paquetes -----------------------------------------------------

## install.packages("pacman")
library(pacman)

p_load(giscoR, mapview, sf, tidyverse)


# 2. Descargar datos -----------------------------------------------------

## descargar límites de los países
world_sf <- gisco_get_countries()

## descargar provincias España
prov_sf <- gisco_get_nuts(
  country    = "Spain",
  nuts_level = 3
)


# 3. Exploración datos ---------------------------------------------------

## 3.1. Países ---------------------------

## visualizar con mapview
mapview(world_sf, layer.name = "Países")

## filtrar Perú
peru_sf <- world_sf |> 
  filter(ISO3_CODE == "PER") |> 
  select(NAME_ENGL)


## 3.2. España ---------------------------

## filtrar Córdoba y Sevilla
## seleccionar solamente NAME_LATN y renombrar
prov_filter_sf <- prov_sf |> 
  filter(NAME_LATN %in% c("Córdoba", "Sevilla")) |> 
  select(prov = NAME_LATN)

## visualizar de forma interactiva
## - Ideal para exploración de datos de forma interactiva
mapview(prov_filter_sf, zcol = "prov")

## visualizar con base R
## - Ideal para visualizar datos rápido
plot(prov_filter_sf['prov'], main = "Provincias")

## visualizar con ggplot2
## - Visualizar datos rápidamente
## - Generar mapas de buena calidad
## - Mayor personalización con el paquete más descargado de R
ggplot(prov_filter_sf) +
  geom_sf(aes(fill = prov), show.legend = FALSE) +
  geom_sf_text(aes(label = prov)) +
  theme_void()


# 4. Exportar / Importar -------------------------------------------------

## exportar como shapefile
write_sf(prov_filter_sf, "00_data/01-geospatial-basico/provincias.shp")

## exportar como geojson
write_sf(prov_filter_sf, "00_data/01-geospatial-basico/provincias.geojson")
st_write(peru_sf, "00_data/01-geospatial-basico/peru.geojson")

## exportar como geopackage
write_sf(prov_filter_sf, "00_data/01-geospatial-basico/datos_base.gpkg", layer = "provincias")
write_sf(peru_sf, "00_data/01-geospatial-basico/datos_base.gpkg", layer = "peru")

## explorar capas geopackage
st_layers("00_data/01-geospatial-basico/datos_base.gpkg")

## leer archivo
peru_01_sf <- read_sf("00_data/01-geospatial-basico/peru.geojson")
peru_02_sf <- st_read("00_data/01-geospatial-basico/peru.geojson")

## leer capa de geopackage
peru_03_sf <- read_sf("00_data/01-geospatial-basico/datos_base.gpkg", layer = "peru")
