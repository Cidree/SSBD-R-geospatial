# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# Ejercicio: 02 Sistemas de Referencia de Coordenadas
# 
#
# OBJETIVOS:
# - Explorar CRS de un objeto sf
# - Asignar CRS 
# - Proyectar/reproyectar/transformar CRS

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(mapview, sf)

# 2. Descargar datos -----------------------------------------------------

## leer datos Galicia
galicia_sf <- read_sf("00_data/01-geospatial-basico/base.gpkg", layer = "galicia") 


# 3. CRS -----------------------------------------------------------------

## 3.1. Asignar CRS ---------------------------------

## NOTA: esto no debería pasar casi nunca

## CRS objeto?
st_crs(galicia_sf)

## Protocolo incorrecto: intuir CRS
galicia_incorrecto_sf <- st_set_crs(galicia_sf, "EPSG:25829")

## Visualizar
mapview(galicia_incorrecto_sf)

## Procolo correcto: preguntar a quién nos dio los datos
## - Está en EPSG:25829
galicia_correcto_sf <- st_set_crs(galicia_sf, "EPSG:25828")

## Visualizar
mapview(galicia_correcto_sf)

## 3.2. Transformar CRS ------------------------------

## transformar a EPSG:25829
galicia_transf_sf <- st_transform(galicia_correcto_sf, "EPSG:25829")

## explorar CRS
st_crs(galicia_transf_sf)
