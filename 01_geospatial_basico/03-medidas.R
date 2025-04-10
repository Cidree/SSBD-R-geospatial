# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# Ejercicio: 03 Medidas
# 
#
# OBJETIVOS:
# - Medir longitudes
# - Medir áreas
# - Medir distancias
# - Medir perímetros
# - Modificar unidades de medida

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(mapSpain, sf, tidyverse, units)

# 2. Cargar datos --------------------------------------------------------

## ríos de España
rivers_sf <- esp_get_rivers()

## cuencas hidrográficas
cuencas_sf <- esp_get_hydrobasin()

# 3. Medidas -------------------------------------------------------------

## 3.1. Longitud -----------------------

## Medir longitud de los ríos
st_length(rivers_sf)

## Añadir longitud como una columna
rivers_sf |> 
  mutate(longitud_m = st_length(geom))

## Añadir longitud como una columna (en km)
rivers_sf |> 
  mutate(longitud_m = st_length(geom) |> set_units(km))


## 3.2. Distancias ---------------------

## calcular distancia de los ríos a la cuenca del Tajo
cuenca_tajo_sf <- cuencas_sf |> 
  filter(rotulo == "TAJO")

rivers_sf |> 
  mutate(
    dist_tajo = st_distance(geom, cuenca_tajo_sf) |> set_units(km)
  )

## 3.3. Áreas y perímetros --------------

## área y perímetro de las cuencas
cuencas_sf |> 
  mutate(
    area = st_area(geom) |> set_units(ha),
    per  = st_perimeter(geom) |> set_units(km)
  )
