# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# Ejercicio: 04 Operaciones con geometrías
# 
#
# OBJETIVOS:
# - Predicados espaciales
# - Filtros espaciales
# - Uniones espaciales
# - Transformaciones unarias
# - Transformaciones binarias

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(geodata, giscoR, sf, tidyverse)

# 2. Cargar datos --------------------------------------------------------

## 2.1. Ocurrencias de lobo -------------------

## descargar ocurrencias de lobo en el año 2024
## - Global Biodiversity Information Facility: https://www.gbif.org/
lobo_df <- geodata::sp_occurrence(
  genus   = "Canis",
  species = "lupus",
  args    = c("year=2024")
)

## explorar datos
glimpse(lobo_df)

## seleccionar columnas
lobo_sf <- lobo_df |> 
  as_tibble() |> 
  select(country, eventDate, lat, lon) |> 
  st_as_sf(
    coords = c("lon", "lat"),
    crs    = "EPSG:4326"
  ) 

## visualizar datos
mapview(lobo_sf)

## 2.2. CCAA y provincias España --------------

## CCAA
ccaa_sf <- gisco_get_nuts(country = "ES", nuts_level = 2, resolution = "01")

## Provincias
prov_sf <- gisco_get_nuts(country = "ES", nuts_level = 3, resolution = "01")

## 2.3. Parques nacionales ---------------------

ppnn_sf <- read_sf("00_data/01-geospatial-basico/base.gpkg", layer = "parques_nacionales")


# 3. Predicados ----------------------------------------------------------

## 3.1. Uso de funciones predicado ----------

## ocurrencias de lobo que intersectan con las provincias de Lugo y Ourense
## - filtrar Lugo de prov_sf
## - utilizar st_intersects() 
intersects_list <- prov_sf |> 
  filter(NAME_LATN %in% c("Lugo", "Ourense")) |> 
  st_intersects(lobo_sf)

## filtrar y visualizar lobos avistados en Ourense
lobo_ourense_sf <- lobo_sf |> 
  slice(intersects_list[[2]])

mapview(lobo_ourense_sf) +
  mapview(prov_sf, col.regions = "orange")

## 3.2. Filtros espaciales -----------------

## filtrar lobos en Ourense (mismo ejercicio)
st_filter(
  x = lobo_sf,
  y = prov_sf |> filter(NAME_LATN == "Ourense"),
  .predicate = st_disjoint
)

## EJERCICIO: filtrar provincias colindantes con Córdoba
## - Córdoba es una provincia de España


## 3.3. Uniones espaciales -----------------

## cuántas ocurrencias de lobo hubo en cada Parque Nacional de España?

### filtrar datos de lobo
lobo_spain_sf <- lobo_sf |> 
  # filter(country == "Spain") |> 
  st_filter(prov_sf)

### visualizar
mapview(lobo_spain_sf)

### unión espacial
st_join(
  x    = lobo_spain_sf,
  y    = ppnn_sf,
  join = st_intersects
) |> 
  count(Name)

## EJERCICIO: añadir columnas de provincia y CCAA a los parques nacionales
## - Ves algo raro al hacerlo?
ppnn_sf |> 
  st_join(prov_sf |> select(prov = NAME_LATN)) |> 
  st_join(ccaa_sf |> select(ccaa = NAME_LATN)) |> 
  relocate(geom, .after = 4)

# 4. Transformaciones unarias --------------------------------------------

## 4.1. Buffer -----------------

## buffer de 10km del Parque Nacional de Ordesa

## filtrar Ordesa
ordesa_sf <- ppnn_sf |> 
  filter(Name == "Parque Nacional de Ordesa y Monte Perdido") 

## aplicar buffer
ordesa_buffer_sf <- st_buffer(ordesa_sf, 10000)

## visualizar
mapview(ordesa_sf, layer.name = "Ordesa") +
  mapview(
    ordesa_buffer_sf,
    layer.name  = "Buffer",
    col.regions = "red"
  )


# 5. Transformaciones binarias -------------------------------------------

## 5.1. Diferencia ------------------

## Extraer solamente la superficie del buffer de Ordesa
st_difference(ordesa_buffer_sf, ordesa_sf) |> 
  mapview()

## 5.2. Intersección 1 ---------------

## st_intersection(puntos, poligonos) = st_filter(puntos, poligonos) |> st_join(poligonos)

## Ocurrencias de lobo dentro de la provincia de Lugo
st_intersection(
  lobo_sf, 
  prov_sf |> filter(NUTS_NAME == "Lugo")
)

st_filter(
  lobo_sf, 
  prov_sf |> filter(NUTS_NAME == "Lugo")
) |> 
  st_join(prov_sf |> filter(NUTS_NAME == "Lugo"))


## 5.3. Intersección 2 ---------------

## st_intersection(poligonos, poligonos2) != st_filter(poligonos, poligonos2) |> st_join(poligonos2)

## filtrar PPNN en provincia de Asturias
st_filter(
  ppnn_sf,
  prov_sf |> filter(NUTS_NAME == "Asturias")
) |> 
  mapview()

## intersección PPNN con provincia de Asturias
st_intersection(
  ppnn_sf,
  prov_sf |> filter(NUTS_NAME == "Asturias")
) |> 
  mapview()

## Eliminar los polígonos pequeños que aparecen en el mapa anterior
st_intersection(
  ppnn_sf,
  prov_sf |> filter(NUTS_NAME == "Asturias")
) |> 
  st_cast("POLYGON") |> 
  mutate(area = st_area(geom) |> units::set_units(ha) |> as.numeric()) |> 
  filter(area > 5) |> 
  mapview()
