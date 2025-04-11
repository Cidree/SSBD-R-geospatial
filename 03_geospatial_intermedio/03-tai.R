# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 03 - Thornwaite Aridity Index
# 
#
# OBJETIVOS:
#
# RECURSOS:
# - https://portalrediam.cica.es/descargas

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(envirem, giscoR, sf, terra, tidyverse, tidyterra)

# 2. Cargar datos --------------------------------------------------------

## constantes
andalucia_crs <- "EPSG:25830"

## 2.1. Área de estudio -------------------

study_area_sf <- gisco_get_nuts(country = "ES", nuts_level = 2) |> 
    filter(NAME_LATN == "Andalucía") |> 
    st_transform(andalucia_crs)

## 2.2. Datos climáticos -----------------

## función para leer archivos, reproyectar, convertir a SpatRaster y renombrar capas
read_temp <- function(path, var) {
    ## listar rasters
    temp_files <- list.files(path, full.names = TRUE)
    ## leer como lista, proyectar y convertir a spatraster
    temp_sr <- map(temp_files, \(file) rast(file) |> project(andalucia_crs)) |> rast()
    ## renombrar archivos
    names(temp_sr) <- paste0(var, sprintf("_%02d", 1:12))
    ## devolver raster
    return(temp_sr)
}

## leer datos climáticos previamente descargados:
## - temperatura media mensual
## - temperatura máxima mensual
## - temperatura mínima mensual
## - precipitación total mensual
tmed_sr <- read_temp("00_data/02-geospatial-intermedio/tmed_rediam/", "tmean")
tmin_sr <- read_temp("00_data/02-geospatial-intermedio/tmin_rediam/", "tmin")
tmax_sr <- read_temp("00_data/02-geospatial-intermedio/tmax_rediam/", "tmax")
prec_sr <- read_temp("00_data/02-geospatial-intermedio/prec_rediam/", "precip")


# 3. Preparar datos ------------------------------------------------------

## 3.1. Evapotranspiración potencial (PET) ----------

## necesitamos:
## - raster de temperaturas medias mensuales
## - raster de radiación solar extraterrestre mensual
## - rango de temperaturas (diferencia entre tmax y tmin)

## obtener radiación solar extraterrestre (utiliza modelo de Laskar et al. 2004)
solrad_sr <- ETsolradRasters(tmed_sr[[1]], year = 2024)

## calcular rango de temperaturas
rango_temp_sr <- tmax_sr - tmin_sr

## calcular evapotranspiración potencial
pet_sr <- envirem::monthlyPET(
    Tmean = tmed_sr,
    RA    = solrad_sr,
    TD    = rango_temp_sr
)

## explorar valores
hist(pet_sr)

## explorar valores con ggplot2
as_tibble(pet_sr) |> 
    na.omit() |> 
    pivot_longer(cols = everything()) |> 
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(vars(name), scales = "free_y")

## 3.2. Calcular TAI -------------------------------

## calcular Indice de Aridez de Thornthwaite
tai_sr <- aridityIndexThornthwaite(
    precipStack = prec_sr,
    PETstack    = pet_sr
)

## reclasificar valores según Thornthwaite 1948
# reclassify_tai <- function(raster) {

#     ## matriz de reclasificación
#     mat <- as.matrix(
#         c(

#         )
#     )


# }


## guardar resultado
writeRaster(tai_sr, "00_data/02-geospatial-intermedio/tai.tiff", overwrite = TRUE)


# 4. Generar mapa --------------------------------------------------------

