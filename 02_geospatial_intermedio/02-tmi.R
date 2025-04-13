# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 03 - Thornwaite Aridity Index
# 
#
# OBJETIVOS:
# - Álgebra de rásters
# - Calcular evapotranspiración potencial corregida (ETP)
# - Calcular Indice de Aridez de Thornthwaite
# - Calcular Indice de Humedad de Thornthwaite
# - Calcular 
#
# RECURSOS:
# - https://portalrediam.cica.es/descargas/
# - https://www.worldclim.org/data/index.html
# - J. Laskar et al., A long-term numerical solution for the insolation quantities of the Earth, Astron. Astroph., 428, 261-285 2004
# - Thornthwaite, C.W. (1948). An approach toward a rational classification of climate. Geographical Review, 38, 55-94.



# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(envirem, geodata, giscoR, sf, terra, tidyverse, tidyterra)

# 2. Cargar datos --------------------------------------------------------

## constantes
selected_crs  <- "EPSG:25829"
selected_ccaa <- "Galicia"

## 2.2. Datos climáticos -----------------

## función para descargar datos, reproyectar, y renombrar capas
load_worldclim_ccaa <- function(ccaa, var, id, crs) {

    ## área de estudio
    study_area_sf <- gisco_get_nuts(country = "Spain", nuts_level = 2, resolution = "01") |> 
        filter(NAME_LATN == ccaa) |> 
        st_transform(crs)
  
    ## descargar datos de worldclim
    wc_sr <- worldclim_country("Spain", res = .5, var = var, path = tempdir()) |> 
      project(crs) |> 
      crop(study_area_sf, mask = TRUE)
  
    ## renombrar archivos
    names(wc_sr) <- paste0(id, sprintf("_%02d", 1:12))
    return(wc_sr)
    
}

## (Andalucía) función para leer archivos, reproyectar, convertir a SpatRaster y renombrar capas
load_clima_andalucia <- function(path, id) {
    ## listar rasters
    temp_files <- list.files(path, full.names = TRUE)
    ## leer como lista, proyectar y convertir a spatraster
    temp_sr <- map(temp_files, \(file) rast(file) |> project("EPSG:25830")) |> rast()
    ## renombrar archivos
    names(temp_sr) <- paste0(id, sprintf("_%02d", 1:12))
    ## devolver raster
    return(temp_sr)
}

## leer datos climáticos previamente descargados:
## - temperatura media mensual
## - temperatura máxima mensual
## - temperatura mínima mensual
## - precipitación total mensual
tmin_sr <- load_worldclim_ccaa(selected_ccaa, "tmin", "tmin", selected_crs)
tmax_sr <- load_worldclim_ccaa(selected_ccaa, "tmax", "tmax", selected_crs)
tmed_sr <- load_worldclim_ccaa(selected_ccaa, "tavg", "tmean", selected_crs)
prec_sr <- load_worldclim_ccaa(selected_ccaa, "prec", "precip", selected_crs)

# tmin_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmin/", "tmin")
# tmax_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmax/", "tmax")
# tmed_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmed/", "tmean")
# prec_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/prec/", "precip")


# 3. Preparar datos ------------------------------------------------------

## 3.1. Evapotranspiración potencial (PET) ----------

## necesitamos:
## - raster de temperaturas medias mensuales
## - raster de radiación solar extraterrestre mensual
## - rango de temperaturas (diferencia entre tmax y tmin)

## obtener radiación solar extraterrestre (utiliza modelo de Laskar et al. 2004)
solrad_sr <- ETsolradRasters(tmed_sr[[1]], year = 40)

## calcular rango de temperaturas
rango_temp_sr <- tmax_sr - tmin_sr

## calcular evapotranspiración potencial
pet_sr <- envirem::monthlyPET(
    Tmean = tmed_sr,
    RA    = solrad_sr,
    TD    = rango_temp_sr
)

## visualizar
## - con range modificamos los valores que se visualizan (comparable entre distintos meses)
plot(pet_sr, col = hcl.colors(200, "Spectral", rev = TRUE))

## visualizar 2.0
## - con range modificamos los valores que se visualizan (comparable entre distintos meses)
pet_max <- global(pet_sr, "max", na.rm = TRUE) |> max() |> ceiling()
plot(pet_sr, col = hcl.colors(200, "Spectral", rev = TRUE), range = c(0, pet_max))

## explorar valores
hist(pet_sr)

## 3.2. Calcular Moisture Index -------------------------

## Basado en Thornthwaite (1948)

## Indice de Humedad Global (Im): índice de humedad global del entorno
## Im = Ih - 0.6 * Ia

## Indice de Humedad estacional (Ih): se use en climas secos para identificar y cuantificar la 
## severidad de las condiciones de humedad
## Ih = (D / PET) * 100; donde D es el déficit de agua anual

## Indice de Aridez estacional (Ia): se use en climas húmedos para identificar y cuantificar la 
## severidad de las condiciones de sequía
## Ia = (S / PET) * 100; donde S es el exceso de agua anual (surplus)

## identificar déficit de precipitación (precipitación < evapotranspiración)
is_deficit_sr <- prec_sr < pet_sr

## calcular déficit como evapostranspiración - precipitación
total_deficit_sr <- ifel(is_deficit_sr, pet_sr - prec_sr, 0) |> sum()

## calcular excesos como precipitación - evapostranspiración
total_surplus_sr <- ifel(!is_deficit_sr, prec_sr - pet_sr, 0) |> sum()

## total de evapotranspiración anual
total_pet_sr <- sum(pet_sr)

## calcular indice de aridez (Ia)
## - Ia = (S / PET) * 100
indice_aridez_sr <- total_deficit_sr / total_pet_sr * 100

## calcular indice de humedad (Ih)
## - Ih = (D / PET) * 100
indice_humedad_sr <- total_surplus_sr / total_pet_sr * 100

## calcular indice climático de Thornthwaite
## - Im = Ih - 0.6 * Ia
indice_thornthwaite_sr <- indice_humedad_sr - (.6 * indice_aridez_sr)

## función para reclasificar
reclassify_index_thornthwaite <- function(raster) {
    ## matriz de reclasificación
    mat <- matrix(
      c(
          -Inf, -40, 1,
          -40, -20, 2,
          -20, 0, 3,
          0, 20, 4,
          20, 40, 5,
          40, 60, 6,
          60, 80, 7,
          80, 100, 8,
          100, Inf, 9
      ),
      ncol  = 3, 
      byrow = TRUE
    )
    ## reclasificar
    raster_class <- classify(raster, mat) |> as.factor()
    return(raster_class)
}

## aplicar reclasificación
indice_thornthwaite_class_sr <- reclassify_index_thornthwaite(indice_thornthwaite_sr)


## 3.3. Indice de Humedad (Feddema, 2005) ------------

## calcular Indice Climático de Humedad
## - Clasificación Feddema 2005
indice_feddema_sr <- envirem::climaticMoistureIndex(sum(prec_sr), sum(pet_sr))

## función para reclasificar
reclassify_cmi_feddema <- function(raster) {

    ## matriz de reclasificación
    mat <- matrix(
        c(
            -1, -.66, 1,
            -.66, -.33, 2,
            -.33, 0, 3,
            0, .33, 4,
            .33, .66, 5,
            .66, 1, 6
        ),
        ncol  = 3, 
        byrow = TRUE
    )

    ## reclasificar
    raster_class <- classify(raster, mat) |> as.factor()
    return(raster_class)

}

## aplicar reclasificación
indice_feddema_class_sr <- reclassify_cmi_feddema(indice_feddema_sr)


## 3.4. Guardar rasters ------------------------------

## guardar resultado
writeRaster(indice_thornthwaite_class_sr, "00_data/02-geospatial-intermedio/indice_thornthwaite_class.tiff", overwrite = TRUE)
writeRaster(indice_feddema_class_sr, "00_data/02-geospatial-intermedio/indice_feddema_class.tiff", overwrite = TRUE)


# 4. Generar mapa --------------------------------------------------------

## 4.1. Indice de Humedad de Thornthwaite ----------------

## etiquetas
labels <- c(
    "Árido", "Semiárido", "Sub-Húmedo\nseco", "Sub-Húmedo\nhúmedo", "Ligeramente\nhúmedo",
    "Moderadamente\nhúmedo", "Húmedo", "Muy húmedo", "Excesivamente\nhúmedo"
  )

## colores
thornthwaite_colors <- grass.colors(9, "roygbiv", rev = TRUE)
names(thornthwaite_colors) <- 1:9

## crear mapa
ggplot() +
    geom_spatraster(data = indice_thornthwaite_class_sr) +
    scale_fill_manual(
        values       = thornthwaite_colors, 
        na.value     = NA, 
        na.translate = FALSE, 
        breaks       = names(thornthwaite_colors),
        labels       = labels
    ) +
    guides(
        fill = guide_legend(
            title = NULL,
            nrow  = 1,
            label.position = "bottom",
            position = "top"
        )
    ) +
    labs(
        title = str_glue("Thornthwaite Moisture Index en {selected_ccaa}, España")
    ) +
    theme_void() +
    theme(
        legend.key.width     = unit(30, "mm"),
        legend.key.height    = unit(3, "mm"),
        legend.key.spacing.x = unit(0, "mm"),
        plot.background      = element_rect(fill = "gray20", colour = NA),
        plot.margin          = margin(1, 3, 1, 3, "cm"),
        text                 = element_text(color = "gray90"),
        plot.title           = element_text(
            face   = "bold",
            size   = 22,
            hjust  = .5,
            margin = margin(b = 10, unit = "mm")
        )
    )
