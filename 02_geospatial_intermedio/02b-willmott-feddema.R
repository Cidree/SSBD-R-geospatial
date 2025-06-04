# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 03b - Willmott & Feddema Moisture Index
# 
#
# OBJETIVOS:
# - Álgebra de rásters
# - Calcular evapotranspiración potencial corregida (ETP)
# - Calcular Indice de Humedad de Willmott y Feddema
#
# RECURSOS:
# - https://portalrediam.cica.es/descargas/
# - https://www.worldclim.org/data/index.html
# - Willmott, C. & Feddema, J. (1992). A More Rational Climatic Moisture Index. The Professional Geographer, 44, 84-88.
# - Feddema, J. (2005). A revised Thornthwaite-type global climate classification. Physical Geography, 26, 6, pp. 442–466


# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(envirem, geodata, giscoR, sf, terra, tidyverse, tidyterra)

# 2. Cargar datos --------------------------------------------------------

## constantes
selected_crs  <- "EPSG:25830"
selected_ccaa <- "Andalucia"

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
# tmin_sr <- load_worldclim_ccaa(selected_ccaa, "tmin", "tmin", selected_crs)
# tmax_sr <- load_worldclim_ccaa(selected_ccaa, "tmax", "tmax", selected_crs)
# tmed_sr <- load_worldclim_ccaa(selected_ccaa, "tavg", "tmean", selected_crs)
# prec_sr <- load_worldclim_ccaa(selected_ccaa, "prec", "precip", selected_crs)

tmin_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmin/", "tmin")
tmax_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmax/", "tmax")
tmed_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/tmed/", "tmean")
prec_sr <- load_clima_andalucia("00_data/02-geospatial-intermedio/clima_andalucia/prec/", "precip")


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
pet_max <- global(pet_sr, "max", na.rm = TRUE) |> max() |> ceiling()
plot(pet_sr, col = hcl.colors(200, "Spectral", rev = TRUE), range = c(0, pet_max))

## 3.2. Calcular Moisture Index -------------------------

## Basado en Willmott & Feddema 1992
## - utiliza datos anuales
cmi_sr <- climaticMoistureIndex(
  annualPrecip = sum(prec_sr),
  PET          = sum(pet_sr)
)

## función para reclasificar
reclassify_cmi <- function(raster) {

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
cmi_class_sr <- reclassify_cmi(cmi_sr)
names(cmi_class_sr) <- "cmi"

## exportar
writeRaster(cmi_class_sr, "00_data/02-geospatial-intermedio/cmi_willmott_feddema_class.tiff", overwrite = TRUE)


# 4. Generar mapa --------------------------------------------------------

## 4.1. Indice de Humedad de Willmott & Feddema ----------------

## etiquetas
labels <- c("Árido", "Semiárido", "Seco", "Húmedo", "Muy húmedo", "Saturado")

## colores
cmi_colors <- grass.colors(6, "roygbiv", rev = TRUE)
names(cmi_colors) <- 1:6

## crear mapa
ggplot() +
    geom_spatraster(data = cmi_class_sr) +
    scale_fill_manual(
        values       = cmi_colors, 
        na.value     = NA, 
        na.translate = FALSE, 
        breaks       = names(cmi_colors),
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
        title = str_glue("Climatic Moisture Index en {selected_ccaa}, España")
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





## Gráfico de superficie de cada clase
cmi_sf <- cmi_class_sr |> 
    as.polygons() |> 
    st_as_sf()

## convertir a tabla preparada para graficar
cmi_tbl <- cmi_sf |> 
    mutate(area = st_area(geometry) |> units::set_units(km2) |> as.numeric()) |> 
    mutate(perc = area / sum(area) * 100) |> 
    mutate(perc_label = paste0(round(perc, 1), "%")) |> 
    st_drop_geometry() |> 
    mutate(
        class = case_match(
            as.character(cmi),
            "1" ~ "Árido",
            "2" ~ "Semiárido",
            "3" ~ "Seco",
            "4" ~ "Húmedo",
            "5" ~ "Muy húmedo",
            "6" ~ "Saturado"
        )
    )

## gráfico
cmi_tbl |> 
  ggplot(aes(x = fct_reorder(class, cmi), y = area)) +
  geom_col(width = 0.6, fill = "#2C0703") +
  geom_text(
    aes(label = perc_label),
    vjust = -0.5,
    size = 5,
    color = "black"
  ) +
  labs(
    title = str_glue("Superficie de cada tipo climático en {selected_ccaa}, España"),
    x = NULL,
    y = "Área (km²)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.title           = element_text(
        face   = "bold",
        size   = 22,
        hjust  = .5,
        margin = margin(b = 10, unit = "mm")
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
