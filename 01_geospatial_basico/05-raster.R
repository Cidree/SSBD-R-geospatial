# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# Ejercicio: 05 Datos Ráster
# 
#
# OBJETIVOS:
# - Leer/Exportar datos ráster
# - Descargar MDE
# - Explorar datos ráster
# - Generar ráster derivados del MDE
# - Cortar/Máscara
# - Operaciones aritméticas con ráster
# - Reclasificar ráster
# - Extraer valores de ráster

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(geodata, geoperu, mapview, sf, terra, tidyverse)

# 2. Cargar datos --------------------------------------------------------

## Cargar Parque Nacional del Manu (Perú)
manu_sf <- get_anp_peru(anp = "Manu")

## Cargar datos oso andino en el Manu
oso_andino_tbl <- sp_occurrence(
  genus   = "Tremarctos",
  species = "ornatus",
  ext     = ext(manu_sf)
)

## convertir a SF
oso_andino_sf <- st_as_sf(
  oso_andino_tbl,
  coords = c("lon", "lat"),
  crs    = "EPSG:4326"
) |> 
  select(geometry)

## descargar primer raster
## MDE de Perú
dem_peru_sr <- elevation_30s(country = "PE", path = tempdir())


# 3. Exploración raster --------------------------------------------------

## Explorar estructura
dem_peru_sr

## Visualizar con R base
plot(dem_peru_sr)
plot(dem_peru_sr, col = hcl.colors(1000, "Spectral") |> rev())

## Propiedades    
crs(dem_peru_sr, describe = TRUE)  # comprobar CRS  
ext(dem_peru_sr)                   # Rectángulo mínimo envolvente
st_bbox(dem_peru_sr)               # idem con paquete sf (!!! distinto orden)
values(dem_peru_sr) |> as_tibble() # valores del ráster
nlyr(dem_peru_sr)                  # número de capas/bandas
names(dem_peru_sr)                 # nombres de las capas
summary(dem_peru_sr)               # resumen estadístico de los valores
hist(dem_peru_sr)                  # histograma de los valores

## EXTRA: convertir entre SpatVector y sf
oso_andino_vect <- vect(oso_andino_sf)
oso_andino_2_sf <- st_as_sf(oso_andino_vect)

# 4. Preparar raster -----------------------------------------------------

## cortar a zona de estudio
dem_manu_nomask_sr <- crop(dem_peru_sr, manu_sf)
dem_manu_sr <- crop(dem_peru_sr, manu_sf, mask = TRUE)

## visualizar (sin máscara)
plot(dem_manu_nomask_sr)
plot(manu_sf$geom, border = "red", lwd = 2, add = TRUE)

## visualizar (con máscara)
plot(dem_manu_sr)

## transformar CRS (reproyectar)
dem_prep_sr <- project(dem_manu_sr, "EPSG:5389")
dem_prep_sr

# 4. Rasters del terreno -------------------------------------------------

## Raster de pendientes
pendiente_sr <- terrain(dem_prep_sr, v = "slope")
pendiente_radians_sr <- terrain(dem_prep_sr, v = "slope", unit = "radians")

## Raster de orientaciones
orientacion_sr <- terrain(dem_prep_sr, v = "aspect")
orientacion_radians_sr <- terrain(dem_prep_sr, v = "aspect", unit = "radians")

## Raster de sombras
hillshade_sr <- shade(pendiente_radians_sr, orientacion_radians_sr)

## Visualizar mapa de sombras con DEM
plot(hillshade_sr, col = grey(0:100/100), legend = FALSE)
plot(dem_prep_sr, col =  hcl.colors(200, "Spectral", alpha = 0.35, rev = TRUE), add = TRUE)

## curvas de nivel
cn_vect <- as.contour(dem_prep_sr)

## visualizar curvas de nivel
plot(dem_prep_sr, col =  hcl.colors(2000, "Spectral", rev = TRUE))
plot(cn_vect, add = TRUE)

## Crear Raster Multibanda
## - Mismo SRC
## - Misma extensión (rectángulo mínimo envolvente)
## - Misma resolución espacial
terrain_sr <- c(dem_prep_sr, pendiente_sr, orientacion_sr, hillshade_sr)

## Cambiar nombres
names(terrain_sr) <- c("Elevacion", "Pendiente", "Orientacion", "Sombras")

## Propiedades Raster Multibanda
crs(terrain_sr, describe = TRUE)
ext(terrain_sr)
values(terrain_sr) |> as_tibble()
nlyr(terrain_sr)
names(terrain_sr)
summary(terrain_sr)
hist(terrain_sr)
terrain_sr$Elevacion
terrain_sr[[1]]
terrain_sr[[1:2]]
terrain_sr[[c(1, 3)]]

# 5. Reclasificar --------------------------------------------------------

## matriz de reclasificación
mat <- matrix(
  c(
    0, 22.5, 1,
    22.5, 67.5, 2,
    67.5, 112.5, 4,
    112.5, 157.5, 6,
    157.5, 202.5, 8,
    202.5, 247.5, 7,
    247.5, 292.5, 5,
    292.5, 337.5, 3,
    337.5, 360, 1
  ),
  ncol = 3, 
  byrow = TRUE
)

## reclasificar
aspect_class_sr <- classify(terrain_sr$Orientacion, mat)
plot(aspect_class_sr)

## convertir a categórico
aspect_class_sr <- as.factor(aspect_class_sr)

## modificar etiquetas
levels(aspect_class_sr)[[1]][, 2] <- 
  c("N", "NE", "NW", "E", "W", "SE", "SW", "S")

## visualizar
plot(aspect_class_sr, main = "Orientación", col = hcl.colors(8, "Oslo", rev = TRUE))


# 5. Datos climáticos -----------------------------------------------------

# extraer
# modificar resolucion espacial

## 5.1. Descarga y preparación de datos ------------

## cargar datos de Worldclim
tmin_peru_sr <- worldclim_country(
  country = "PE",
  var     = "tmin",
  path    = tempdir()
)

## visualizar
plot(tmin_peru_sr)

## nombre de capas?
names(tmin_peru_sr)

## modificar a mejores nombres
names(tmin_peru_sr) <- paste0("tmin_", month.abb)

## unir al stack de rasters
c(terrain_sr, tmin_peru_sr)

## condiciones stack:
## - Mismo SRC
## - Misma extensión (rectángulo mínimo envolvente)
## - Misma resolución espacial
## -> TIP: resample() ajusta la extensión y la resolución espacial. El CRS no
tmin_peru_projected_sr <- project(tmin_peru_sr, crs(terrain_sr))
tmin_manu_sr <- resample(tmin_peru_projected_sr, terrain_sr)
plot(tmin_manu_sr)

## enmascarar
tmin_manu_prep_sr <- mask(
  tmin_manu_sr, 
  st_transform(manu_sf, crs(tmin_manu_sr))
)

plot(tmin_manu_prep_sr)

## stack de rasters
variables_manu_sr <- c(terrain_sr, tmin_manu_prep_sr)

## 5.2. Extraer valores -------------------

## extraer valores de las variables para las localizaciones
## del oso andino
variables_oso_tbl <- terra::extract(
  x = variables_manu_sr,
  y = st_transform(oso_andino_sf, crs(variables_manu_sr))
) |> 
  as_tibble() |> 
  na.omit()


## visualizar
plot(variables_manu_sr$Elevacion)
plot(st_transform(oso_andino_sf, crs(variables_manu_sr)), add = TRUE)


## EXTRA -------------

p_load(dlookr, ggspatial, tidyterra)

## resumen resultados
dlookr::diagnose_numeric(variables_oso_tbl)

## EXTRA: mapa
ggplot() +
  geom_spatraster(data = variables_manu_sr$Elevacion) +
  scale_fill_cross_blended_c() +
  geom_sf(data = manu_sf, fill = NA, color = "red", lwd = 1) +
  geom_sf(data = oso_andino_sf) +
  labs(
    title    = "Observaciones de Oso Andino",
    subtitle = "Parque Nacional del Manu",
    fill     = "Elevación"
  ) +
  theme_void() +
  annotation_north_arrow(location = "tr", pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) +
  annotation_scale(location = "tr", pad_x = unit(1, "cm"), pad_y = unit(3, "cm"))
