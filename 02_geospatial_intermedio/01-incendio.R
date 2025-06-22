# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 01 - Análisis de incendios forestales con R
# 
#
# OBJETIVOS:
# - Descarga de imágenes Landsat
# - Cálculo de la severidad de un incendio (dNBR)
# - Extracción perímetro incendio
# - Cálculo área quemada
# - Cálculo área quemada por clase de uso del suelo
#
# RECURSOS:
# - https://sentiwiki.copernicus.eu/web/sentinel-2
# - https://www.fs.usda.gov/rm/pubs_series/rmrs/gtr/rmrs_gtr164.pdf


# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(
  forestdata, # descarga de usos del suelo
  leaflet,    # mapa interactivo
  leafem,     # añadir raster a leaflet
  mapview,    # mapa interactivo rápido
  rsi,        # descarga de datos satelitales
  sf,         # manejo de datos vectoriales
  stars,      # otro formato de datos de ráster
  terra,      # manejo de datos ráster
  tidyterra,  # visualización de objetos del paquete terra con ggplot2
  tidyverse,  # manejo de datos en general
  units       # cambio de unidades
)

# 2. Cargar datos --------------------------------------------------------

## 2.1. Área que cubre el incendio -----------------

## definir puntos del polígono en una matriz
coords <- matrix(c(
  -7.420322661854423,42.46799455460519,
  -7.043354278065361,42.46799455460519,
  -7.043354278065361,42.69651706297985,
  -7.420322661854423,42.69651706297985,
  -7.420322661854423,42.46799455460519
), ncol = 2, byrow = TRUE)

## crear polígono
aoi_sf <- st_polygon(list(coords)) |> 
  st_sfc(crs = 4326) |> 
  st_as_sf() |> 
  rename(geom = x) |> 
  st_transform("EPSG:25829")

## visualizar
mapview(aoi_sf)

## 2.2. Imagen pre-incendio --------------------------

## fechas incendio: 15-07-2022 hasta ~30-07-2022

## dar un nombre aleatorio en el directorio temporal
prefire_file <- tempfile(fileext = ".tiff")

## descargar imagen previa al incendio (~ 70 segundos)
get_sentinel2_imagery(
  aoi             = aoi_sf,
  start_date      = "2022-07-08",
  end_date        = "2022-07-09",
  output_filename = prefire_file,
  mask_band       = NULL
)

## leer imagen y convertir a reflectancia
prefire_sr <- rast(prefire_file) / 10000

## visualizar RGB
plotRGB(prefire_sr, 4, 3, 2)
plotRGB(prefire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3))


## 2.3. Imagen post-incendio -------------------------

## fechas incendio: 15-07-2022 hasta ~30-07-2022

## dar un nombre aleatorio en el directorio temporal
postfire_file <- tempfile(fileext = ".tiff")

## descargar imagen posterior al incendio (~ 70 segundos)
get_sentinel2_imagery(
  aoi             = aoi_sf,
  start_date      = "2022-08-01",
  end_date        = "2022-08-03",
  output_filename = postfire_file,
  mask_band       = NULL
)

## leer imagen
postfire_sr <- rast(postfire_file) / 10000

## visualizar RGB
par(mfrow = c(1, 2))
plotRGB(prefire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3), mar = .1, main = "8 de julio de 2022")
plotRGB(postfire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3), mar = .1, main = "2 de agosto de 2022")
dev.off()

# 3. Severidad -----------------------------------------------------------

## calcular NBR
nbr_pre_sr  <- (prefire_sr$N - prefire_sr$S2) / (prefire_sr$N + prefire_sr$S2)
nbr_post_sr <- (postfire_sr$N - postfire_sr$S2) / (postfire_sr$N + postfire_sr$S2)

## calcular dNBR
dnbr_sr <- nbr_pre_sr - nbr_post_sr
names(dnbr_sr) <- "dnbr"

## visualizar
plot(dnbr_sr)

## clasificar según clasificación Firemon
mat <- matrix(c(
  -Inf, .099, 1,
  .099, .269, 2,
  .269, .439, 3,
  .439, .659, 4,
  .659, Inf, 5
), byrow = TRUE, ncol = 3)

## reclasificar y convertir a factor
dnbr_class_sr <- classify(
  x   = dnbr_sr,
  rcl = mat
) |> 
  as.factor()

## modificar etiquetas
levels(dnbr_class_sr)[[1]][, 2] <- 
  c("No quemado", "Severidad baja", "Severidad moderada-baja",
    "Severidad moderada-alta", "Severidad alta")

## visualizar
plot(dnbr_class_sr)

## crear mapa con ggplot2
ggplot() +
  geom_spatraster(data = dnbr_class_sr) +
  scale_fill_whitebox_d(
    palette = "bl_yl_rd",
    name   = "Severidad"
  ) +
  labs(
    title = "Severidad incendio Courel 2022",
    x     = "@2024 Autor: Adrián Cidre"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    legend.position = c(.8, .8),
    legend.box.background = element_rect(fill = "white", colour = "grey30"),    
    plot.title = element_text(size = 20, face = "bold", hjust = .5)
  ) 


# 4. Perímetro -----------------------------------------------------------

## 4.1. Definir umbral -------------------

## probar umbrales
dnbr_burned_sr <- ifel(dnbr_sr < .27, 0, 1)

## visualizar
plot(dnbr_burned_sr)

## 4.2. Extraer perímetro ---------------

## suavizar
dnbr_burned_smooth_sr <- focal(
  x   = dnbr_burned_sr, 
  w   = 7,
  fun = "modal"
)

## visualizar
plot(dnbr_burned_smooth_sr)

## extraer bounding box
dnbr_burned_crop_sr <- crop(
  dnbr_burned_sr, 
  ext(c(635000, 655500, 4705000, 4726000))
)

## visualizar
plot(dnbr_burned_crop_sr)

## suavizar nueva extensión
dnbr_burned_smooth_sr <- focal(
  x   = dnbr_burned_crop_sr, 
  w   = 11,
  fun = "modal"
)

## renombrar capa
names(dnbr_burned_smooth_sr) <- "dnbr"

## visualizar
plot(dnbr_burned_smooth_sr)

## convertir a polígono
perimeter_sf <- as.polygons(dnbr_burned_smooth_sr) |> 
  st_as_sf() |> 
  filter(dnbr == 1) 

## extraer solamente el polígono grande
perimeter_def_sf <- perimeter_sf |> 
  st_cast("POLYGON") |> 
  mutate(area = st_area(geometry)) |> 
  filter(area >= set_units(30, ha))

## 3.3. Superficie incendio ------------

## superficie total
perimeter_def_sf |> 
  st_area() |> 
  set_units(ha) |> 
  sum()

## superficie según severidad
### extraer polígonos
dnbr_sf <- crop(dnbr_class_sr, perimeter_def_sf, mask = TRUE) |> 
  as.polygons() |> 
  st_as_sf()

### calcular área
dnbr_sf |> 
  mutate(area = st_area(geometry) |> set_units(ha)) |> 
  filter(dnbr != "No quemado") |> 
  mutate(dnbr = factor(
    dnbr, 
    levels =  c(
      "Severidad baja", "Severidad moderada-baja",
      "Severidad moderada-alta", "Severidad alta")
  )) |> 
  ggplot(aes(dnbr, area)) +
  geom_col(fill = "#890620", width = .6) +
  labs(
    x = NULL,
    y = "Superficie"
  ) +
  theme_bw()

# 4. Mapa incendio -------------------------------------------------------

## 4.1. Mapa perímetro --------------

ggplot() +
  geom_spatraster_rgb(
    data = postfire_sr,
    r = 4,
    g = 3,
    b = 2,
    stretch = "lin",
    zlim = c(0.1, .3)
  ) +
  geom_sf(
    data  = perimeter_def_sf,
    fill  = "transparent",
    color = "yellow",
    lwd   = .3
  ) +
  theme_void()

## 4.2. Mapa web --------------------

## convertir raster a stars (necesario para leaflet)
## además, necesita ser WGS84
postfire_stars <- postfire_sr |> 
  subset(c(2, 3, 4)) |> 
  aggregate(fact = 5) |> 
  project("EPSG:4326") |> 
  st_as_stars()

## mapa interactivo
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data      = st_transform(perimeter_def_sf, "EPSG:4326"),
    fillColor = "transparent",
    color     = "yellow",
    weight    = 2,
    group     = "Perimetro"
  ) |>
  addStarsRGB(
    x         = postfire_stars,
    quantiles = c(0, .999),
    group     = "Sentinel"
  ) |> 
  addLayersControl(
    overlayGroups = c("Perimetro", "Sentinel"),
    options       = layersControlOptions(collapsed = FALSE)
  )

## exportar perímetro
write_sf(perimeter_def_sf, "00_data/02-geospatial-intermedio/perimetro.gpkg")

## 4.3. Mapa severidad ------------------

## cortar dnbr
dnbr_class_crop_sr <- crop(
  dnbr_class_sr, 
   ext(c(635000, 655500, 4705000, 4726000))
)

## suavizar
dnbr_class_smooth_sr <- focal(
  x   = dnbr_class_crop_sr, 
  w   = 11,
  fun = "modal"
)

## enmascarar perímetro
dnbr_class_smooth_sr <- terra::mask(
  dnbr_class_smooth_sr, 
  perimeter_def_sf, 
  updatevalue = 1
)

## mapa severidad con nuevo perímetro
ggplot() +
  geom_spatraster(data = dnbr_class_smooth_sr) +
  scale_fill_whitebox_d(
    palette = "bl_yl_rd",
    name   = ""
  ) +
  labs(
    title = "Severidad incendio Courel 2022",
    x     = "@2024 Autor: Adrián Cidre"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    legend.position = "top",   
    plot.title = element_text(size = 20, face = "bold", hjust = .5),
    legend.text = element_text(size = 8)
  ) 

# 5. Uso del suelo quemado -----------------------------------------------

## descargar land cover
landcover_sr <- fd_landcover_copernicus(
  x     = perimeter_def_sf,
  year  = 2019,
  layer = "discrete",
  crop  = TRUE,
  mask  = TRUE
) |> as.factor()

## visualizar
plot(landcover_sr)

## códigos de nuestro raster?
landcover_codes_tbl <- metadata_forestdata$landcover_copernicus$discrete |> 
  filter(Code %in% levels(landcover_sr)[[1]][, 2]) 

## modificar etiquetas y nombre de la capa
levels(landcover_sr)[[1]][, 2] <- landcover_codes_tbl$Class
set.names(landcover_sr, "class")

## visualizar
plot(landcover_sr)

## convertir a polígonos y calcular área
landcover_sf <- landcover_sr |> 
  as.polygons() |> 
  st_as_sf() |> 
  mutate(area = st_area(geometry) |> set_units(ha)) 

## visualizar área quemada por clase
landcover_sf |> 
  mutate(class = fct_reorder(class, area)) |> 
  ggplot(aes(area, class)) +
  geom_col(fill = "#890620", width = .6) +
  labs(
    x = "Superficie",
    y = NULL
  ) +
  theme_minimal()
