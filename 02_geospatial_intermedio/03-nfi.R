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

p_load(forestdata, leaflet, leafem, mapview, rsi, sf, stars, terra, tidyterra, tidyverse, units)

# 2. Cargar datos --------------------------------------------------------

## 2.1. Área que cubre el incendio -----------------

## definir puntos del polígono en una matriz
coords <- matrix(c(
  -7.607130373923812, 42.488892173687724,
  -7.508940066306624, 42.488892173687724,
  -7.508940066306624, 42.56555535474994,
  -7.607130373923812, 42.56555535474994,
  -7.607130373923812, 42.488892173687724
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

## fechas incendio: 05-09-2019 hasta 07-09-2019

## dar un nombre aleatorio en el directorio temporal
prefire_file <- tempfile(fileext = ".tiff")

## descargar imagen previa al incendio (~ 20-30 segundos)
get_landsat_imagery(
  aoi             = aoi_sf,
  start_date      = "2019-08-23",
  end_date        = "2019-08-25",
  output_filename = prefire_file,
  mask_band       = NULL
)

## leer imagen
prefire_sr <- rast(prefire_file)

## visualizar RGB
plotRGB(prefire_sr, 4, 3, 2)
plotRGB(prefire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3))


## 2.3. Imagen post-incendio -------------------------

## fechas incendio: 05-09-2019 hasta 07-09-2019

## dar un nombre aleatorio en el directorio temporal
postfire_file <- tempfile(fileext = ".tiff")

## descargar imagen posterior al incendio (~ 20-30 segundos)
get_landsat_imagery(
  aoi             = aoi_sf,
  start_date      = "2019-10-10",
  end_date        = "2019-10-12",
  output_filename = postfire_file,
  mask_band       = NULL
)

## leer imagen
postfire_sr <- rast(postfire_file)

## visualizar RGB
par(mfrow = c(1, 2), pad = rep(6, 4))
plotRGB(prefire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3), mar = .1, main = "24 de agosto de 2019")
plotRGB(postfire_sr, 4, 3, 2, stretch = "lin", zlim = c(0, .3), mar = .1, main = "11 de octubre de 2019")
dev.off()

# 3. Severidad -----------------------------------------------------------

## calcular NBR
nbr_pre_sr  <- (prefire_sr$N - prefire_sr$S2) / (prefire_sr$N + prefire_sr$S2)
nbr_post_sr <- (postfire_sr$N - postfire_sr$S2) / (postfire_sr$N + postfire_sr$S2)

## calcular dNBR
dnbr_sr <- nbr_pre_sr - nbr_post_sr
names(dnbr_sr) <- "dnbr"

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
levels(dnbr_class_sr)[[1]][,2] <- 
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
    title = "Severidad incendio Monforte de Lemos 2019",
    x     = "@2024 Autor: Adrián Cidre"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    legend.position = c(.75, .2),
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
  ext(c(616000, 620000, 4706500, ext(dnbr_burned_sr)[4]))
)

## visualizar
plot(dnbr_burned_crop_sr)

## suavizar nueva extensión
dnbr_burned_smooth_sr <- focal(
  x   = dnbr_burned_crop_sr, 
  w   = 5,
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
  slice_max(area, n = 1)

## 3.3. Superficie incendio ------------

## superficie total
perimeter_def_sf |> 
  st_area() |> 
  set_units(ha)

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
    zlim = c(0, .15)
  ) +
  geom_sf(
    data  = perimeter_def_sf,
    fill  = "transparent",
    color = "red",
    lwd   = 1
  ) +
  theme_void()

## 4.2. Mapa web --------------------

## convertir raster a stars (necesario para leaflet)
## además, necesita ser WGS84
postfire_stars <- postfire_sr |> 
  subset(c(2, 3, 4)) |> 
  project("EPSG:4326") |> 
  st_as_stars()

## mapa interactivo
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data      = st_transform(perimeter_def_sf, "EPSG:4326"),
    fillColor = "transparent",
    color     = "blue",
    weight    = 2,
    group     = "Perimetro"
  ) |>
  addStarsRGB(
    x         = postfire_stars,
    quantiles = c(0, .999),
    group     = "Landsat"
  ) |> 
  addLayersControl(
    overlayGroups = c("Perimetro", "Landsat"),
    options       = layersControlOptions(collapsed = FALSE)
  )

## exportar perímetro
write_sf(perimeter_sf, "00_data/02-geospatial-intermedio/perimetro.gpkg")


# 5. Uso del suelo quemado -----------------------------------------------

## descargar land cover
landcover_sr <- fd_landcover_copernicus(
  x     = perimeter_sf,
  year  = 2019,
  layer = "discrete",
  crop  = TRUE,
  mask  = TRUE
) |> as.factor()

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
