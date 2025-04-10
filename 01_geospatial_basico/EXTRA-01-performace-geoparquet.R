# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 1
# EXTRA: 01 Comparación formatos
# 
#

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(arrow, geoarrow, sf, tictoc, tidyverse)


# 2. Crear datos ---------------------------------------------------------

## random word generator
random_word <- function(length = 5) {
    paste0(sample(letters, length, replace = TRUE), collapse = "")
}

## crear datos aleatorios
size <- 1e7
test_tbl <- tibble(
    a = sample(1:1000000, size = size, replace = TRUE),
    b = sample(1:1000000, size = size, replace = TRUE),
    c = sample(1:1000000, size = size, replace = TRUE),
    d = sample(1:1000000, size = size, replace = TRUE),
    g = sample(1:1000000, size = size, replace = TRUE),
    h = sample(1:1000000, size = size, replace = TRUE),
    i = sample(1:1000000, size = size, replace = TRUE),
    j = sample(1:1000000, size = size, replace = TRUE),
    k = sample(1:1000000, size = size, replace = TRUE),
    l = sample(1:1000000, size = size, replace = TRUE),
    m = sample(replicate(10, random_word(7)), size = size, replace = TRUE),
    n = sample(replicate(10, random_word(9)), size = size, replace = TRUE),
    o = sample(replicate(10, random_word(4)), size = size, replace = TRUE),
    e = sample(seq(40, 42, .001), size = size, replace = TRUE),
    f = sample(seq(-7, -10, -.001), size = size, replace = TRUE)
)

## convertir a espacial
test_sf <- st_as_sf(
    test_tbl,
    coords = c("e", "f"),
    crs    = "EPSG:4326"
) 

# 3. Performance test ----------------------------------------------------

## crear directorio para guardar archivos
dir.create("performance-test")

tic()
write_sf(test_sf |> head(500), "performance-test/test.shp")
toc()

tic()
write_sf(test_sf, "performance-test/test.gpkg")
toc()

tic()
write_parquet(test_sf, "performance-test/test.parquet")
toc()

