# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 01: Mapeado de la distribución de especies forestales en dos
# escenario de cambio climático
# 
#
# OBJETIVOS:
# - Descargar datos de EU-Trees4F
# - Crear mapas de distribución potencial en escenarios de cambio climático

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(forestdata, terra, tidyterra, tidyverse)

# 2. Cargar datos --------------------------------------------------------

## elegir especie
metadata_forestdata$eutrees4f_species
especie <- "Pinus pinaster"

## cargar datos "actuales" de 2005
especie_2005_sr <- fd_forest_eutrees4f(
  species  = especie,
  period   = 2005,
  type     = "bin",
  distrib  = "pot"
)

## Load Pinus sylvestris data for 2095 (rcp45)
especie_2095_rcp45_sr <- fd_forest_eutrees4f(
  species  = especie,
  period   = 2095,
  scenario = "rcp45"
)

## Load Pinus sylvestris data for 2095 (rcp85)
especie_2095_rcp85_sr <- fd_forest_eutrees4f(
  species  = especie,
  period   = 2095,
  scenario = "rcp85"
)


# 3. Preparar datos ------------------------------------------------------

## 3.1 Reclasificar --------------------------------

## crear una función para reclasificar cada capa
reclassify_rcp <- function(raster, classes) {
  
  ## reclasificar el valor 1 por el valor 2
  raster_class <- ifel(
    raster == 1, 2, raster
  )
  ## sumar a la distribucion actual
  raster_class <- as.factor(raster_class + especie_2005_sr)

  ## renombrar niveles
  levels(raster_class)[[1]][, 2] <- classes
  return(raster_class)
  
}

## clases y colores para cada clase
ps_classes <- c("Ausente", "Desaparecerá", "Nueva distribución",  "Presente y estable")
ps_colors  <- c("#BE92A2", "#96031A", "#6DA34D", "#CEEDDB")

## aplicar función
especie_rcp45_sr <- reclassify_rcp(especie_2095_rcp45_sr, ps_classes)
especie_rcp85_sr <- reclassify_rcp(especie_2095_rcp85_sr, ps_classes)

## unir en un solo raster
especie_stack_sr <- c(especie_rcp45_sr, especie_rcp85_sr)
names(especie_stack_sr) <- c("RCP 4.5", "RCP 8.5")

## 3.2. Visualización exploratoria -----------------

plot(especie_stack_sr, col = ps_colors)
plot(especie_rcp85_sr, col = ps_colors)


# 4. Visualización --------------------------------------------------------

ggplot() +
  geom_spatraster(
    data = especie_stack_sr
  ) +
  scale_fill_manual(
    values   = ps_colors,
    na.value = NA,
    na.translate = FALSE
  ) +
  facet_wrap(vars(lyr)) +
  labs(
    title = str_glue("Distribución de {especie} en dos escenarios de cambio climático"),
    caption = "Autor: Adrián Cidre | Fuente datos: EU-Trees4F"
  ) +
  guides(
    fill = guide_legend(
      title          = NULL,
      position       = "top",
      label.position = "top"
    )
  ) +
  theme_void(base_family = "Roboto", base_size = 16) +
  theme(
    text = element_text(color = "snow"),
    plot.title = element_text(
      face       = "bold", 
      hjust      = .5, 
      color      = "snow",
      lineheight = 1.2,
      margin     = margin(t = 5, l = 5, b = 10)
    ),
    plot.caption = element_text(hjust  = .5),
    plot.background = element_rect(
      fill   = "gray10",
      colour = NA
    ),
    legend.key.width     = unit(50, "mm"),
    legend.key.height    = unit(3, "mm"),
    legend.key.spacing.x = unit(0, "mm"),
    strip.text = element_text(
      face   = "bold",
      size   = 20,
      margin = margin(t = 5, unit = "mm")
    )
  )



