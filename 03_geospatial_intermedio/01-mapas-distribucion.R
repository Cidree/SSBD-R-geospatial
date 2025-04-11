# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Introducción al análisis geoespacial con R - Parte 2
# Ejercicio 01: Mapeado de la distribución de especies forestales en dos
# escenario de cambio climático
# 
#
# OBJETIVOS:

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(forestdata, ggtext, patchwork, terra, tidyterra, tidyverse)

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

## 3.2. Visualización exploratoria -----------------

plot(especie_rcp45_sr, col = ps_colors)
plot(especie_rcp85_sr, col = ps_colors)


# 4. Visualization --------------------------------------------------------

## Create a helper function
map_rcp <- function(data, title = "(a) RCP 4.5") {
  
  ggplot() +
    geom_spatraster(
      data = data,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values   = ps_colors,
      na.value = NA
    ) +
    labs(
      title = title
    ) +
    theme_void(base_family = "Roboto") +
    theme(
      plot.title = element_text(
        face = "bold", hjust = .5, color = "snow"
      )
    )
  
}

## Create maps
rcp45_gg <- map_rcp(especie_rcp45_sr)
rcp85_gg <- map_rcp(especie_rcp85_sr, title = "(b) RCP 8.5")

## Wrappers for {ggtext}
absent_txt   <- str_glue("<b style = 'color: {ps_colors[1]};'>almost absent</b>")
decrease_txt <- str_glue("<b style = 'color: {ps_colors[2]};'>decrease</b>")
increase_txt <- str_glue("<b style = 'color: {ps_colors[3]};'>shift</b>")
present_txt  <- str_glue("<b style = 'color: {ps_colors[4]};'>present</b>")

## Plot title
title_txt <- str_glue(
  "*Pinus sylvestris*, {absent_txt} in southern Europe and {present_txt} in Northern and Central Europe, is<br>
  projected to {increase_txt} its potential distribution northward and {decrease_txt} in Central Europe under two<br>
  climatic scenarios by 2095"
)

## Final maps
ps_gg <- rcp45_gg + 
  rcp85_gg +
  plot_annotation(
    title   = title_txt,
    caption = "Author: Adrián Cidre | Data source: EU-Trees4F",
    theme   = theme(
      plot.title = element_markdown(
        family     = "Merriweather",
        face       = "bold",
        lineheight = 1.2,
        margin     = margin(t = 5, l = 5, b = 10)
      ),
      plot.caption = element_text(
        hjust  = .5,
        family = "Roboto"
      ),
      plot.background = element_rect(
        fill = "gray10",
        colour = "gray10"
      )
    )
  ) 

## Export
ggsave(
  filename = "005_pinus_sylvestris_rcp_map/especie_rcp.png",
  plot     = ps_gg,
  width    = 25,
  height   = 20,
  units    = "cm"
)