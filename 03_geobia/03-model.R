# SUMMER SCHOOL - BOSQUE DIGITAL - UNIVERSIDAD DE CÓRDOBA
#
# Clase: Clasificación de usos del suelo en R (GEOBIA)
# 
#
# OBJETIVOS:
# - Generar modelo con {tidymodels}
# - Evaluar modelo
# - Aplicar modelo

# 1. Cargar paquetes -----------------------------------------------------

library(pacman)

p_load(corrplot, kknn, sf, ranger, terra, tidymodels, tidyverse, vip)

# 2. Cargar datos --------------------------------------------------------

## cargar datos para crear el modelo
annotated_data_sf <- read_sf("00_data/03-geobia/training-points-stats.gpkg")

## preparar datos para modelo
annotated_data_tbl <- annotated_data_sf |> 
  st_drop_geometry() |> 
  distinct() |> 
  mutate(class = as.factor(class))

## número de observaciones por clase?
count(annotated_data_tbl, class)

## valores ausentes?
colSums(is.na(annotated_data_tbl))

# 3. Modelado ------------------------------------------------------------

## 3.1. Data split ---------------------

## separar en entrenamiento y prueba
set.seed(137)
splits    <- initial_split(annotated_data_tbl, prop = .8, strata = class)
train_tbl <- training(splits)
test_tbl  <- testing(splits)

## 3.2. Feature engineering -----------

## crear receta de preprocesado
## - step_zv(): elimina variables con 0 varianza 
## - step_lincomb(): elimina variables duplicadas
## - step_normalize(): normaliza los datos a media = 0; std = 1
## - step_rm(): elimina variables
base_rec <- recipe(class ~ ., data = train_tbl) |> 
  step_zv(all_predictors()) |>  
  step_lincomb(all_predictors())

## ver datos procesados
base_rec |> 
  prep() |> 
  juice()

## crear receta para KNN
norm_rec <- recipe(class ~ ., data = train_tbl) |> 
  step_zv(all_predictors()) |>  
  step_lincomb(all_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_rm(starts_with("max"), median.R, median.G, stdev.G, stdev.B, min.R, min.G)

## visualizar correlación para actualizar step_select()
norm_rec |> 
  prep() |> 
  juice() |> 
  select(-class) |> 
  cor() |> 
  corrplot("number")

## 3.3. Model specifications -----------------

## Random forest
rf_spec <-
  rand_forest(
    mode  = "classification",
    trees = 500,
    mtry  = tune(),
    min_n = tune()
  ) |> 
  set_engine(
    "ranger",
    importance = "permutation"
  )

## K-Nearest Neighbor
knn_spec <- nearest_neighbor(
  mode = "classification",
  neighbors = tune()
)

## 3.4. Workflows ------------------------------

## crear workflow con los algoritmos + recetas
model_wflw <- workflow_set(
  preproc = list(
    base  = base_rec,
    norm  = norm_rec
  ),
  models  = list(
    ranger = rf_spec,
    svm    = knn_spec
  ),
  cross = FALSE
)


## 3.5. Hyperparameter tuning -----------------

## Resampling - 5 Cross-Validation
set.seed(137)
folds <- vfold_cv(train_tbl, strata = class, v = 5)

## parámetros de control de la grilla
grid_ctrl <- control_grid(
  verbose       = TRUE,
  save_pred     = TRUE,
  save_workflow = TRUE
)

## tunear modelos
grid_results <- workflow_map(
  model_wflw,
  fn        = "tune_grid",
  seed      = 137,
  resamples = folds,
  grid      = 20, 
  control   = grid_ctrl,
  metrics   = metric_set(accuracy, f_meas)
)

## 3.6. Evaluate ---------------------------

## mejor modelo?
best_model <- grid_results |> 
  collect_metrics() |> 
  filter(.metric == "f_meas") |> 
  slice_max(mean, n = 1) |> 
  pull(wflow_id)

## cómo responde el modelo a los distintos valores de los hiperparámetros?
autoplot(grid_results, id = "base_ranger", metric = "f_meas")

## mejores hiperparámetros?
best_parameters <- grid_results |> 
  extract_workflow_set_result(best_model) |> 
  select_best(metric = "f_meas")

## finalizar workflow
## - extraemos el mejor modelo del workflow inicial
## - finalizamos el mejor workflow con los mejores hiperparámetros
best_wflow <- grid_results |> 
    extract_workflow(best_model) |> 
    finalize_workflow(best_parameters)

## último ajuste con mejores hiperparámetros
## - se ajusta el modelo con los mejores hiperparámetros en los datos de entrenamiento
## - se evalúa en los datos de prueba
set.seed(137)
test_res <- last_fit(
  best_wflow,
  split   = splits,
  metrics = metric_set(accuracy, f_meas)
)

## metricas de error en datos de prueba
test_res |> 
  collect_metrics()

## extraer workflow
test_wflow <- test_res |> 
  extract_workflow()

## obtener matriz de confusión
confmat_test <- test_wflow  |> 
  augment(test_tbl) |> 
  conf_mat(class, .pred_class)

## visualizar matriz de confusión
autoplot(confmat_test, "heatmap")

## métricas a partir de la matriz de confusión
summary(confmat_test)

## importancia de las variables
test_res |> 
  extract_fit_parsnip() |> 
  vip(geom = "point")


## 3.7. Export model -----------

write_rds(test_wflow, "00_data/03-geobia/best_ranger.rds")


