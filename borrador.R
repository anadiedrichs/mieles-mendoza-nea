library(tidyverse)
library(DataExplorer)

library(readxl)
mieles_mza_nea <- read_excel("data/mieles-mza-nea.xls", 
                             n_max = 205)
#View(mieles_mza_nea)

dataset <- 
  mieles_mza_nea %>%  
  select(-starts_with("Columna")) %>% 
  rename( "Pb" = "208 Pb")

# colnames(dataset)[17] <- "Pb"


# carga libreria
library(tidymodels)
library(themis)
# train y test datasets 
set.seed(123)
split <- initial_split(dataset, strata = Origin) # 75% train 25% test
train <- training(split)
test <- testing(split)


# cross validation 

set.seed(123)
folds <- vfold_cv(train, strata = Origin, v= 5) # 5 folds
folds

# receta 

receta <- recipe(Origin ~ ., data = train) %>%
   step_smote(Origin, over_ratio= 1) 

rf_spec <- rand_forest(trees = 500) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec

wf <- workflow() %>%
  add_recipe(receta)

metrics <- metric_set(kap, accuracy, sensitivity, specificity)

rf_rs <- wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

rf_rs


collect_metrics(rf_rs)

matriz <- rf_rs %>%
  conf_mat_resampled()


matriz %>% filter(Prediction == "Noreste")


final <- wf %>%
  add_model(rf_spec) %>%
  last_fit(split)

final

collect_metrics(final)

#####

receta_sin_smote <- recipe(Origin ~ ., data = train) 

wf_sin_smote <- workflow() %>%
  add_recipe(receta_sin_smote)


rf_sin_smote <- wf_sin_smote %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

rf_sin_smote


collect_metrics(rf_sin_smote)

matriz <- rf_sin_smote %>%
  conf_mat_resampled()


matriz %>% filter(Prediction == "Noreste")


final <- wf_sin_smote %>%
  add_model(rf_sin_smote) %>%
  last_fit(split)

final

collect_metrics(final)

# Con transformación PCA

receta_pca <- recipe(Origin ~ ., data = train) %>% 
  step_umap(all_numeric(),-all_outcomes()) %>% 
  step_smote() 

wf_pca <- workflow() %>%
  add_recipe(receta_pca)


rf_pca <- wf_pca %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = folds,
    metrics = metrics,
    control = control_resamples(save_pred = TRUE)
  )

rf_pca


collect_metrics(rf_pca)

matriz <- rf_pca %>%
  conf_mat_resampled()


matriz %>% filter(Prediction == "Noreste")


final <- wf_pca %>%
  add_model(rf_pca) %>%
  last_fit(split)

final

collect_metrics(final)
# Con transformación UMAP 

# viendo prep() y bake()
# https://stackoverflow.com/questions/62189885/what-is-the-difference-among-prep-bake-juice-in-the-r-package-recipes

receta <- recipe(Origin ~ ., data = train) %>%
  step_smote(Origin, over_ratio= 1) %>% 
  prep()

train_datos <- bake(receta,new_data = train)
