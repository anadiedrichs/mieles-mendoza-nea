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

unique(dataset$Origin)

ggplot(dataset,aes(V,Li)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(dataset,aes(Origin,Na)) +
  geom_point() 
  

plot_boxplot(dataset,by="Origin")

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
  step_corr() %>%
  step_normalize() %>% 
  step_smote(Origin,over_ratio= 2) 

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