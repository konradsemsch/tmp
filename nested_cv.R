
library(tidymodels)
library(tidyverse)
library(recipes)
library(magrittr)
library(parsnip)
library(tune)
library(workflows)
library(dials)
library(yardstick)

data("iris")
colnames(iris) <- tolower(names(iris))

# Problem with multi-class classification??
iris %<>%
  select(species, sepal.length)
  # filter(species %in% c("setosa", "versicolor")) %>%
  # mutate(species = factor(species, levels = c("setosa", "versicolor")))

split <- initial_split(iris, prop = 0.9, strata = "species")

train <- training(split)
test <- testing(split)

###  Different ways to do data splitting ###

# Regular cross-validation
cv <- vfold_cv(
  train,
  v = 3,
  repeats = 5,
  strata = "species"
)

# Bootstrap
bs <- bootstraps(
  train,
  times = 10,
  strata = "species"
)

# Leave-one-out cross validation


# Nested cross-validation - I will need to do this in a custom way
# You can add that at a later stage
cv_nested <- nested_cv(
  train,
  outside = cv,
  inside = bootstraps(times = 20, strata = "species")
  )

recipe <- train %>%
  recipe(species ~ .) %>%
  step_medianimpute(all_numeric()) %>%
  step_BoxCox(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  step_lincomb(all_numeric(), max_steps = 25) %>%
  step_corr(all_numeric(), threshold = 0.75)

model <- logistic_reg(
  penalty = tune(),
  mixture = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("glm")

grid <- grid_max_entropy(
  penalty(),
  mixture(),
  size = 10,
  variogram_range = 1
)

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model)

# Update the package and check if the error persists
tuning <- tune_grid(
  workflow,
  resamples = bs,
  grid = grid,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE) # add paralell procecessing at a later stage
)




















