
library(modeldata)
library(tidymodels)
library(tidyverse)
library(magrittr)

data("okc")
colnames(okc) <- tolower(names(okc))

okc <- sample_n(okc, 1000)

strata <- "class"
split <- initial_split(okc, prop = 0.8, strata = strata)

train <- training(split)
test <- testing(split)

cv <- vfold_cv(
  train,
  v = 5,
  repeats = 1,
  strata = strata
)

### Function implementation
# 1) Create a base recipe where training data is provided 
# and roles for all variables are defined
recipe_base <- train %>%
  recipe() %>%
  update_role(class, new_role = "outcome") %>% 
  update_role(-one_of("class"), new_role = "predictors")

# 2) Pass your base recipe into the apply_recipe function
# and provide the recipe blueprint to bind to it
recipe <- recipe_base %>% 
  add_blueprint("v1")

# 3) Run your model as you normally would (single blueprint)
model <- logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(model) %>% 
  fit(data = train)

# 4) Test your model on multiple blueprints


# Helper functions
add_blueprint <- function(recipe_base, blueprint_id){
  
  recipe_summary <- summarise_recipe(recipe_base)
  
  # Binding the base recipe with the selected blueprint
  recipe <- select_blueprint(recipe_base, recipe_summary, blueprint_id)
  
  return(recipe)

  }

summarise_recipe <- function(recipe_base){
  
  # Recipe summary
  recipe_summary <- summary(recipe_base)
  
  # Collecting individual variable information by role
  var_outcome <- recipe_summary %>% 
    filter(role == "outcome") %>% 
    pull(variable)
  
  var_predictor <- recipe_summary %>% 
    filter(role == "predictors") %>% 
    pull(variable)
  
  var_other <- recipe_summary %>% 
    filter(!(role %in% c("outcome", "predictors"))) %>% 
    pull(variable)
  
  # Collecting individual variable information by type
  var_numeric <- recipe_summary %>% 
    filter(type == "numeric") %>% 
    pull(variable)
  
  var_nominal <- recipe_summary %>% 
    filter(type == "nominal") %>% 
    pull(variable)
  var_date <- recipe_summary %>% 
    filter(type == "date") %>% 
    pull(variable)
  
  var_info <- list(
    by_role = list(
      outcome = var_outcome,
      predictor = var_predictor,
      other = var_other
    ),
    by_type = list(
      numeric = var_numeric,
      nominal = var_nominal,
      date = var_date
    )
  )
  
  return(var_info)
  
}

select_blueprint <- function(recipe_base, recipe_summary, blueprint_id){
  
  if(blueprint_id == "v1"){
    blueprint_v1(recipe_base, recipe_summary) 
  } else {
    stop("Blueprint id not available")
  }
  
}

blueprint_v1 <- function(recipe_base, recipe_summary){
  
  var_numeric <- recipe_summary$by_type$numeric
  var_date <- recipe_summary$by_type$date
  
  recipe_base %>% 
  ### Handling time predictors
  step_date(one_of(var_date)) %>%
  step_holiday(one_of(var_date)) %>% 
  step_rm(has_type("date")) %>% 
  
  ### Imputation
  # Numeric predictors
  step_medianimpute(all_numeric(), -all_outcomes()) %>%
  
  # Categorical predictors
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  
  ### Lumping infrequent categories (optional)
  step_other(all_nominal(), -all_outcomes(), other = "infrequent") %>% 
  
  ### Removing zero-variance predictors (optional)
  
  ### Individual transformations (optional)
  
  ### Dummyfying (optional features)
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  
  ### Interactions (optional)
  
  ### Normalization (optional)
  step_normalize(one_of(var_numeric)) 

  ### Multivariate transformations (optional)
  
  ### Removing unnecessary predictors
  
  ### Checks
  
}

blueprint_v2 <- function(recipe_base, recipe_summary){
  
  var_numeric <- recipe_summary$by_type$numeric
  var_date <- recipe_summary$by_type$date
  
  recipe_base %>% 
    ### Handling time predictors
    step_date(one_of(var_date)) %>%
    step_holiday(one_of(var_date)) %>% 
    step_rm(has_type("date")) %>% 
    
    ### Imputation
    # Numeric predictors
    step_medianimpute(all_numeric(), -all_outcomes()) %>%
    
    # Categorical predictors
    step_modeimpute(all_nominal(), -all_outcomes()) %>%
    
    ### Lumping infrequent categories (optional)
    step_other(all_nominal(), -all_outcomes(), other = "infrequent") %>% 
    
    ### Removing zero-variance predictors (optional)
    
    ### Individual transformations (optional)
    step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
    
    ### Dummyfying (optional features)
    step_dummy(all_nominal(), -all_outcomes()) %>% 
    
    ### Interactions (optional)
    
    ### Normalization (optional)
    step_normalize(one_of(var_numeric)) 
  
  ### Multivariate transformations (optional)
  
  ### Removing unnecessary predictors
  
  ### Checks
  
}

### Original piece of code

# Creating base recipe
recipe_base <- train %>%
  recipe() %>%
  
  ### Roles assignment
  update_role(strata, new_role = "outcome") %>% 
  update_role(-one_of(strata), new_role = "predictors")

# Recipe summary
recipe_summary <- summary(recipe_base)

# Collecting individual variable information
var_date <- recipe_summary %>% 
  filter(type == "date") %>% 
  pull(variable)

var_numeric <- recipe_summary %>% 
  filter(type == "numeric") %>% 
  pull(variable)

var_nominal <- recipe_summary %>% 
  filter(type == "nominal") %>% 
  pull(variable)

# Building the actual recipe
recipe <- recipe_info %>% 
  ### Imputation
  # Numeric predictors
  step_medianimpute(all_numeric(), all_predictors()) %>%
  
  # Categorical predictors
  step_modeimpute(all_nominal(), all_predictors()) %>%
  
  # Time predictors
  step_mutate_at(one_of(recipe_var_date), fn = as.factor) %>% 
  step_unknown(date) %>% 
  step_mutate_at(one_of(recipe_var_date), fn = as.Date) %>% 
  
  ### Handling time predictors
  step_date(one_of(recipe_var_date)) %>%
  step_holiday(one_of(recipe_var_date)) %>% 
  
  ### Individual transformations (optional)
  
  ### Lumping infrequent categories (optional)
  step_other(all_nominal(), all_predictors(), other = "infrequent") %>% 
  
  ### Dummyfying (optional features)
  step_dummy(all_nominal(), all_predictors(), -all_outcomes()) %>% 
  
  ### Interactions (optional)
  
  ### Normalization (optional)
  step_normalize(one_of(recipe_var_numeric)) %>% 
  
  ### Multivariate transformations (optional)
  
  ### Removing unnecessary predictors
  step_rm(has_type("date"))

### Checks

# Check the structure of the input file
prep(recipe) %>% juice() %>% glimpse()