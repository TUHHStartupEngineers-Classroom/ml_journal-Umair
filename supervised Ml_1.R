# Standard
library(tidyverse)

# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
library(rpart.plot)
library(tidymodels)
library(broom.mixed)
# Modeling ----------------------------------------------------------------

bike_features_tbl <- readRDS("raw_data//bike_features_tbl.rds")
bike_features_tbl_data <- bike_features_tbl %>% 
  select(model:gender, `Rear Derailleur`, `Shift Lever`) 

set.seed(seed = 1113)

# LINEAR REGRESSION -
model_linear_regression <- linear_reg("regression") %>%
  set_engine("lm")
split_obj <- rsample::initial_split(bike_features_tbl_data, prop   = 0.80, 
                                    strata = "category_2")

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

train_tbl <- train_tbl %>% set_names(str_replace_all(names(train_tbl), " |-", "_"))
test_tbl  <- test_tbl  %>% set_names(str_replace_all(names(test_tbl),  " |-", "_"))

bike_price_rec <- 
  
  recipe(price  ~ ., data = train_tbl %>% select(-c(model:weight), -category_1, -c(category_3:gender))) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  prep()

summary(bike_price_rec)

train_transformed_tbl <- bake(bike_price_rec, new_data = NULL)
test_transformed_tbl  <- bake(bike_price_rec, new_data = test_tbl)

train_transformed_tbl
test_transformed_tbl

# Bundle the model and recipe with the workflow package
bikes_wflow <- 
  workflow() %>% 
  add_model(model_linear_regression) %>% 
  add_recipe(bike_price_rec)
bikes_fit <- 
  bikes_wflow %>% 
  fit(data = train_tbl)

# Evaluate your model with the yardstick package
predict(bikes_fit, test_tbl) %>%
  
  bind_cols(test_tbl %>% select(price)) %>%
  yardstick::metrics(truth = price, estimate = .pred)
