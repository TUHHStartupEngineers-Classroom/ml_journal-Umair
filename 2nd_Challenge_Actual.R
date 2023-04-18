# Load data
library(tidyverse)
library(readxl)
library(h2o)
library(rsample)
library(recipes)
#library(PerformanceAnalytics)




############ STEP # 1 ############################


###                  went_on_backorder


product_backorder_tbl          <- read_csv("product_backorders.csv")

#employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(product_backorder_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

recipe_obj <- recipe(went_on_backorder ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
 # step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)















############ STEP # 2 ############################




# Modeling
h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)






############ STEP # 3 ############################




?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 300,
  nfolds            = 5 
)




################Step 4 View the leaderboard##############










typeof(automl_models_h2o)
## "S4"

slotNames(automl_models_h2o)
## [1] "project_name"   "leader"         "leaderboard"    "event_log"      "modeling_steps" "training_info" 

automl_models_h2o@leaderboard


automl_models_h2o@leader






extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = T) {
  
  model_name <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(n) %>%
    pull(model_id)
  
  if (verbose) message(model_name)
  
  return(model_name)
  
}


automl_models_h2o@leaderboard %>% 
  extract_h2o_model_name_by_position(14) %>% 
  h2o.getModel() %>%






################ Saving & Loading H2O models Step#5 ###############


#h2o.getModel("DeepLearning_grid__1_AutoML_20200820_190823_model_1") %>% 
  h2o.saveModel(path = "04_Modeling/h20_models/challenge")

#h2o.loadModel("04_Modeling/h20_models/challenge/DeepLearning_grid__2_AutoML_20210530_122753_model_1")



h2o.loadModel("04_Modeling/h20_models/challenge/GBM_grid__1_AutoML_20210531_150000_model_3")

############### Step 5 Predicting using Leader Model#####################




# Choose whatever model you want
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/challenge/GBM_grid__1_AutoML_20210531_150000_model_3")
stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)
## [1] "environment"

predictions_tbl_2 <- predictions %>% as_tibble()



################### end ######################