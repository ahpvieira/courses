#' Two of the most important questions that a data scientist must answer when building machine
#' learning models are: how well would my model perform on new data and did I select the best
#' performing model?
#' 
#' Thgoughout this chapter you'll learng the techniques necessary to answer these questions.
#' 
#' To answer the first question, start with all of your data and aplit it into two portions.
#' The first portion is used to train a model and the second portion is used to test how
#' well it performs on new data. This is known as the train-test split.
#' 
#' In a disciplined machine learning workflow this is a critical first step.
#' 
#' So long as the test data is a fair representation of the data you can expect to see in the future,
#' you can use it to estimate the expected performance for future observations.
#' 
#' To make the train-test split you will use the initial_split() function from the rsample package.
#' The prop parameter is used to specify the proportion of data that will be selected for train
#' set.
#' 
#' To prepare the trianing and the testing dataframes, you use the functions training() and
#' testing(), respectively.
#' 
#' The train data can be further splitted into two partitions of train and validate. Now you can use
#' the train data to build your models and use validate to calculate their performance.
#' 
#' You can take this one step further, by repeating this train-validate split several times, 
#' each time reserving a different portion of data for evaluation. This is known as cross-validation
#' and provides two key advantages: at the end, you'll use all the data to evaluate the overall
#' performance of a model; second, you'll be able to calculate multiple measures of performance.

# Load packages -------------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(broom)
library(rsample) # Train-est split
library(Metrics) # MAE
library(ranger) # random forest


# Make the train-test split -------------------------------------------------------------------

#' Note: Since this is a random split of the data it is good practice to set a seed before 
#' splitting it.

set.seed(42)

# Prepare the initial split object
gap_split <- initial_split(drop_na(gapminder), prop = 0.75)

# Extract the training dataframe
training_data <- training(gap_split)

# Extract the testing dataframe
testing_data <- testing(gap_split)
nrow(training_data)
nrow(testing_data)


# Build cross-validated of train and validate data --------------------------------------------

cv_split <- vfold_cv(training_data, v = 3) # v - how many times your data should be splitted

#' In order to build a model for each fold, you'll need to extract the training and validation
#' dataframes into their own list columns.

# Create the train and validate dataframes for each fold

cv_data <- cv_split %>% 
      mutate(train = map(splits, ~training(.x)),
             validate = map(splits, ~testing(.x)))

#' You can use each of the training dataframes to build corresponding models
cv_models_lm <- cv_data %>% 
      mutate(model = map(train, ~lm(formula = life_expectancy~., data = .x)))


# Measuring cross-validation performance ------------------------------------------------------

#' Now that you've generated your cross-validated dataframes and models, let's learn how to use
#' validation data to measure the performance of each model.

#' You need three ingredients to measure performance:
#' 1. Actual outcome values
#' 2. Predicted outcome values
#' 3. A metric to compare 1) and 2)

# Extract actual outcome values
cv_prep_lm <- cv_models_lm %>% 
      mutate(validate_actual = map(validate, ~.x$life_expectancy))

# Prepare the predicted values (The predict() & map2() functions)
cv_prep_lm <- cv_models_lm %>% 
      mutate(
            # Extract the recorded life expectancy for the records in the validate dataframes
            validate_actual = map(validate, ~.x$life_expectancy),
            # Predict life expectancy for each validate set using its corresponding model
            validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y))
      )

# Calculate MAE
cv_eval_lm <- cv_prep_lm %>% 
      mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, 
                                     ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_lm$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_lm$validate_mae) 

#' The average mean absolute error is 1.5 years, meaning that you can expect the model predictions
#' will be off, on average, by 1.5 years.

# Buiding and tuning a random forest model ----------------------------------------------------

#' Because the same data will be used across the models, you can directly compare their validation
#' performance between them, allowing you to select the best performing model.
#' 
#' You can use this machine learning workflow to compare virtually any model. So let's try this out
#' with a random forest model to see if it achieves a higher performance.
#' 
#' The random forest is a very popular model in the machine learning community. It natively handle
#' both non-linear relationships and feature interactions so we can be optimistic about trying
#' this model. 

# Build a random forest model for each fold
cv_models_rf <- cv_data %>% 
      mutate(model = map(train, ~ranger(formula = life_expectancy~., 
                                        data = .x, seed = 42)))

# Generate predictions using the random forest model
cv_prep_rf <- cv_models_rf %>% 
      mutate(validate_actual = map(validate, ~.x$life_expectancy),
             validate_predicted = map2(model, validate, 
                                       ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold
cv_eval_rf <- cv_prep_rf %>% 
      mutate(validate_mae = map2_dbl(validate_actual, validate_predicted, ~mae(actual = .x, predicted = .y)))

# Print the validate_mae column
cv_eval_rf$validate_mae

# Calculate the mean of validate_mae column
mean(cv_eval_rf$validate_mae)

#' You've dropped the average error of your predictions from 1.47 to 0.79. 
#' That's quite an improvement!

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>% 
      crossing(mtry = 2:5) 

# Build a model for each fold & mtry combination
cv_model_tunerf <- cv_tune %>% 
      mutate(model = map2(.x = train, .y = mtry, ~ranger(formula = life_expectancy~., 
                                                         data = .x, mtry = .y, 
                                                         num.trees = 100, seed = 42)))

#' You've built a model for each fold/mtry combination. 
#' Next, you'll measure the performance of each to find the best performing value of mtry

# Generate validate predictions for each model
cv_prep_tunerf <- cv_model_tunerf %>% 
      mutate(validate_actual = map(validate, ~.x$life_expectancy),
             validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y)$predictions))

# Calculate validate MAE for each fold and mtry combination
cv_eval_tunerf <- cv_prep_tunerf %>% 
      mutate(validate_mae = map2_dbl(.x = validate_actual, 
                                     .y = validate_predicted, 
                                     ~mae(actual = .x, predicted = .y)))

# Calculate the mean validate_mae for each mtry used  
cv_eval_tunerf %>% 
      group_by(mtry) %>% 
      summarise(mean_mae = mean(validate_mae))

#' Looks like parameter tuning was able to eke out another slight boost in performance, dropping 
#' the mae from .842 (mtry = 2) to 0.830 (mtry = 3). Assuming that you've finished your model 
#' selection you can conclude that your final (best performing) model will be the random forest
#' model built using ranger with an mtry = 3 and num.trees = 100. 

# Tune the hyper-parameters
cv_tune <- cv_data %>% 
      crossing(mtry = 1:5)

cv_model_tunerf <- cv_tune %>% 
      mutate(model = map2(train, mtry, ~ranger(formula = life_expectancy~., 
                                               data = .x, mtry = .y)))
# Measuring the test performance --------------------------------------------------------------

#' Now you will use all of the train data prepared during the initial split to build the random
#' forest model. This is the final model and is the one you would expect to use in a production
#' environment. As such you would like to know how well you can expect this model will perform
#' on new data.
#' 
#' To do this you bring back the test data and treat it as the desired new data for evaluation.
#' 
#' By comparing the actual values of life expectancy for the test set with the values predicted
#' using the final model you can estimate the model's performance on new data.
#' 
#' This is known as the model's test performance.

# Build the best performing model
best_model <- ranger(formula = life_expectancy~., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the actual and predicted values for comparison
test_actual <- testing_data$life_expectancy
test_predict <- predict(best_model, testing_data)$predictions

# Compare the actual and predicted values useing a desired metric (MAE, for example)
mae(test_actual, test_predict)

## Practice!

# Build the model using all training data and the best performing parameter
best_model <- ranger(formula = life_expectancy ~ ., data = training_data,
                     mtry = 4, num.trees = 100, seed = 42)

# Prepare the test_actual vector
test_actual <- testing_data$life_expectancy

# Predict life_expectancy for the testing_data
test_predicted <- predict(best_model, testing_data)$predictions

# Calculate the test MAE
mae(test_actual, test_predicted)

#' You have successfully leveraged the list column workflow to identify and build a model to 
#' predict life expectancy. You can claim that based on the test holdout you can expect that 
#' your predictions on new data will only be off by a magnitude of 0.625 years.







