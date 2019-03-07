#' In this chapter you will shift gears to work with another group of models called binary
#' classification models.
#' 
#' To learn the tools and skills associated with these models you will explore the attrition dataset.
#' 
#' Throughout this chapter you will work on building a model that will use the available features
#' to predict if an employee has quit.
#' 
#' The first model you will work with is the logistic regression model.
#' 
#' In order to build a logistic regression model in R you will use the generalized linear model 
#' function, glm().
#' 
#' Working with the cross-validated dataframe, you will build a LRM for each fold. As before, you
#' can leverage the mutate() and map() combination to map the glm() function for each train
#' dataframe.
#' 
#' You will prepare the attrition dataset for the train-test-validate splits and build a LRM
#' for each fold.

# Load packages -------------------------------------------------------------------------------

library(tidyverse)
library(dslabs)
library(broom)
library(rsample) # Train-est split
library(Metrics) # MAE, precision, recall, accuracy
library(ranger) # random forest

# Logistic regression models ------------------------------------------------------------------

## Practice!

#' You will first prepare the training & testing data sets, then you will further split the 
#' training data using cross-validation so that you can search for the best performing model 
#' for this task.

set.seed(42)

# Prepare the initial split object
data_split <- initial_split(attrition, prop = 0.75)

# Extract the training dataframe
training_data <- training(data_split)

# Extract the testing dataframe
testing_data <- testing(data_split)

# Build a dataframe for 5-fold cross validation from the training_data
set.seed(42)
cv_split <- vfold_cv(training_data, v = 5)

cv_data <- cv_split %>% 
      mutate(
            # Extract the train dataframe for each split
            train = map(splits, ~training(.x)),
            # Extract the validate dataframe for each split
            validate = map(splits, ~testing(.x))
      )

# Build a model using the train data for each fold of the cross validation
cv_models_lr <- cv_data %>% 
      mutate(model = map(train, ~glm(formula = Attrition ~ ., 
                                     data = .x, family = "binomial")))

# Evaluating classification models ------------------------------------------------------------

#' Now you need to learn how to evaluate the performance of the LRMs.
#' 
#' The ingredients needes to measure performance are the same as before:
#' 1. Actual outcome classes
#' 2. Predicted attrition classes
#' 3. A metric to compare 1) & 3)
#' 
#' To prepare the vector of actual classes you need to convert the attrition vector from character
#' to a binary.
#' 
#' To prepare the predicted classes you first need to prepare the probability vector. You will use
#' the predict() function with the argument type equal to "response". Next you need to convert these
#' probability values into a binary vector.
#' 
#' Now that you have the actual and predicted binary vectors you can decide which metric is appropriate
#' for the problem you are trying to solve. This course introduces three popular metrics: 
#' accuracy, precision and recall.

## Practice!

#' You will learn how to prepare these vectors using the model and validate dataframes from the 
#' first cross-validation fold as an example.

# Extract the first model and validate 
model <- cv_models_lr$model[[1]]
validate <- cv_models_lr$validate[[1]]

# Prepare binary vector of actual Attrition values in validate
validate_actual <- validate$Attrition == "Yes"

# Predict the probabilities for the observations in validate
validate_prob <- predict(model, validate, type = "response")

# Prepare binary vector of predicted Attrition values for validate
validate_predicted <- validate_prob > 0.5

# Compare the actual & predicted performance visually using a table
table(validate_actual, validate_predicted)

# Calculate the accuracy
accuracy(validate_actual, validate_predicted)

# Calculate the precision
precision(validate_actual, validate_predicted)

# Calculate the recall
recall(validate_actual, validate_predicted)

#' Now that you know how to calculate the performance metrics for a single model, you are now 
#' ready to expand this for all the folds in the cross-validation dataframe.

cv_prep_lr <- cv_models_lr %>% 
      mutate(
            # Prepare binary vector of actual Attrition values in validate
            validate_actual = map(validate, ~.x$Attrition == "Yes"),
            # Prepare binary vector of predicted Attrition values for validate
            validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response") > 0.5)
      )


# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_lr %>% 
      mutate(validate_recall = map2_dbl(validate_actual, validate_predicted, 
                                        ~recall(actual = .x, predicted = .y)))

# Print the validate_recall column
cv_perf_recall$validate_recall

# Calculate the average of the validate_recall column
mean(cv_perf_recall$validate_recall)

#' As you can see the validate recall of the model is 0.42.

# Random forest for classification ------------------------------------------------------------

#' To evaluate the random forest model, you use the same framwework of comparing the actual
#' and predicted classes.

# Prepare for tuning your cross validation folds by varying mtry
cv_tune <- cv_data %>%
      crossing(mtry = c(2, 4, 8, 16)) 

# Build a cross validation model for each fold & mtry combination
cv_models_rf <- cv_tune %>% 
      mutate(model = map2(train, mtry, ~ranger(formula = Attrition~., 
                                               data = .x, mtry = .y,
                                               num.trees = 100, seed = 42)))

# Next you will evaluate the validation performance of these random forest models.

cv_prep_rf <- cv_models_rf %>% 
      mutate(
            # Prepare binary vector of actual Attrition values in validate
            validate_actual = map(validate, ~.x$Attrition == "Yes"),
            # Prepare binary vector of predicted Attrition values for validate
            validate_predicted = map2(.x = model, .y = validate, ~predict(.x, .y, type = "response")$predictions == "Yes")
      )

# Calculate the validate recall for each cross validation fold
cv_perf_recall <- cv_prep_rf %>% 
      mutate(recall = map2_dbl(.x = validate_actual, .y = validate_predicted, ~recall(actual = .x, predicted = .y)))

# Calculate the mean recall for each mtry used  
cv_perf_recall %>% 
      group_by(mtry) %>% 
      summarise(mean_recall = mean(recall))


#' This time you can see that none of the random forest models were able to outperform the 
#' logistic regression model with respect to recall.

#' You will build the logistic regression model using all of the train data and you will prepare 
#' the necessary vectors for evaluating this model's test performance.

# Build the logistic regression model using all training data
best_model <- glm(formula = Attrition ~ ., 
                  data = training_data, family = "binomial")


# Prepare binary vector of actual Attrition values for testing_data
test_actual <- testing_data$Attrition == "Yes"

# Prepare binary vector of predicted Attrition values for testing_data
test_predicted <- predict(best_model, testing_data, type = "response") > 0.5

#' You've now selected & built your best performing model and have prepared the necessary parts to 
#' evaluate its performance.

#' Now its time to calculate the test performance of your final model (logistic regression). 
#' Here you will use the held out testing data to characterize the performance you would expect 
#' from this model when it is applied to new data.

# Compare the actual & predicted performance visually using a table
table(test_actual, test_predicted)

# Calculate the test accuracy
accuracy(test_actual, test_predicted)

# Calculate the test precision
precision(test_actual, test_predicted)

# Calculate the test recall
recall(test_actual, test_predicted)

#' You now have a model that you can expect to identify 36% of employees that are at risk to leave 
#' the organization.

# Wrap-up -------------------------------------------------------------------------------------

#' The list column workflow is the backbone of working with models in the tidyverse.

# Link to course: https://www.datacamp.com/courses/machine-learning-in-the-tidyverse




