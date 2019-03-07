#' Machine Learning utilizes computer to turn data into insight and action. This course focus on a 
#' subset of machine learning that focuses on training a machine to learn from prior examples. When
#' a concept to be learned is a set of categories, the task is called classification.
#' 
#' A algorithm called k nearest neighbors uses the principle of the nearest neighbors to classify
#' unlabeled examples.
#' 
#' By default, R's knn function searches a dataset for the observation most similar to the nearly 
#' observed #' one.


# Classification with Nearest Neighbors -------------------------------------------------------

# Practice!

# Load the 'class' package
library(class)
library(tidyverse)

# Load data
signs <- read_csv("./cursos/machine-learning-fundamentals/supervised-learning in-r-classification/knn_traffic_signs.csv")

# Create a vector of labels
sign_types <- signs$sign_type

# Classify the next sign observed
knn(train = signs[-1], test = next_sign, cl = sign_types) # next_sign not available

# kNN isn't really learning anything; it simply looks for the most similar example.

# Examine the structure of the signs dataset
glimpse(signs)

# Count the number of signs of each type
janitor::tabyl(signs$sign_type)

# Check r10's average red level by sign type
summarize(group_by(signs, sign_type), mean_r10 = mean(r10))

# Stop signs tend to have a higher average red value. This is how kNN identifies similar signs.

## Classifying a collection of road signs

# Use kNN to identify the test road signs
sign_types <- signs$sign_type
signs_pred <- knn(train = signs[-1], test = test_signs[-1], cl = sign_types)

# Create a confusion matrix of the predicted versus actual values
signs_actual <- test_signs$sign_type
table(signs_pred, signs_actual)

# The confusion matrix lets you look for patterns in the classifier's errors.

# Compute the accuracy
mean(signs_pred == signs_actual)
# 2.343.095

# What about the 'k' in kNN? ------------------------------------------------------------------

#' The value of k may have a substantial impact on the performance of our
#' classifier. A smaller k may utilize more subtle patterns.
#' Setting k larger ignores some potentially noisy points in an effort 
#' to discover a broader more general pattern.
#' 
#' There is no universal rule to set k. In practice, the optimal value depends
#' on the complexity of the pattern to be learn as well as the impact of noisy
#' data.
#' 
#' Some suggest the rule of thumb of starting k as the squared root of
#' the number of observations in the training data. An even better approach
#' is to test several values of k and compare their performance on data
#' it has not seen before.

## Practice!

# Compute the accuracy of the baseline model (default k = 1)
k_1 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types)
mean(k_1 == signs_actual)

# Modify the above to set k = 7
k_7 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 7)
mean(k_7  == signs_actual)

# Set k = 15 and compare to the above
k_15 <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 15)
mean(k_15 == signs_actual)

# Use the prob parameter to get the proportion of votes for the winning class
sign_pred <- knn(train = signs[-1], test = signs_test[-1], cl = sign_types, k = 7, prob = TRUE)

# Get the "prob" attribute from the predicted classes
sign_prob <- attr(sign_pred, "prob")

# Examine the first several predictions
head(sign_pred)

# Examine the proportion of votes for the winning class
head(sign_prob)

# Data preparation for kNN --------------------------------------------------------------------

#' KNN learners use distance functions to identify the most similar or 
#' nearest examples. Many common disntace functions assume your data is in
#' a numeric format. When calculating distance each feature of the input
#' data should be measured with the same range of values.
#' 
#' The features with a wider range will have more influence over the
#' distance calculation.  Rescaling reduces the influence of extreme values
#' on kNN's distance function.

# define a min-max normalize() function
normalize <- function(x) {
      return((x - min(x)) / (max(x) - min(x)))
}










