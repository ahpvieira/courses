#' Naive Bayes uses principles from the field of statistics to make 
#' predictions. This chapter will introduce the basics of Bayesian methods
#' while exploring how to apply these techniques to iPhone-like destination
#' suggestions.

# Understanding Bayesian methods -------------------------------------

# building a Naive Bayes model
library(naivebayes)
m <- naive_bayes(location ~ time_of_day, data = location_history)

# making predictions with Naive Bayes
future_location <- predict(m, future_conditions)

# Practice!

# Load data
where9am <- read_csv("./cursos/machine-learning-fundamentals/supervised-learning in-r-classification/locations.csv")

# Compute P(A) 
p_A <- nrow(filter(where9am, location == "office"))/nrow(where9am)

# Compute P(B)
p_B <- nrow(filter(where9am, daytype == "weekday"))/nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(filter(where9am, location == "office" & daytype == "weekday"))/nrow(where9am)

nrow(filter(where9am, location == "office" & daytype == "weekend"))

# Compute P(A | B) and print its value
p_A_given_B <- p_AB/p_B
p_A_given_B

# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict Thursday's 9am location
predict(locmodel, thursday9am)

# Predict Saturdays's 9am location
predict(locmodel, saturday9am)

# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am, type = "prob")

# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am, type = "prob")

# Understanding NB's "naivety" ---------------------------------------

#' Until we only have considered conditional probability on a single events
#' predicts another. Adding more predictors complicates matters and this is
#' reason why this method is called naive.
#'
#' Rather than treating a problem as an intersection of all the related
#' events, the algorithm makes the so called naive assumption about the
#' data. Specifically, it assumes the events are independent.
#'
#' Researchers have found that although the naive assumption is rarely true
#' in practice, the naive bayes models still performs admirably in many
#' real world tasks.
#'
#' One should be aware of ... Laplace correction or Laplace estimator.

# Practice!

# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype + hourtype, data = locations)

# Predict Brett's location on a weekday afternoon
predict(locmodel, weekday_afternoon)

# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")

# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype + hourtype, data = locations,laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")

#' Adding the Laplace correction allows for the small chance that Brett 
#' might go to the office on the weekend in the future.


# Applying Naive Bayes to other problems -----------------------------

#' Naive Bayes tends to work wellon problems where the information from
#' multiple attributes needs to be considered simultaneously and evaluated
#' as a whole. IT has also been frequently used to classify text data 
#' (e.g. identifying whether or not an email is a spam).
#' 
#' Naive Bayes models trained with bag of words can be very effective 
#' text classifiers.




