#' Logistic regression involves fitting a curve to numeric data to make 
#' predictions about binary events. Arguably one of the most widely used
#' machine learning methods, this chapter will provide an overview of the 
#' technique while illustrating how to apply it to fundraising data.


# Making binary predictions with regression --------------------------

# Practice!

donors <- read_csv("./cursos/machine-learning-fundamentals/supervised-learning in-r-classification/donors.csv")

# Build the donation model
donation_model <- glm(donated ~ interest_religion + interest_veterans + bad_address, data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donation_prob)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)

#' With an accuracy of nearly 80%, the model seems to be doing its job.
#' But the model is actually performing WORSE than if it were to predict 
#' non-donor for every record.

# Model performance tradeoffs ----------------------------------------

#' Rare events create challenges for classification models. When one outcome
#' is very rare, predicting the opposite can result in a very high accurcacy.

# Practice!

# Load the pROC package
library(pROC)

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)


#' When AUC values are very close, it's important to know more about how
#' the model will be used.

# Dummy variables, missing data, and interactions --------------------

# create gender factor
my_data$gender <- factor(my_data$gender,
                         levels = c(0, 1, 2),
                         labels = c("Male", "Female", "Other"))

# interaction of obesity and smoking
glm(disease ~ obesity * smoking,
    data = health,
    family = "binomial")

# Convert the wealth rating to a factor
donors$wealth_rating <- factor(donors$wealth_rating, levels = c(0, 1, 2, 3), labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_rating <- relevel(donors$wealth_rating, ref = "Medium")

# See how our factor coding impacts the model
model <- glm(donated ~ wealth_rating, data = donors, family = "binomial")
summary(model)

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), round(mean(donors$age, na.rm = T), 2), donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money + recency*frequency, data = donors, family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = "response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)

#' Based on the ROC curve, you've confirmed that past giving patterns are 
#' certainly predictive of future giving.

# Automatic feature selection ----------------------------------------

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)





