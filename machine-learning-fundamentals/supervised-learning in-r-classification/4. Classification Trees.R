#' Classification trees use flowchart-like structures to make decisions. 
#' Because humans can readily understand these tree structures, 
#' classification trees are useful when transparency is needed, such as in
#' loan approval. We'll use the Lending Club dataset to simulate this 
#' scenario.

# building a simple rpart classification tree
library(rpart)
library(tidyverse)

m <- rpart(outcome ~ loan_amount + credit_score, data = loans,
           method = "class")

# making predictions from an rpart tree
p <- predict(m, test_data, type = "class")

# Making decision with trees -----------------------------------------

loans <- read_csv("./cursos/machine-learning-fundamentals/supervised-learning in-r-classification/loans.csv")

# Load the rpart package
library(rpart)

# Build a lending model predicting loan outcome versus loan amount and credit score
loan_model <- rpart(outcome ~ loan_amount + credit_score, data = loans, method = "class", control = rpart.control(cp = 0))

# Make a prediction for someone with good credit
predict(loan_model, good_credit, type = "class")

# Make a prediction for someone with bad credit
predict(loan_model, bad_credit, type = "class")

library(rpart.plot)

# Examine the loan_model object
loan_model

# Load the rpart.plot package
library(rpart.plot)

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), 
           fallen.leaves = TRUE)

# Growing larger classification trees --------------------------------

# Determine the number of rows for training
0.75*nrow(loans)

# Create a random sample of row IDs
sample_rows <- sample(11312, 8484)

# Create the training dataset
loans_train <- loans[sample_rows, ]

# Create the test dataset
loans_test <- loans[-sample_rows, ]

# Grow a tree using all of the available applicant data
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, type = "class")

# Examine the confusion matrix
table(loans_test$pred, loans_test$outcome)

# Compute the accuracy on the test dataset
mean(loans_test$pred == loans_test$outcome)

# Tending to classification trees ------------------------------------

#' Decision trees have the tendency to grow very largeand complex very
#' quickly. One method of preventing a tree from becoming too large
#' involves stopping the growing process early. This is known as pre-pruning.
#' 
#' One of the simplest approaches to pre-pruning stops divide-and-conquer
#' once the tree reaches a predefined size. Another pre-pruning method
#' requires a minimum number of observations at a node in order for a split
#' to occur.
#' 
#' However, a tree stopped too early may fail to discober subtle or 
#' important patterns it might have discovered later. To address this 
#' concern, it is also possible to grow a very large tree, knowing that it
#' will be overly complex, but then prune it back to reduce the size.
#' This is known as post-pruning.
#' 
#' 

# Grow a tree using all of the available applicant data
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", 
                    control = rpart.control(cp = 0))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model, loans_test, type = "class")

# Examine the confusion matrix
table(loans_test$pred, loans_test$outcome)

# Compute the accuracy on the test dataset
mean(loans_test$pred == loans_test$outcome)

# post-pruning with rpart
m <- rpart(repaid ~ credit_score + request_amt,
           data = loans,
           method = "class")

plotcp(m)

m_pruned <- prune(m, cp = 0.20)

# Practice!

# Grow a tree with maxdepth of 6
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", 
                    control = rpart.control(cp = 0, maxdepth = 6))

# Make a class prediction on the test set
loans_test$pred <- predict(loan_model, loans_test, type = "class")

# Compute the accuracy of the simpler tree
mean(loans_test$pred == loans_test$outcome) # 59.2%
 
# Swap maxdepth for a minimum split of 500 
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, minsplit = 500))

# Run this. How does the accuracy change?
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome) # 59.2%

#' It may seem surprising, but creating a simpler decision tree may 
#' actually result in greater performance on the test dataset.

# Grow an overly complex tree
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome) # 60.1%

#' As with pre-pruning, creating a simpler tree actually improved the 
#' performance of the tree on the test dataset.

# Seeing the forest from the trees -----------------------------------

#' Classification trees can be combined into a collection known as a 
#' decision tree forest. These forests are among the most powerful 
#' machine learning classifiers.
#' 
#' The power of decision tree forests comes from a collection of smaller,
#' simpler trees that together reflect the data's complexity. Each of the
#' forest's trees is diverse, and may reflect some subtle pattern in the
#' outcome to be modeled.
#' 
#' Generating this diversity is the key to building powerful decision 
#' tree forests. This is done by allocating each tree a random subset of
#' data. One may receive a vastly different training set than another. 
#' 
#' The term random forests refers to the specific growth algorithm in 
#' which both the features and examples may differ from tree to tree.
#' 
#' Machine learning methods like random forests that apply this principle
#' are called ensemble methods. They are based on the principle that 
#' weaker learners become stronger with teamwork.
#' 
#' In a random forest, each tree is asked to make a prediction, and 
#' the group's overall prediction is determined by a majority vote.
#' 
#' The R package randomForest implements the random forest algorithm.

# building a simple random forest
library(randomForest)
m <- randomForest(repaid ~ credit_score + request_amt, data = loans,
                  ntree = 500,    # number of trees in the forest
                  mtry = sqrt(p)) # number of predictors (p) per tree

# making predictions from a random forest
p <- predict(m, test_data)

# Practice!

# Load the randomForest package
library(randomForest)

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, loans_test)
mean(loans_test$pred == loans_test$outcome) # 60.2%

#' Classification is only one of the problems you'll have to tackle as a 
#' data scientist.
