#' The goal of this chapter is to guide you through a complete analysis using the unsupervised 
#' learning techniques covered in the first three chapters. You'll extend what you've learned by 
#' combining PCA as a preprocessing step to clustering using data that consist of measurements of 
#' cell nuclei of human breast masses.
#' 
#' We'lll complete six steps during this analysis:
#' 
#' * Download and prepare data for modeling
#' * Exploratory data analysis
#' * Perform PCA and interpret results
#' * Complete two types of clustering
#' * Understand and compare the two types
#' * Combine PCA as a pre-processing step to clustering

# Introduction to the case study --------------------------------------------------------------

# Practice

library(tidyverse)
library(janitor)
library(skimr) # summary of data
library(factoextra) # pcz visualization

options(scipen = 999)

# Read the data: wisc.df

wisc.df <- read_csv("WisconsinCancer.csv")

# Convert the features of the data: wisc.data
wisc.data <- as.matrix(wisc.df[, c(3:32)])
                               
# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

length(str_subset(names(wisc.df), "_mean"))

# Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, 2, sd)

filter(skim(select(wisc.df, 3:32)), stat %in% c("mean", "sd"))

# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr <- prcomp(wisc.data, scale = T)

# Look at summary of results
summary(wisc.pr)

# Create a biplot of wisc.pr
biplot(wisc.pr)

# What stands out to you about this plot? Is it easy or difficult to understand? Why?

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
        xlab = "PC1", ylab = "PC3")

# What do you notice about these plots?

#' Because principal component 2 explains more variance in the original data than principal 
#' component 3, you can see that the first plot has a cleaner cut separating the two subgroups.

# Do additional data exploration of your choosing below (optional)
fviz_eig(wisc.pr)
s
fviz_pca_ind(wisc.pr,
             label = "none", # hide individual labels
             habillage = wisc.df$diagnosis, # color by groups
             # palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

fviz_pca_ind(wisc.pr,
             axes = c(1, 2),
             label = "none", # hide individual labels
             habillage = wisc.df$diagnosis, # color by groups
             # palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
             ) +
      scale_color_brewer(palette = "Set1")

fviz_pca_ind(wisc.pr,
             axes = c(1, 3),
             label = "none", # hide individual labels
             habillage = wisc.df$diagnosis, # color by groups
             # palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
) +
      scale_color_brewer(palette = "Set1")

# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var/sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

#' The loadings, represented as vectors, explain the mapping from the original features to the
#' principal components.

wisc.pr$rotation[, 1:3]

# PCA review and next steps -------------------------------------------------------------------

# Scale the wisc.data data: data.scaled
data.scaled <- scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust <- hclust(data.dist, method = "complete")

par(mfrow = c(1, 1))
plot(wisc.hclust)

#' Normally when performing unsupervised learning like this, a target variable isn't available. 
#' We do have it with this dataset, however, so it can be used to check the performance of the 
#' clustering model.

#' When performing supervised learning—that is, when you're trying to predict some target variable 
#' of interest and that target variable is available in the original data—using clustering to 
#' create new features may or may not improve the performance of the final model. This exercise will
#' help you determine if, in this case, hierarchical clustering provides a promising new feature.

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, wisc.df$diagnosis)

#' Four clusters were picked after some exploration. Before moving on, you may want to explore how 
#' different numbers of clusters affect the ability of the hierarchical clustering to separate the 
#' different diagnoses.

# Create a k-means model on wisc.data: wisc.km
wisc.km <- kmeans(scale(wisc.data), centers = 2, nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, wisc.df$diagnosis)

# How well does k-means separate the two diagnoses?

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)

#' Looking at the second table you generated, it looks like clusters 1, 2, and 4 from the 
#' hierarchical clustering model can be interpreted as the cluster 1 equivalent from the k-means 
#' algorithm, and cluster 3 can be interpreted as the cluster 2 equivalent.

#' Recall from earlier exercises that the PCA model required significantly fewer features to 
#' describe 80% and 95% of the variability of the data. In addition to normalizing data and 
#' potentially avoiding overfitting, PCA also uncorrelates the variables, sometimes improving 
#' the performance of other modeling techniques.

# Let's see if PCA improves or degrades the performance of hierarchical clustering.

# Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, c(1:7)]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
table(wisc.hclust.clusters, wisc.df$diagnosis)
table(wisc.pr.hclust.clusters, wisc.df$diagnosis)
table(wisc.km$cluster, wisc.df$diagnosis)

# Compare to k-means and hierarchical
table(wisc.km$cluster, wisc.pr.hclust.clusters)


# Wrap-up and review --------------------------------------------------------------------------







