#' The k-means algorithm is one common approach to clustering. Learn how 
#' the algorithm works under the hood, implement k-means clustering in R, 
#' visualize and interpret the results, and select the number of clusters 
#' when it's not known ahead of time. By the end of the chapter, you'll 
#' have applied k-means clustering to a fun "real-world" dataset!

#' The goal of the unsurpervised learning is to find structure in unlabeled
#' data. Unlabeled data is data without a target, without labeled responses.
#' 
#' There are two major goals: 1. to find homogeneous subgroups within larger
#' group. The process of doing this is referred as clustering. 2. to find
#' patterns in the features of the data. One way to do this is through
#' dimensionality reduction, a method to decrease the number of features to
#' describe an observation or maintining the maximum information content
#' under the constraints of lower dimensionality. 
#' 
#' Dimensionality reduction also allows us to visualize high dimensional
#' data and can serve as a pre-processing before supervised learning.
#' 
#' Unsurpervised learning often does not have one single goal of analysis.
#' And there is much more unlabeled data available than cleanly labeled data,
#' which offers a lot of opportunity to apply unsurpersived learning.

# Introduction to k-means clustering ---------------------------------

#' k-means in R comes with the base R.

# k-means algorithm with 5 centers, run 20 times

kmeans(x, centers = 5, nstart = 20)

# How kmeans() works and practical matters ---------------------------

#' The first step in a k-means algorithm is to randonmly assign each point
#' to any of the clusters. This is the random aspect of the algorithm.
#' The next step is to calculate the centers of each of the subgroups. The
#' centers are the average positions of all the points in each subgroup.
#' Next, each point in the data is assigned to the cluster of the nearer
#' center.
#' 
#' The k-means algorithm stops when there is no change in the assignment 
#' of points to subgroups.
#' 
#' There are others stopping criteria that one can specify to k-means 
#' algorithm (e.g. stopping after some number of iterations of when the 
#' centers move less than some distance).
#' 
#' The k-means in R uses the total within cluster sum of squares as a 
#' measure of the model quality to determine the best outcome after a 
#' couple of runs.
#' 
#' For reproducibility, use set.seed() before running k-means.

# Introduction to the Pokemon data -----------------------------------

#' It is important to consider which features should be used in the 
#' clustering exercise. Sometimes trying different set of features is
#' an important step to find patterns in the data.
#' 
#' If the features being used in modelling are of different units or
#' scales, scaling the data to a common measurement is often completed in
#' order to improve the insights gained from unsupervised learning.
#' 
#' The first challenge with the Pokemon data is that there is no 
#' pre-determined number of clusters.
#' 
#' Use your judgement on making the determination of the number of clusters.
#' 
#' The second part of this exercise includes plotting the outcomes of the
#' clustering on two dimensions, or features, of the data. 

# Practice!

pokemon <- read_csv("./cursos/machine-learning-fundamentals/unsupervised-learning in-r/Pokemon.csv")

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
      # Fit the model: km.out
      km.out <- kmeans(na.omit(pokemon[, 5:11]), centers = i, nstart = 20, iter.max = 50)
      # Save the within cluster sum of squares
      wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 3

# Build model with k clusters: km.out
km.out <- kmeans(na.omit(pokemon[, 5:11]), centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

pokemon %>% 
      ggplot(aes(Defense, Speed)) +
      geom_point(aes(colour = as.character(km.out$cluster), 
                     shape = as.character(km.out$cluster))) +
      ggthemes::theme_gdocs() +
      scale_colour_brewer(palette = "Set1",
                          labels = c("Group 1", "Group 2", "Group 3")) +
      # scale_colour_manual(labels = c("Group 1", "Group 2", "Group 3"),
      #                     values = c("1", "2", "3")) +
      scale_shape_manual(labels = c("Group 1", "Group 2", "Group 3"),
                         values = c(16, 17, 15)) + 
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "horizontal")

