#' Hierarchical clustering is another popular method for clustering. The 
#' goal of this chapter is to go over how it works, how to use it, and how
#' it compares to k-means clustering.

#' Hierarchical clustering does not assume the number of clusters ahead
#' of time. 
#' 
#' This course focuses on bottom-up clustering.
#' 
#' Performing hierarchical clustering in R requires only one parameter:
#' the distance between observations. There are many ways to calculate
#' this distance. For this class, we are using the standard euclidean
#' distance.

# Introduction to hierarchical clustering ----------------------------





# Selecting number of clusters ---------------------------------------





# Clustering linkage and practical matters ---------------------------

#' The clustering algorithm needs rules about how to measure the distance
#' between clusters. There are four methods available in R to measure the 
#' distance or similarity between clusters: (1) Complete method; (2) Single
#' method; (3) Average method; (4) Centroid.
#' 
#' In practice, the choice of linkages is based on insights provided by
#' the distance methods. As a rule of thumb, complete and average tend to
#' produce more balanced trees and are the most commonly used.
#' 
#' Data on different scales can cause undesirable results in clustering
#' methods. The solution is to scale data so that features have same mean
#' and standard deviation.
#' 
#' Whether you want balanced or unbalanced trees for your hierarchical 
#' clustering model depends on the context of the problem you're trying 
#' to solve. Balanced trees are essential if you want an even number of 
#' observations assigned to each cluster. On the other hand, if you want 
#' to detect outliers, for example, an unbalanced tree is more desirable 
#' because pruning an unbalanced tree can result in most observations 
#' assigned to one cluster and only a few observations assigned to other 
#' clusters.

# Practice!

# View column means
colMeans(pokemon[, 5:11])

# View column standard deviations
apply(pokemon[, 5:11], 2, sd)

# Scale the data
pokemon.scaled <- scale(na.omit(pokemon[, 5:11]))

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon <- hclust(dist(pokemon.scaled), method = "complete")
plot(hclust.pokemon)

#' Comparing k-means and hierarchical clustering, you'll see the two 
#' methods produce different cluster memberships. This is because the two
#' algorithms make different assumptions about how the data is generated. 
#' In a more advanced course, we could choose to use one model over 
#' another based on the quality of the models' assumptions, but for now, 
#' it's enough to observe that they are different.

# Apply cutree() to hclust.pokemon: cut.pokemon
cut.pokemon <- cutree(hclust.pokemon, k = 3)

# Compare methods
table(km.out$cluster, cut.pokemon)

#' Looking at the table, it looks like the hierarchical clustering model 
#' assigns most of the observations to cluster 1, while the k-means 
#' algorithm distributes the observations relatively evenly among all 
#' clusters. It's important to note that there's no consensus on which
#' method produces better clusters. The job of the analyst in unsupervised 
#' clustering is to observe the cluster assignments and make a judgment 
#' call as to which method provides more insights into the data.

# Review of hierarchical clustering ----------------------------------





