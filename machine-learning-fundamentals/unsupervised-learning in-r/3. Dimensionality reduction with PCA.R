#' Principal component analysis, or PCA, is a common approach to 
#' dimensionality reduction. Learn exactly what PCA does, visualize the 
#' results of PCA with biplots and scree plots, and deal with practical 
#' issues such as centering and scaling the data before performing PCA.

#' PCA has three goals: (1) find linear combination of variables to create
#' principal components. Linear combination means that you take some
#' fraction of some or all the features and add them together. (2) In those
#' new features, PCA will maintain as much variance as it can from the
#' original data for a giving number of principal components. (3) Principal
#' components are uncorrelated to each other.

# Introduction to PCA ------------------------------------------------

# Perform scaled PCA: pr.out
pr.out <- prcomp(pokemon[, 5:11], scale = TRUE) 

# Inspect model output
summary(pr.out)

# Visualizing and interpreting PCA results ---------------------------




# Practical issues with PCA ------------------------------------------

# Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling <- prcomp(pokemon, scale = TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling <- prcomp(pokemon, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)


# Additional uses of PCA and wrap-up ---------------------------------


