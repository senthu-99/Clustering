
# ASSESSED EXERCISE:
# Using the Weighted Kappa function, WK_R.r, in blackboard, explore how the hierarchical clustering compares to k means for the seeds dataset "seeds_dataset.csv" given the correct clusters in "seeds_real.csv".
# You need to:
# • Calculate the WK for K means clustering with different values of K when compared to the correct clusters
# • Also compare Hierarchical to the correct clusters with different linkage measures: single, complete and average
# • Use scatterplots to illustrate the different clustering assignments.
# • Plot the dendrograms where necessary.
# • Plot the different Weighted Kappa values on an appropriate graph.


# Load datasets in
mydata = read.csv('C:/Users/senth/Documents/RDatasets/seeds_dataset.csv', sep=",")
seeds_real = read.csv('C:/Users/senth/Documents/RDatasets/seeds_real.csv', sep=",")

# Pre-processing to prepare data
# 1. Deletion of missing data
# 2. Standardize Variables - make sure data is in a common format
mydata = na.omit(mydata)
mydata = scale(mydata)
seeds_real = na.omit(seeds_real)
seeds_real = scale(seeds_real)


# Load in code that calculates Weighted Kappa
source("C:/Users/senth/Documents/RDatasets/WK_R.r")


#-----------------------------------------K-Means Clustering---------------------------------------------

# Performs K-means clustering - place 2-6 cluster points(represents centroids) in to the feature space
fit <- kmeans(mydata, 3)

# Retrieves the clusters
Kgroups = fit$cluster

# Plot scatter chart
plot(mydata, col=Kgroups)

# Calculates Weighted Kappa
wk = WK_R(Kgroups, seeds_real)
print(wk)

# Weighted Kappa Values:
# K = 2       WK = 0.4823692
# K = 3       WK = 0.7188752
# K = 4       WK = 0.6373057
# K = 5       WK = 0.5794173
# K = 6       WK = 0.5184716

# Plot scatter chart for Weighted Kappa values 
x <- c(2, 3, 4, 5, 6)
y <- c(0.4823692, 0.7188752, 0.6373057, 0.5794173, 0.5184716)

plot(x,y,main = "Weighted Kappa using K-Means Clustering", xlab = "Clusters(K)", ylab = "Weighted Kappa(WK)", col="red" )


#-----------------------------------------Hierarchical Clustering---------------------------------------------

# Generate a matrix of Euclidean distances between each data point - using euclidean distance metric
d <- dist(mydata, method = "euclidean")

# Perform hierarchical clustering( using average link clustering)
fit_average <- hclust(d, method="average")

# Plot Dendrogram
plot(fit_average)

# Create 3 clusters by cutting the Dendrogram
Hgroups <- cutree(fit_average, k=3)

# Draw red borders around the clusters on the Dendrogram
rect.hclust(fit_average, k=3, border="red")

# Plot scatter chart with the assigned colours as clusters
plot(mydata, col=Hgroups)

# Calculates Weighted Kappa
wk_average = WK_R(Hgroups, seeds_real)
print(wk_average) # 0.7360565



# Perform hierarchical clustering( using single link clustering)
fit_single <- hclust(d, method="single")

# Plot Dendrogram
plot(fit_single)

# Create 3 clusters by cutting the Dendrogram
Hgroups <- cutree(fit_single, k=3)

# Draw red borders around the clusters on the Dendrogram
rect.hclust(fit_single, k=3, border="red")

# Plot scatter chart with the assigned colours as clusters
plot(mydata, col=Hgroups)

# Calculates Weighted Kappa
wk_single = WK_R(Hgroups, seeds_real)
print(wk_single) # 0.002856584



# Perform hierarchical clustering( using complete link clustering)
fit_complete <- hclust(d, method="complete")

# Plot Dendrogram
plot(fit_complete)

# Create 3 clusters by cutting the Dendrogram
Hgroups <- cutree(fit_complete, k=3)

# Draw red borders around the clusters on the Dendrogram
rect.hclust(fit_complete, k=3, border="red")

# Plot scatter chart with the assigned colours as clusters
plot(mydata, col=Hgroups)

# Calculates Weighted Kappa
wk_complete = WK_R(Hgroups, seeds_real)
print(wk_complete) # 0.5501314


# Plot bar chart for Weighted Kappa values 
x <- c("Average", "Single", "Complete")
y <- c(0.7360565, 0.002856584, 0.5501314)

barplot(y, names.arg = x,main = "Weighted Kappa using Hierarchical Clustering",xlab = "Linkage", ylab = "Weighted Kappa(WK)", density = 10, col = "red")



