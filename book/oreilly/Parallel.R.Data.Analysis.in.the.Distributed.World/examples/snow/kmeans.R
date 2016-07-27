# Load MASS package to get "Boston" data frame
library(MASS)

#######################################################
# Example use of the kmeans function
result <- kmeans(Boston, 4, nstart=100)
print(result)

#######################################################
# Sequential splitting using lapply function
results <- lapply(rep(25, 4), function(nstart) kmeans(Boston, 4, nstart=nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]
print(result)

#######################################################
# Parallel version, converted from lapply version
library(snow)
cl <- makeSOCKcluster(3)

ignore <- clusterEvalQ(cl, {library(MASS); NULL})
results <- clusterApply(cl, rep(25, 4), function(nstart)
                    kmeans(Boston, 4, nstart=nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]
print(result)

stopCluster(cl)
