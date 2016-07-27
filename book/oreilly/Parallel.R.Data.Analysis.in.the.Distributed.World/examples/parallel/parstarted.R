library(parallel)
library(MASS)  # needed to load the Boston data frame

# Parallel kmeans using the "multicore-derived" API with parallel RNG
RNGkind("L'Ecuyer-CMRG")
mc.cores <- detectCores()
results <- mclapply(rep(25, 4),
                    function(nstart) kmeans(Boston, 4, nstart=nstart),
                    mc.cores=mc.cores)
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]
print(result)

# Parallel kmeans using the "snow-derived" API with parallel RNG
cl <- makeCluster(detectCores())
clusterSetRNGStream(cl)
clusterEvalQ(cl, library(MASS))
results <- clusterApply(cl, rep(25, 4),
    function(nstart) kmeans(Boston, 4, nstart=nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]
print(result)
stopCluster(cl)
