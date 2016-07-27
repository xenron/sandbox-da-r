library(snow)
cl <- makeSOCKcluster(3)

# Define a function that loads a specified vector of packages
worker.init <- function(packages) {
  for (p in packages) {
    library(p, character.only=TRUE)
  }
  NULL
}

# Load the MASS and boot packages on each of the workers
results <- clusterCall(cl, worker.init, c('MASS', 'boot'))

# Verify that MASS and boot are loaded on all the workers
results <- clusterEvalQ(cl, search())
print(results)

# Assign a unique worker ID to each worker using clusterApply
results <- clusterApply(cl, seq(along=cl), function(id) WORKER.ID <<- id)

# Verify that WORKER.ID is set on each worker
results <- clusterEvalQ(cl, paste("I am worker", WORKER.ID))
print(results)

# Cleanup
stopCluster(cl)
