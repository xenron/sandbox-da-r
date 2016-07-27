library(snow)
cl <- makeSOCKcluster(4)

results <- clusterSplit(cl, 1:30)
print(results)

parVapply <- function(cl, x, fun, ...) {
  do.call("c", clusterApply(cl, clusterSplit(cl, x), fun, ...))
}
results <- parVapply(cl, 1:10, '^', 1/3)
print(results)

stopCluster(cl)
