library(snow)
cl <- makeSOCKcluster(4)

par(ask=TRUE)

set.seed(7777442)
sleeptime <- abs(rnorm(10, 10, 10))
tm <- snow.time(clusterApplyLB(cl, sleeptime, Sys.sleep))
plot(tm)
cat(sprintf("Elapsed time for clusterApplyLB: %f\n", tm$elapsed))

set.seed(7777442)
sleeptime <- abs(rnorm(10, 10, 10))
tm <- snow.time(clusterApply(cl, sleeptime, Sys.sleep))
plot(tm)
cat(sprintf("Elapsed time for clusterApply: %f\n", tm$elapsed))

stopCluster(cl)
