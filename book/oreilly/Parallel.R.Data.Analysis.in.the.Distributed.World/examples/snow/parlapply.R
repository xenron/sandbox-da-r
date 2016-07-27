library(snow)
cl <- makeCluster(4, type="SOCK")

par(ask=TRUE)

bigsleep <- function(sleeptime, mat) Sys.sleep(sleeptime)
bigmatrix <- matrix(0, 2000, 2000)
sleeptime <- rep(1, 100)

tm <- snow.time(clusterApply(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)
cat(sprintf("Elapsed time for clusterApply: %f\n", tm$elapsed))

tm <- snow.time(parLapply(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)
cat(sprintf("Elapsed time for parLapply: %f\n", tm$elapsed))

stopCluster(cl)
