library(snow)
cl <- makeCluster(4, type="SOCK")

par(ask=TRUE)

# Wrapper for clusterApplyLB that sends the function and
# the "fixed" arguments to the workers only once, rather
# than in every task.  In some cases this can significantly
# reduce communication, thus improving performance.
parLapplyLB <- function(cl, x, fun, ...) {
  clusterCall(cl, LB.init, fun, ...)
  r <- clusterApplyLB(cl, x, LB.worker)
  clusterEvalQ(cl, rm('.LB.fun', '.LB.args', pos=globalenv()))
  r
}

# Worker initialization function called by parLapplyLB
LB.init <- function(fun, ...) {
  assign('.LB.fun', fun, pos=globalenv())
  assign('.LB.args', list(...), pos=globalenv())
  NULL
}

# Worker task function called by parLapplyLB
LB.worker <- function(x) {
  do.call('.LB.fun', c(list(x), .LB.args))
}

bigsleep <- function(sleeptime, mat) Sys.sleep(sleeptime)
bigmatrix <- matrix(0, 2000, 2000)
sleeptime <- rep(1, 100)

# Time parLapplyLB with our contrived example
tm <- snow.time(parLapplyLB(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)
cat(sprintf("Elapsed time for parLapplyLB: %f\n", tm$elapsed))

# Time clusterApplyLB with the same contrived example
tm <- snow.time(clusterApplyLB(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)
cat(sprintf("Elapsed time for clusterApplyLB: %f\n", tm$elapsed))

# Cleanup
stopCluster(cl)
