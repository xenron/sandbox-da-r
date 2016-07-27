library(parallel)

RNGkind("L'Ecuyer-CMRG")

mclapply(1:2, function(i) rnorm(1))

set.seed(7777442)
mc.reset.stream()
results <- mclapply(1:2, function(i) rnorm(1))
print(unlist(results))

set.seed(7777442)
mc.reset.stream()
results <- mclapply(1:2, function(i) rnorm(1))
print(unlist(results))

cl <- makeCluster(4, type="FORK")
clusterSetRNGStream(cl, 7777442)
unlist(clusterEvalQ(cl, rnorm(1)))
clusterSetRNGStream(cl, 7777442)
results <- clusterEvalQ(cl, rnorm(1))
print(unlist(results))
stopCluster(cl)
