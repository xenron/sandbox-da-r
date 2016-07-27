library(multicore)
library(snow)
library(rsprng)

nw <- 3
seed <- 7777442
kind <- 0
para <- 0

f1 <- parallel({initSprngNode(0, nw, seed, kind, para); rnorm(1)})
f2 <- parallel({initSprngNode(1, nw, seed, kind, para); rnorm(1)})
f3 <- parallel({initSprngNode(2, nw, seed, kind, para); rnorm(1)})
results <- collect(list(f1, f2, f3))
print(unlist(results, use.names=FALSE))

cl <- makeCluster(3, type="SOCK")
seed <- 7777442
clusterSetupSPRNG(cl, seed=seed)
results <- clusterEvalQ(cl, rnorm(1))
print(unlist(results, use.names=FALSE))
stopCluster(cl)
