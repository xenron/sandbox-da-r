# This example demonstrates the perils of using mclapply
# with mc.set.seed=FALSE.

library(multicore)

cat('mc.set.seed=FALSE when .Random.seed is not set:\n')
results <- mclapply(1:3, function(i) rnorm(3), mc.cores=3, mc.set.seed=FALSE)
print(results)

# This sets .Random.seed to a nonrepeatable value in the global environment
ignore <- rnorm(1)

cat('mc.set.seed=FALSE when .Random.seed is set:\n')
results <- mclapply(1:3, function(i) rnorm(3), mc.cores=3, mc.set.seed=FALSE)
print(results)

# This sets .Random.seed to a repeatable value
set.seed(7777442)

cat('mc.set.seed=TRUE when .Random.seed is set:\n')
results <- mclapply(1:3, function(i) rnorm(3), mc.cores=3, mc.set.seed=TRUE)
print(results)
