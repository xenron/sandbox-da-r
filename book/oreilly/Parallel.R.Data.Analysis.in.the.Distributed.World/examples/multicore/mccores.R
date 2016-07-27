library(multicore)

results <- mclapply(1:100, function(i) Sys.getpid(), mc.cores=2)
print(unique(unlist(results)))

options(cores=3)
results <- mclapply(1:100, function(i) Sys.getpid())
print(unique(unlist(results)))
