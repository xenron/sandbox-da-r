library(snow)
cl <- makeCluster(4, type="SOCK")

results <- clusterSetupRNG(cl, type='RNGstream', seed=c(1,22,333,444,55,6))
print(unlist(results))

stopCluster(cl)
