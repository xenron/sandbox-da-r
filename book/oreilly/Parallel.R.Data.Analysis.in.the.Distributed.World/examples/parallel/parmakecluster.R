library(parallel)

type <- if (exists("mcfork", mode="function")) "FORK" else "PSOCK"
cores <- getOption("mc.cores", detectCores())
cl <- makeCluster(cores, type=type)
results <- parLapply(cl, 1:10, sqrt)
print(results)
stopCluster(cl)
