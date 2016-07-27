library(multicore)

# Make the task time vary a lot to increase the need for load balancing
set.seed(93564990)
sleeptime <- abs(rnorm(10, 10, 10))

cat('mclapply with prescheduling:\n')
tm <- system.time(mclapply(sleeptime, Sys.sleep, mc.cores=4))
print(tm)

cat('mclapply without prescheduling:\n')
tm <- system.time(mclapply(sleeptime, Sys.sleep, mc.cores=4, mc.preschedule=FALSE))
print(tm)
