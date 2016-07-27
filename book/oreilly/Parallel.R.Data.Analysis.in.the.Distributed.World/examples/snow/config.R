# WARNING: You will have to change the host and user names
# in order to successfully run this example.

library(snow)

tryCatch({
  workerList <- list(list(host='n1'),
                     list(host='n2', user='steve'))
  cl <- makeSOCKcluster(workerList)
  results <- clusterEvalQ(cl, Sys.info()[['user']])
  print(results)
  stopCluster(cl)

  workerList <- list(list(host='n1', outfile='n1.log', user='weston'),
                     list(host='n2', outfile='n2-1.log'),
                     list(host='n2', outfile='n2-2.log'))
  cl <- makeSOCKcluster(workerList, user='steve')
  results <- clusterEvalQ(cl, Sys.glob('*.log'))
  print(results)
  stopCluster(cl)
},
error=function(e) {
  print(conditionMessage(e))
  cat("Did you change the host and user names in this script for your system?",
      file=stderr())
})
