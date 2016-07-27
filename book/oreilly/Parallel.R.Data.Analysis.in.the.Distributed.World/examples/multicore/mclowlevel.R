library(multicore)

# Simplified version of mclapply that adds an "mc.init" argument
mclapply.init <- function(X, FUN, ..., mc.cores=4, mc.init=NULL) {
  cores <- max(min(mc.cores, length(X)), 1)
  ix <- lapply(1:cores, function(i) seq(i, length(X), by=cores))
  forkloop <- function(core) {
    proc <- fork()
    if (inherits(proc, "masterProcess")) {
      sendMaster(tryCatch({
        suppressWarnings(rm(".Random.seed", pos=.GlobalEnv))
        if (is.function(mc.init))
          mc.init(core, cores)
        lapply(X[ix[[core]]], FUN, ...)
      },
      error=function(e) {
        lapply(ix[[core]], function(i) e)
      }))
      exit(0)
    }
    proc$pid
  }
  pids <- sapply(1:cores, forkloop)
  results <- vector("list", length(X))
  while (! is.null(ready <- selectChildren(pids, 1))) {
    if (is.integer(ready)) {
      for (pid in ready) {
        data <- readChild(pid)
        if (is.raw(data)) {
          core <- which(pid == pids)
          results[ix[[core]]] <- unserialize(data)
        }
      }
    }
  }
  names(results) <- names(X)
  results
}

# Define an "mc.init" function that defines a worker ID variable
set.worker.id <- function(id, cores) {
  assign(".MC.WORKER.ID", id, pos=.GlobalEnv)
}

# Try out mclapply.init, initializing the workers with "set.worker.id"
mclapply.init(11:13, function(i) c(i, .MC.WORKER.ID),
              mc.cores=2, mc.init=set.worker.id)

# Define an "mc.init" function that sets the RNG seed of the
# workers in the same way as the real mclapply function.
set.worker.seed <- function(id, cores) {
  set.seed(Sys.getpid())
}

# Try out mclapply.init, initializing the workers with "set.worker.seed"
mclapply.init(1:3, function(i) rnorm(1), mc.init=set.worker.seed)
