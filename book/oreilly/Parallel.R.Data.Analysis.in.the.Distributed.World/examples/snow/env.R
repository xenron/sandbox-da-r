library(snow)
cl <- makeCluster(4, type="SOCK")

a <- 1:4
x <- rnorm(4)
clusterExport(cl, "x")
mult <- function(s) s * x
parLapply(cl, a, mult)

pmult <- function(cl) {
  a <- 1:4
  x <- rnorm(4)
  mult <- function(s) s * x
  parLapply(cl, a, mult)
}
pmult(cl)

pmult <- function(cl, a, x) {
  x  # force x
  mult <- function(s) s * x
  parLapply(cl, a, mult)
}
scalars <- 1:4
dat <- rnorm(4)
pmult(cl, scalars, dat)

pmult <- function(cl, a, x) {
  mult <- function(s, x) s * x
  environment(mult) <- .GlobalEnv
  parLapply(cl, a, mult, x)
}
scalars <- 1:4
dat <- rnorm(4)
pmult(cl, scalars, dat)

stopCluster(cl)
