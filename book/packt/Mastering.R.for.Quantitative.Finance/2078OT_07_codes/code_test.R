GetGreeks <- function(FUN, arg, epsilon,...) {
    all_args1 <- all_args2 <- list(...)
    all_args1[[arg]] <- as.numeric(all_args1[[arg]] + epsilon)
    all_args2[[arg]] <- as.numeric(all_args2[[arg]] - epsilon)
    (do.call(FUN, all_args1) -
        do.call(FUN, all_args2)) / (2 * epsilon)
}

Gamma <- function(FUN, epsilon, S, ...) {
    arg1 <- list(S, ...)
    arg2 <- list(S + 2 * epsilon, ...)
    arg3 <- list(S - 2 * epsilon, ...)
    y1 <- (do.call(FUN, arg2) - do.call(FUN, arg1)) / (2 * epsilon)
    y2 <- (do.call(FUN, arg1) - do.call(FUN, arg3)) / (2 * epsilon)
    (y1 - y2) / (2 * epsilon)
}



x = seq(0.9202, 0.9598, length = 200)
delta <- vega <- theta <- gamma <- rep(0, 200)

for(i in 1:200){
  delta[i] <- GetGreeks(FUN = dnt1, arg = 1, epsilon = 0.0001, 
    x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.02, -0.02)
  vega[i]  <-   GetGreeks(FUN = dnt1, arg = 5, epsilon = 0.0005, 
    x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
  theta[i] <- - GetGreeks(FUN = dnt1, arg = 6, epsilon = 1/365, 
    x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
  gamma[i] <- Gamma(FUN = dnt1, epsilon = 0.0001, S = x[i], K = 
    1e6, U = 0.96, L = 0.92, sigma = 0.06, Time = 0.5, r = 0.02, b = -0.02)
}

windows()
plot(x, vega, type = "l", xlab = "S",ylab = "", main = "Vega")
windows()
plot(x, delta, type = "l", xlab = "S",ylab = "", main = "Delta")
windows()
plot(x, gamma, type = "l", xlab = "S",ylab = "", main = "Gamma")
windows()
plot(x, theta, type = "l", xlab = "S",ylab = "", main = "Theta")





