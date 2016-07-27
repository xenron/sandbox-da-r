
install.packages("fOptions")
library(fOptions)

Price_simulation = function(S0, mu, sigma, rf, K, Time,  dt, plots = F){

t <- seq(0, Time, by = dt)
N <- length(t)

S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*cumsum(rnorm(N)))
S[1] = S0

delta <- rep(0, N-1)
call_ <- rep(0, N-1)

for(i in 1:(N-1) ){
delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}

if(plots){
plot(t, S, type = "l", main = "Price of underlying")
windows()
plot(t[-length(t)], delta, type = "l", main = "Delta")
windows()
plot(t[-length(t)], call_ , type = "l", main = "Price of option")
}
}


Price_simulation(100, 0.2, 0.3, 0.05, 100, 0.5, 1/250, plots = T)

