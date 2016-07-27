install.packages("fOptions")
library(fOptions)

cost_simulation = function(S0, mu, sigma, rf, K, Time,  dt, plots = F){

t <- seq(0, Time, by = dt)
N <- length(t)

S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*cumsum(rnorm(N)))
S[1] = S0
SN=S[N]

delta <- rep(0, N-1)
call_ <- rep(0, N-1)

for(i in 1:(N-1) ){
delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}


share_cost <- rep(0,N-1)
interest_cost <- rep(0,N-1)
total_cost <- rep(0, N-1)

share_cost[1] <- S[1]*delta[1]
interest_cost[1] <- (exp(rf*dt)-1) * share_cost[1]
total_cost[1] <- share_cost[1] + interest_cost[1]

for(i in 2:(N-1)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i]
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
             }

c = max( S[N] - K , 0)

cost = c - delta[N-1]*S[N] + total_cost[N-1]			 

return(cost*exp(-Time*rf))

}

call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
A = rep(0, 1000)
for (i in 1:1000){A[i] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/52)}
windows()
hist(A, freq = F, main = paste("E = ",round(mean(A), 4) ,"  sd = ",round(sd(A), 4)," c=", call_price, sep = ""), xlim = c(6,14), ylim = c(0,0.7))
curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")

for (i in 1:1000){A[i] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/250)}
windows()
hist(A, freq = F, main = paste("E = ",round(mean(A), 4) ,"  sd = ",round(sd(A), 4)," c=", call_price, sep = ""), xlim = c(6,14), ylim = c(0,0.7))
curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")

