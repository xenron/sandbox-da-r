library(fOptions)

cost_simulation = function(S0, mu, sigma, rf, K, Time, dt, periods, plots = F){

t <- seq(0, Time, by = dt)
N <- length(t)
W = c(0,cumsum(rnorm(N-1)))
S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
S[1] = S0
SN = S[N]

delta <- rep(0, N-1)
call_ <- rep(0, N-1)

for(i in 1:(N-1) ){
delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price
                   }

S = S[seq(1, N-1, by = periods)]
delta = delta[seq(1, N-1, by = periods)]

m = length(S)

share_cost <- rep(0,m)
interest_cost <- rep(0,m)
total_cost <- rep(0, m)

share_cost[1] <- S[1]*delta[1]
interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
total_cost[1] <- share_cost[1] + interest_cost[1]


for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i]
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
             }

c = max( SN - K , 0)

cost = c - delta[m]*SN + total_cost[m]                         

return(cost*exp(-Time*rf))

}
###############################################################################################################

r = 0
per = c(2,4,8,20,40,80) 
call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2 weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "s", "ratio")
for (j in per){
r = r+1
A = rep(0, 1000)
set.seed(10125987)
for (i in 1:1000){A[i] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/1000,j)}
E = mean(A)
s = sd(A)
results[r, 1] = E
results[r, 2] = E-1.96*s/sqrt(1000)
results[r, 3] = E+1.96*s/sqrt(1000)
results[r, 4] = s
results[r, 5] = s/call_price
windows()
hist(A, freq = F, main = paste("E = ",round(E, 4) ,"  sd = ",round(s, 4)))
curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
}
print(results)
