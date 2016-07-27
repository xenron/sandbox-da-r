library(fOptions)

cost_simulation <- function(S0, mu, sigma, rf, K, Time, dt, periods, relative_cost, plots = F){

t <- seq(0, Time, by = dt)
N <- length(t)
W <- c(0,cumsum(rnorm(N-1)))
S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
S[1] <- S0
SN <- S[N]

delta <- mapply(GBSGreeks, S = S[1:(N-1)], Time = (Time-t)[1:(N-1)], Selection = "Delta", TypeFlag = "c", X = K, r = rf, b = rf, sigma = sigma)

S <- S[seq(1, N-1, by = periods)]
delta <- delta[seq(1, N-1, by = periods)]
m <- length(S)

share_cost <- rep(0,m)
interest_cost <- rep(0,m)
total_cost <- rep(0, m)

share_cost[1] <- S[1]*delta[1] * (1 + relative_cost)
interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
total_cost[1] <- share_cost[1] + interest_cost[1]

for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i] + abs( delta[i] - delta[i-1] ) * (1 + relative_cost)
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
             }

c <- max( SN - K , 0)

cost <- c - delta[m]*SN + total_cost[m]                         

#call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price

return(cost*exp(-Time*rf))

}
#######################################################################################################################################

n_sim = 1000
treshold <- 12


cost_Sim <- function(cost = 0.01, n = n_sim, per = 1){a <- replicate(n, cost_simulation(100, .20, .30,.05, 100, 0.5, 1/1000,per,cost)); 
l <- list(mean(a), sd(a), quantile(a,0.95))}

A <- sapply(seq(1,80,by =1) ,function(per) {print(per); set.seed(2019759); cost_Sim(per = per)})
e <- unlist(A[1,])
s <- unlist(A[2,])
q <- unlist(A[3,])
u <- e+s^2
A <- cbind(t(A), u)
print(A)
z1 <- which.min(e)
z2 <- which.min(s)
z3 <- which.min(u)
print(paste("E min =", z1, "cost of hedge = ",e[z1]," sd = ", s[z1]))
print(paste("s min =", z2, "cost of hedge = ",e[z2]," sd = ", s[z2]))
print(paste("U min =", z3, "u = ",u[z3],"cost of hedge = ",e[z3]," sd = ", s[z3]))
matplot(A, type = "l", lty = 1, xlab = "?t")

abline( v = c(z1,z2,z3), lty = 2, col = "grey")
abline( h = treshold, lty = 1, col = 6)
text(c(z1,z1,z2,z2,z3,z3,z3),c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),round(c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),3), pos = 3, cex = 0.7)

e2 <- e
e2[q > treshold] <- max(e)
z4 <- which.min(e2)
z5 <- which.min(q)

if( q[z5] < treshold ){
    print(paste(" min VaR = ", q[z4], "at", z4 ,"E(cost | VaR < treshold = " ,e[z4], " s = ", s[z4]))
                    } else {
    print(paste("optimization failed, min VaR = ", q[z5], "at", z5 , "where cost = ", e[z5], " s = ", s[z5])) 
                           }


