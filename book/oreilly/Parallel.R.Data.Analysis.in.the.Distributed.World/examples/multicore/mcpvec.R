library(multicore)

x <- 1:10
results <- pvec(x, '^', 1/3)
print(results)
