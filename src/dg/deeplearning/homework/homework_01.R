x1 <- c(1, 1, 1, 1, 0, 0, 0, 0)
x2 <- c(0, 0, 1, 1, 0, 1, 1, 0)
x3 <- c(0, 1, 0, 1, 1, 0, 1, 0)
target <- c(-1, 1, 1, 1, -1, -1, 1, -1)
data <- data.frame(x1, x2, x3, target)

library(nnet)
# help(package="nnet")

r <- 1/max(abs(data[,1:3]))
model <- nnet(target~., linout=F, data=data, size=1, rang=r, decay=0.001, maxit=200)
model
predict(model, data[,1:3])

