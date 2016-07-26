
setwd()

salinity.dat <- read.csv('salinity.csv')

senate <- read.csv('senate.csv')

# PCA Example with toy data set
set.seed(20)
x <- sample(c(0:100), replace = TRUE, 1000)
y <- x + sample(c(-10:10), replace = TRUE, 1000)

plot(y ~ x)

# Do PCA using a Singular Value Decomposition
svd.sample <- svd(matrix(c(x,y), ncol = 2))
manual.rotation <- svd.sample$u %*% -diag(svd.sample$d)
plot(manual.rotation[,1], manual.rotation[,2], xlim = c(0, 150), ylim = c(-75, 75))



# Example of correlation vs covaraince mattering.
wine.eigen.cov <- eigen(cov(red.wine[,-12]))
wine.eigen.cor <- eigen(cor(red.wine[,-12]))
wine.eigen.cov$values / sum(wine.eigen.cov$values)
wine.eigen.cor$values / sum(wine.eigen.cor$values)

summary(red.wine)


#Example of scaled vs unscaled prcomp
wine.prcomp <- prcomp(red.wine[,-12])
wine.prcomp.scaled <- prcomp(red.wine[,-12], scale = TRUE)
summary(wine.prcomp)
summary(wine.prcomp.scaled)

summary(red.wine)



# Red Wine Analysis
library(FactoMineR)
red.wine <- read.csv('winequality-red.csv')
cor(red.wine)
wine.pca <- PCA(red.wine, quanti.sup = 12)
summary(wine.pca)

plot(wine.pca$eig$eigenvalue, type = 'l', xlab = 'Principal Component', ylab = 'Eigenvalue', main = 'Eigenvalues of Principal Components')

#Abalone PCA comparison
plot(abalone.pca$eig$eigenvalue, type = 'l', xlab = 'Principal Component', ylab = 'Eigenvalue', main = 'Eigenvalues of Principal Components')

summary(wine.pca, ncp = 3, )


#PCA of physical functioning data

phys.func <- read.csv('phys_func.csv')[,c(-1)]
phys.func.pca <- PCA(phys.func)
summary(phys.func.pca)
plot(phys.func.pca$eig$eigenvalue, type = 'b', xlab = 'Principal Component', ylab = 'Eigenvalue', main = 'Eigenvalues of Principal Components')
dimdesc(phys.func.pca)

phys.func.cos <- phys.func.pca$var$cos2
phys.func.cos[ phys.func.cos < 0.2 ] <- 0
 
 
# Demonstration of PCA using simulated random data
simulated.data <- matrix(sample(1:100, 20000, replace = TRUE), ncol = 20)
summary(PCA(simulated.data))

#Factor analysis by hand of a unidimensional construct
le.matrix <- as.matrix(phys.func[,c(2,3,4,8,9,10,13,14)])
le.cor <- cor(le.matrix)
le.cor.reduced <- le.cor

row.sums <- le.cor.reduced %*% matrix(rep(1, 8), nrow = 8)
total.sum <- sum(row.sums)
sqrt.total <- sqrt(total.sum)

row.sums / sqrt.total



# Principal Axis Factoring
phys.cor <- cor(phys.func)

reduce.cor.mat <- function(cor.mat) {
	inverted.cor.mat <- solve(cor.mat)
	reduced.cor.mat <- cor.mat
	diag(reduced.cor.mat) <- 1 - (1/diag(inverted.cor.mat))
	
	return(reduced.cor.mat)
}

phys.cor.reduced <- reduce.cor.mat(phys.cor)

paf.method <- function(reduced.matrix, nfactor) {
	row.count <- nrow(reduced.matrix)
	eigen.r <- eigen(reduced.matrix, symmetric = TRUE)
	V <- eigen.r$vectors[,c(1:nfactor)]
	L <- diag(sqrt(eigen.r$values[c(1:nfactor)]), nrow = nfactor)
	
	return((V %*% L))
}

path.coef <- paf.method(phys.cor.reduced, 3)
path.coef[,1] <- -path.coef[,1]

#Rotation
library(GPArotation)
rotated.structure <- oblimin(path.coef)
loading.matrix <- rotated.structure$loadings
print(loading.matrix[ loading.matrix < 0.3] <- 0




#Using the psych package
fit.efa.prep <- polychoric(phys.func, polycor = TRUE)

fit.efa.3 <- fa(fit.efa.prep$rho, nfac = 3, rotate = 'promax')
fit.efa.3

omega(fit.efa.prep$rho, nfac = 3, rotate = 'promax')





