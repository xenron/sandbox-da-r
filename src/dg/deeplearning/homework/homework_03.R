# install.packages('autoencoder')
# install.packages('bmp')
library(autoencoder)

library(bmp)
training.bmp <- read.bmp('d:/tmp/lena.bmp')

nl=3 ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic" ## specify the network unit type, i.e., the unit's

Nx.patch=256 ## width of training image patches, in pixels
Ny.patch=256 ## height of training image patches, in pixels
N.input = Nx.patch*Ny.patch ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 5*5 ## number of units in the hidden layer
lambda = 0.0002 ## weight decay parameter
beta = 6 ## weight of sparsity penalty term
rho = 0.01 ## desired sparsity parameter
epsilon <- 0.001 ## a small parameter for initialization of weights

max.iterations = 2000 ## number of iterations in optimizer

autoencoder.object <- autoencode(X.train=training.bmp,
                                 nl=nl,
                                 N.hidden=N.hidden,
                                 unit.type=unit.type,
                                 lambda=lambda,
                                 beta=beta,
                                 rho=rho,
                                 epsilon=epsilon,
                                 optim.method="BFGS",
                                 max.iterations=max.iterations,
                                 rescale.flag=TRUE,
                                 rescaling.offset=0.001)

cat("autoencode(): mean squared error for training set: ", round(autoencoder.object$mean.error.training.set,3),"\n")

visualize.hidden.units(autoencoder.object,Nx.patch,Ny.patch)

X.output <- predict(autoencoder.object,
                    X.input=training.bmp,
                    hidden.output=FALSE)$X.output

op <- par(no.readonly = TRUE) ## save the whole list of settable par's.
par(mfrow=c(3,2),mar=c(2,2,2,2))
for (n in c(7,26,16)){
  ## input image:
  image(matrix(training.matrix[n,],nrow=Ny.patch,ncol=Nx.patch),axes=FALSE,main="Input image",
  col=gray((0:32)/32))
  ## output image:
  image(matrix(X.output[n,],nrow=Ny.patch,ncol=Nx.patch),axes=FALSE,main="Output image",
  col=gray((0:32)/32))
}
par(op) ## restore plotting par's



