##################
# Code Snipped-1
##################

# notice that during split step a negative 5 is used within the code,
# this negative 5 has been used to discard fifth column of the iris data
# that contains "species" information and we do not need that column to calculate mean.

iris.set <- iris[iris$Species=="setosa",-5]
iris.versi <- iris[iris$Species=="versicolor",-5]
iris.virg <- iris[iris$Species=="virginica",-5]

# calculating mean for each piece ( The apply step)
mean.set <- colMeans(iris.set)
mean.versi <- colMeans(iris.versi)
mean.virg <- colMeans(iris.virg)

# combining the output (The combine step)
mean.iris <- rbind(mean.set,mean.versi,mean.virg)

# giving row names so that the output could be easily understood
rownames(mean.iris) <- c("setosa","versicolor","virginica")

##################
# Code Snipped-2
##################

# split-apply-combine using loop
# each iteration represents split
# mean calculation within each iteration represents apply step
# rbind command in each iteration represents combine step

mean.iris.loop <- NULL
for(species in unique(iris$Species))
{
  iris_sub <- iris[iris$Species==species,]
  column_means <- colMeans(iris_sub[,-5])
  mean.iris.loop <- rbind(mean.iris.loop,column_means)
}

# giving row names so that the output could be easily understood
rownames(mean.iris.loop) <- unique(iris$Species)

##################
# Code Snipped-3
##################

mean.iris.loop <- NULL
for(species in unique(iris$Species))
{
  iris_sub <- iris[iris$Species==species,]
  column_means <- colMeans(iris_sub[,-5])
  mean.iris.loop <- rbind(mean.iris.loop,column_means)
}
rownames(mean.iris.loop) <- unique(iris$Species)

mean.iris.loop

#The same mean calculation, but this time using the plyr package:
library(plyr)
ddply(iris,~Species,function(x) colMeans(x[,-
which(colnames(x)=="Species")]))

mean.iris.loop

##################
# Code Snipped-4
##################

# class of iris3 dataset is array
class(iris3)
# dimension of iris3 dataset
dim(iris3)

##################
# Code Snipped-5
##################

# Calculate column mean for each species and output will be data frame
iris_mean <- adply(iris3,3,colMeans)

class(iris_mean)
iris_mean

##################
# Code Snipped-6
##################

# again we will calculate the mean but this time output will be an array
iris_mean <- aaply(iris3,3,colMeans)
class(iris_mean)
iris_mean

# note that here the class is showing "matrix",
# since the output is a two dimensional array which represents matrix

# Now calculate mean again with output as list
iris_mean <- alply(iris3,3,colMeans)
class(iris_mean)
iris_mean

##################
# Code Snipped-7
##################

# converting 3 dimensional array to a 2 dimensional data frame
iris_dat <- adply(iris3, .margins=3)
class(iris_dat)
str(iris_dat)

##################
# Code Snipped-8
##################

# Function to calculate five number summary
fivenum.summary <- function(x)
{
  results <-data.frame(min=apply(x,2,min),
  mean=apply(x,2,mean),
  median=apply(x,2,median),
  max=apply(x,2,max),
  sd=apply(x,2,sd))
  return(results)
}

#To calculate the summaries for the five numbers using a for loop with default R is as shown:
# initialize the output list object
all_stats <- list()

# the for loop will run for each species
for(i in 1:dim(iris3)[3])
{
  sub_data <- iris3[,,i]
  all_stat_species <- fivenum.summary(sub_data)
  all_stats[[i]] <-  all_stat_species
}

# class of the output object
class(all_stats)
all_stats

# Let's calculate the same statistics, but this time using the adply() function from the plyr package:
all_stats <- alply(iris3,3,fivenum.summary)

class(all_stats)
all_stats

##################
# Code Snipped-9
##################

# define parameter set
parameter.dat <- data.frame(n=c(25,50,100,200,400),mean=c(0,2,3.5,2.5,0.1),sd=c(1,1.5,2,5,2))

# displaying parameter set

parameter.dat

# random normal variate generate using base R
# set seed to make the example reproducible
set.seed(12345)

# initialize blank list object to store the generated variable
dat <- list()
for(i in 1:nrow(parameter.dat))
{
 dat[[i]] <- rnorm(n=parameter.dat[i,1],
             mean=parameter.dat[i,2],sd=parameter.dat[i,3])
}

# estimating mean from the newly generated data
estmean <- lapply(dat,mean)
estmean

# Performing same task as above but this time use plyr package

dat_plyr <- mlply(parameter.dat,rnorm)
estmean_plyr <- llply(dat_plyr,mean)
estmean_plyr
