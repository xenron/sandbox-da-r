#Chapter 3: Data Manipulation using plyr and dplyr

#######################
#  Code block 1
#######################

#Application of split-apply-combine strategy
# Step 1: Splitting dataset
iris.setosa <- subset(iris,Species=="setosa",          
select=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
iris.versicolor <- subset(iris,Species=="versicolor",          
select=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
iris.virginica <-  subset(iris,Species=="virginica",          
select=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
# Step 2: Applying mean function to calculate mean
setosa <- colMeans(iris.setosa)
versicolor <- colMeans(iris.versicolor)
virginica <-colMeans(iris.virginica)
# Step 3: Combining results
rbind(setosa=setosa,versicolor=versicolor,virginica=virginica)

#######################
#  Code block 2
#######################

# Step 1: Splitting dataset
iris.split<- split(iris,as.factor(iris$Species))
# Step 2: Applying mean function to calculate mean
iris.apply<- lapply(iris.split,function(x)colMeans(x[-5]))
# Step 3: Combining results
iris.combine<- do.call(rbind,iris.apply)

#######################
#  Code block 3
#######################

#Utilities of plyr
library(plyr)
ddply(iris, .(Species), function(x) colMeans(x[-5]))
#Intuitive function names in plyr library
# class of iris3 dataset is array 
class(iris3) 
# dimension of iris3 dataset 
dim(iris3) 
# Calculate column mean for each species and output will be 
# data frame 
iris_mean <- adply(iris3,3,colMeans) 
class(iris_mean) 
iris_mean 

#######################
#  Code block 4
#######################

# again we will calculate the mean but this time output will be an # array 
iris_mean <- aaply(iris3,3,colMeans) 
class(iris_mean) 
iris_mean 
# note that here the class is showing "matrix", 
# since the output is a # two dimensional array which represents 
# matrix. Now calculate mean again with output as list 
iris_mean <- alply(iris3,3,colMeans) 
class(iris_mean) 
iris_mean 

#######################
#  Code block 5
#######################

#Input and arguments
# converting 3 dimensional array to a 2 dimensional data frame 
iris_dat <- adply(iris3, .margins=3) 
class(iris_dat)
str(iris_dat) 

#######################
#  Code block 6
#######################


#Multiargument functions
# define parameter set 
parameter.dat <- data.frame(n=c(25,50,100,200,400),mean=c(0,2,3.5,2.5 ,0.1),sd=c(1,1.5,2,5,2))
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

#######################
#  Code block 7
#######################

#Comparing default R and plyr
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
# initialize the output list object 
all_stats <- list() 
# the for loop will run for each species 
for(i in 1:dim(iris3)[3]) 
{ 
sub_data <- iris3[,,i] 
all_stat_species <- fivenum.summary(sub_data) 
all_stats[[i]] <- all_stat_species 
}
# class of the output object 
class(all_stats) 
all_stats 
#Let's calculate the same statistics, but this time using the alply() function from the plyr package:
all_stats <- alply(iris3,3,fivenum.summary) 
class(all_stats) 
all_stats 


#######################
#  Code block 8
#######################
library(dplyr)

#Filtering and slicing rows
filter(iris,Species=="virginica")
filter(iris,Species=="virginica" &  Sepal.Length<6  & Sepal.Width<=2.7)
slice(iris, 1:10)
slice(iris, 140:150)
slice(iris, 95:105)

#######################
#  Code block 9
#######################

#Arranging rows
arrange(iris, Sepal.Length)
arrange(iris, Sepal.Length, Sepal.Width)
arrange(iris, Sepal.Length, desc(Sepal.Width))

#Selecting and renaming 
select(iris, Species, Sepal.Length, Sepal.Width) 
rename(iris, SL=Sepal.Length, SW= Sepal.Width, PL=Petal.Length, PW= Petal.Width )

#Adding new columns
mutate(iris, SLm=Sepal.Length/100, SWm= Sepal.Width/100, PLm=Petal.Length/100, PWm= Petal.Width/100 ) 
mutate(iris, SLsd=(Sepal.Length-mean(Sepal.Length))/sd(Sepal.Length), 
                      SWsd= (Sepal.Width-mean(Sepal.Width))/sd(Sepal.Width),
                      PLsd=(Petal.Length-mean(Petal.Length))/sd(Petal.Length),
                      PWsd= (Petal.Width-mean(Petal.Width))/sd(Petal.Width) ) 
transmute(iris, SLsd=(Sepal.Length-mean(Sepal.Length))/sd(Sepal.Length), 
                      SWsd= (Sepal.Width-mean(Sepal.Width))/sd(Sepal.Width),
                      PLsd=(Petal.Length-mean(Petal.Length))/sd(Petal.Length),
                      PWsd= (Petal.Width-mean(Petal.Width))/sd(Petal.Width) ) 
#Selecting distinct rows
distinct(iris,Species,Petal.Width)

#Column-wise descriptive statistics
summarise(iris, meanSL=mean(Sepal.Length),
                             meanSW=mean(Sepal.Width), 
                             meanPL=mean(Petal.Length), 
                             meanPW=mean(Petal.Width))

#Group-wise operations
iris.grouped<- group_by(iris, Species)  
summarize(iris.grouped, count=n(), 
                       meanSL= mean(Sepal.Length),
                      meanSW=mean(Sepal.Width), 
                      meanPL=mean(Petal.Length), 
                      meanPW=mean(Petal.Width))

#Chaining
iris
iris.grouped<- group_by(iris, Species)
iris.grouped.selected<- select(iris.grouped, Sepal.Length, Sepal.Width)   
iris.grouped.selected.summarised<- summarise(iris.grouped.selected, 
                      meanSL=mean(Sepal.Length),
                      sdSL=sd(Sepal.Length),
                     meanSW= mean(Sepal.Width),
                     sdSW= sd(Sepal.Width))
filter(iris.grouped.selected.summarised, meanSL==max(meanSL) | meanSW==max(meanSW))
iris %>%
       group_by( Species) %>%
       select(Sepal.Length, Sepal.Width)   %>%
       summarise( meanSL=mean(Sepal.Length),
                             sdSL=sd(Sepal.Length),
                             meanSW= mean(Sepal.Width),
                             sdSW= sd(Sepal.Width)) %>%
       filter(meanSL==max(meanSL) | meanSW==max(meanSW))







