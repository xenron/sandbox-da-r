#! /usr/bin/env Rscript

library(segue)


runModel <- function( params ){
	result <- paste( params$x , params$y , params$z , sep="::" )
	return( result )
}


## can also set env vars "AWSACCESSKEY" and "AWSSECRETKEY"
## if you (quite understandably) don't want to save those values
## in a script.

setCredentials( "your access key", "your secret key" , FALSE )

## this will block your R console for a moment.  You'll
## see messages "STARTING - {timestamp}" until your
## cluster has built out.

emr.handle <- createCluster(
	numInstances=6 ,
	ec2KeyName="your ssh key"
)


emr.result <- emrlapply(emr.handle , input.list , runModel, taskTimeout=10 )

## NOTE: if you're using Segue for iterative/interactive work, you don't
## want to call stopCluster() until you're done for the day.
stopCluster(emr.handle)
