#! /usr/bin/env Rscript

library(Rhipe)

source.data.file <- "/tmp/small-sample-tweets.dat.bz2"
output.folder <- "/tmp/rhipe-out"

setup.block <- list(
	map=expression({
		## uncomment and modify the following lines if you're using
		## packages from someplace other than the global install path

		## home.Rlib <- "/path/to/your/other/R/libs"
		## invisible( .libPaths( c( home.Rlib , .libPaths() ) ) )
		## library(RJSONIO)
	}) ,
	reduce=expression({ })
)

config.block <- list(
	mapreduce.job.reduces=0 ,
	mapred.task.timeout=600000
)

map.block <- expression({

	rhcounter( "map_stage" , "enter_block" , 1 )

	map.function <- function( tweet.raw ){
	print( .libPaths() )
		tryCatch({
			tweet <- fromJSON( tweet.raw )
			chars.in.tweet <- nchar( tweet$text )
			rhcollect( tweet$user$screen_name , chars.in.tweet )
			rhcounter( "map_stage" , "success" , 1 )
		} ,
		error=function( error ){
			rhcounter( "map_stage" , "error" , 1 )
			print( error )
		})
	}

	lapply( map.values , map.function )
})

reduce.block <- expression(
	pre = {
		tweetCount <- 0
		tweetLength <- 0
		currentKey <- reduce.key
		rhcounter( "reduce_stage" , "pre" , 1 )
	} ,
	reduce = {
		tweetCount <- tweetCount + length( reduce.values )
		tweetLength <- tweetLength + sum( unlist( reduce.values ) )
		rhcounter( "reduce_stage" , "reduce" , 1 )
	} ,
	post = {
		mean.length <- as.integer( round(tweetLength/tweetCount) )
		rhcollect( currentKey , mean.length )
		rhcounter( "reduce_stage" , "post" , 1 )
	}
)

rhinit(TRUE,TRUE,buglevel=2000)

options.block <- rhoptions()
## you'll need this if the R package is installed in a different directory
## on the cluster than on your local workstation
## options.block$runner[1] <- "/usr/local/lib/R/site-library/Rhipe/libs/imperious.so"

rhipe.job.def <- rhmr(
	jobname="rhipe tweet test" ,

	setup=setup.block ,
	map=map.block ,
	reduce=reduce.block ,

	opts=options.block ,

	mapred=config.block ,

	ifolder=source.data.file ,
	ofolder=output.folder ,
	inout=c( "text" , "text" )
)

rhipe.job.result <- rhex( rhipe.job.def )

## this will be a matrix of one column. Each row is a key/value
## pair output from the Reduce step, in the format:
##    {key}\t{value}\r
## As an alternative, one could fetch the file using rhget()
## and read it in using the usual read.table().  If there's 
## more than one file, though, it'll be up to you to first 
## concatenate them and then read them into R.

output.data <- rhread( paste( output.folder , "/part-*" , sep="" ) , type="text" )

## strip the trailing line-feed from the output
output.data <- gsub( "\r$" , "" , output.data )

library(plyr)

mdply(
	output.data ,
	function( line ){
		tuple <- unlist( strsplit( line , "\t" ) )
		return( data.frame( tname=tuple[1] , tcount=tuple[2] ))
	},
	.expand=FALSE
)
